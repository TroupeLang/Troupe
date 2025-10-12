#!/bin/bash
set -euo pipefail

# Troupe Multi-Node Test Runner - Refactored Version
# Orchestrates multi-node tests with proper cleanup and output synchronization
#
# IMPORTANT MAINTENANCE NOTES:
# ============================
# 1. NEGATIVE TESTS: Some tests are designed to fail or timeout (exit code 124).
#    Examples: ring-echo (intermediary node), trust-flow-issue-42 (node1).
#    DO NOT add retry logic or exponential backoffs that would delay these failures.
#
# 2. RELAY LOGIC: The relay must work both with and without the relay server.
#    Tests can specify "use_relay": false in config.json. Any changes to relay
#    handling must preserve this dual-mode functionality.
#
# 3. NO POLLING: Service discovery uses fixed delays, not polling loops.
#    This is by design - polling can mask timing issues and race conditions
#    that tests are meant to detect.
#
# 4. NO UNIVERSAL RETRIES: Network operations should NOT have automatic retries.
#    Retries would interfere with negative tests and tests that verify
#    timeout behavior.
#
# 5. TIMEOUT CONFIGURATIONS: Tests specify their own timeouts in config.json.
#    Some tests have internal sleep commands that must be shorter than the
#    configured timeout. Always verify: internal_sleep < config_timeout - buffer
#
# 6. CI SCALING: In CI environments, timeouts are scaled by 1.2x (20% increase)
#    to account for slower machines. This is a simple multiplier, not retry logic.
#
# 7. EXIT CODE PRESERVATION: The cleanup function MUST preserve the original test
#    exit code. Without 'exit $exit_code' at the end of cleanup, the script would
#    exit with the status of the last cleanup command, causing false test failures.
#
# 8. CLEANUP TIMING: Cleanup operations need CI scaling too. Processes take longer
#    to terminate gracefully in CI. We give relay/nodes 3s base (3.6s in CI) to
#    shutdown cleanly before force-killing to avoid race conditions.
#
# 9. ARITHMETIC SAFETY: Use '|| true' after ((var++)) when set -e is active.
#    In bash, ((0)) returns exit code 1, which terminates the script with set -e.
#
# 10. PORT MANAGEMENT: Tests use specific ports. Cleanup must be thorough to
#    prevent "port already in use" errors, but without retrying binds.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TROUPE_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
TEST_CONFIG=""
CLEANUP_PIDS=()
TEMP_DIR=""
VERBOSE=false
RELAY_MULTIADDR=""
RELAY_PID=""

# Detect CI environment and apply modest scaling
TIMEOUT_SCALE=1.0
if [[ -n "${CI:-}" ]] || [[ -n "${GITHUB_ACTIONS:-}" ]]; then
    TIMEOUT_SCALE=1.2  # 20% more time in CI only
    echo "[INFO] CI environment detected, scaling timeouts by ${TIMEOUT_SCALE}x" >&2
fi

# ============================================================================
# Helper Functions - Common operations extracted to reduce duplication
# ============================================================================

# Check if a PID is valid and the process is running
is_valid_pid() {
    local pid="$1"
    [[ -n "$pid" ]] && [[ "$pid" =~ ^[0-9]+$ ]] && kill -0 "$pid" 2>/dev/null
}

# Kill all processes listening on a specific port (OS-aware)
kill_processes_on_port() {
    local port="$1"
    if command -v lsof >/dev/null 2>&1; then
        if [[ "$(uname)" == "Darwin" ]]; then
            lsof -ti ":$port" | xargs kill -9 2>/dev/null || true
        else
            lsof -ti ":$port" | xargs -r kill -9 2>/dev/null || true
        fi
    fi
}

# Get all ports used by the test (nodes + relay)
get_all_ports() {
    local config_file="$1"
    local ports=($(jq -r '.nodes[].port' "$config_file" 2>/dev/null))
    local relay_port=$(jq -r '.network.relay_port // 5555' "$config_file" 2>/dev/null)
    ports+=("$relay_port")
    echo "${ports[@]}"
}

# Get the number of nodes in the test configuration
get_node_count() {
    local config_file="$1"
    jq -r '.nodes | length' "$config_file"
}

# Display standardized error message for node failures
display_node_error() {
    local node_id="$1"
    local error_type="$2"  # "timeout" or "exit_code"
    local actual="$3"
    local expected="$4"
    local output_file="$5"
    local error_file="$6"
    local timeout_val="${7:-}"  # Optional, only for timeout errors

    if [[ "$error_type" == "timeout" ]]; then
        echo "ERROR: Node $node_id timed out unexpectedly" >&2
        echo "  Timeout: ${actual}s (original: ${timeout_val}s)" >&2
        echo "  Expected exit code: $expected" >&2
    else
        echo "ERROR: Node $node_id exit code mismatch" >&2
        echo "  Actual: $actual" >&2
        echo "  Expected: $expected" >&2
    fi

    echo "  Last 10 lines of output:" >&2
    tail -n 10 "$output_file" 2>/dev/null | sed 's/^/    /' >&2

    if [[ "$error_type" == "exit_code" ]] && [[ -s "$error_file" ]]; then
        echo "  Last 5 lines of errors:" >&2
        tail -n 5 "$error_file" 2>/dev/null | sed 's/^/    /' >&2
    fi
}

usage() {
    cat << EOF
Usage: $0 [options] <test-config.json>

Options:
    -v, --verbose       Enable verbose output
    -h, --help         Show this help message

Arguments:
    test-config.json   Configuration file for the multi-node test

Examples:
    $0 tests/rt/multinode-tests/basic-echo/config.json
    $0 -v tests/rt/multinode-tests/consensus/raft.json
EOF
}

cleanup() {
    # CRITICAL: Capture the original exit code immediately
    # This must be the FIRST command in cleanup to preserve test results
    local exit_code=$?
    log "Cleaning up test processes (exit code: $exit_code)..."

    local cleaned_count=0
    local cleanup_failures=()  # Track cleanup operations that fail

    # Calculate cleanup timeouts with CI scaling
    # In CI environments, processes take longer to terminate gracefully
    local relay_grace_period=3
    local node_grace_period=3
    local socket_release_wait=3

    if [[ -n "${TIMEOUT_SCALE:-}" ]]; then
        # Apply the same scaling factor used for test timeouts to cleanup
        relay_grace_period=$(awk "BEGIN {print int($relay_grace_period * $TIMEOUT_SCALE + 0.5)}")
        node_grace_period=$(awk "BEGIN {print int($node_grace_period * $TIMEOUT_SCALE + 0.5)}")
        socket_release_wait=$(awk "BEGIN {print int($socket_release_wait * $TIMEOUT_SCALE + 0.5)}")
        log "CI environment: using scaled cleanup timeouts (relay=${relay_grace_period}s, nodes=${node_grace_period}s, sockets=${socket_release_wait}s)"
    fi

    # Kill relay first with graceful shutdown attempt
    if is_valid_pid "$RELAY_PID"; then
        log "Stopping relay (PID: $RELAY_PID)"
        if ! kill "$RELAY_PID" 2>/dev/null; then
            cleanup_failures+=("Failed to send SIGTERM to relay PID $RELAY_PID")
        fi

        # Wait for graceful shutdown with timeout
        local wait_count=0
        local relay_terminated=false
        while [[ $wait_count -lt $relay_grace_period ]]; do
            if ! kill -0 "$RELAY_PID" 2>/dev/null; then
                log "Relay terminated gracefully"
                relay_terminated=true
                break
            fi
            sleep 1
            ((wait_count++)) || true
        done

        # Force kill if still running
        if [[ "$relay_terminated" == "false" ]]; then
            if kill -9 "$RELAY_PID" 2>/dev/null; then
                log "Force-killed relay after ${relay_grace_period}s timeout"
                cleanup_failures+=("Relay PID $RELAY_PID required force-kill after ${relay_grace_period}s")
            else
                cleanup_failures+=("Failed to force-kill relay PID $RELAY_PID (may have already exited)")
            fi
        fi
        ((cleaned_count++)) || true
    fi

    # Kill by process name as fallback
    if pkill -f "relay.mjs" 2>/dev/null; then
        ((cleaned_count++)) || true
        cleanup_failures+=("Found relay.mjs processes via pkill (PID tracking may have failed)")
    fi
    if pkill -f "node.*network.sh" 2>/dev/null; then
        ((cleaned_count++)) || true
        cleanup_failures+=("Found network.sh processes via pkill (PID tracking may have failed)")
    fi

    # Kill all spawned node processes
    if [[ -n "${CLEANUP_PIDS[*]:-}" ]]; then
        log "Cleaning up ${#CLEANUP_PIDS[@]} node processes"
        local failed_kills=()
        for pid in "${CLEANUP_PIDS[@]}"; do
            if is_valid_pid "$pid"; then
                if ! kill "$pid" 2>/dev/null; then
                    failed_kills+=("$pid")
                else
                    ((cleaned_count++)) || true
                fi
            fi
        done

        if [[ -n "${failed_kills[*]:-}" ]]; then
            cleanup_failures+=("Failed to send SIGTERM to node PIDs: ${failed_kills[*]}")
        fi

        # Give processes time to clean up gracefully
        sleep "$node_grace_period"

        # Force kill remaining processes
        local force_killed=()
        local still_running=()
        for pid in "${CLEANUP_PIDS[@]}"; do
            if is_valid_pid "$pid"; then
                if kill -9 "$pid" 2>/dev/null; then
                    log "Force-killed remaining process $pid"
                    force_killed+=("$pid")
                else
                    # Process may have exited between check and kill
                    if kill -0 "$pid" 2>/dev/null; then
                        still_running+=("$pid")
                    fi
                fi
            fi
        done

        if [[ -n "${force_killed[*]:-}" ]]; then
            cleanup_failures+=("Node PIDs required force-kill after ${node_grace_period}s: ${force_killed[*]}")
        fi
        if [[ -n "${still_running[*]:-}" ]]; then
            cleanup_failures+=("Failed to kill node PIDs: ${still_running[*]}")
        fi
    fi

    # Clean up ports used by the test
    if [[ -n "$TEST_CONFIG" ]]; then
        local ports=($(get_all_ports "$TEST_CONFIG"))

        for port in "${ports[@]}"; do
            if [[ "$port" =~ ^[0-9]+$ ]]; then
                kill_processes_on_port "$port"
            fi
        done
    fi

    # Final cleanup of any troupe processes
    if pkill -9 -f "node.*troupe" 2>/dev/null; then
        cleanup_failures+=("Found lingering troupe processes requiring pkill -9")
    fi

    # Clean up temporary directory (preserve on failure for debugging)
    if [[ -n "$TEMP_DIR" && -d "$TEMP_DIR" ]]; then
        if [[ $exit_code -ne 0 ]]; then
            echo "Test failed. Preserving logs in: $TEMP_DIR" >&2
            echo "Node outputs:" >&2
            for f in "$TEMP_DIR"/output/*.out "$TEMP_DIR"/output/*.err; do
                if [[ -f "$f" ]]; then
                    echo "=== $f ===" >&2
                    cat "$f" >&2
                    echo "" >&2
                fi
            done
        else
            rm -rf "$TEMP_DIR"
        fi
    fi

    # Wait for OS to release sockets
    # This prevents "port already in use" errors in subsequent tests
    # Critical in CI where socket cleanup can be slower
    log "Cleaned up $cleaned_count processes, waiting ${socket_release_wait}s for socket release..."
    sleep "$socket_release_wait"

    # Report cleanup issues if any occurred
    # These are informational only and don't affect the test result
    # Check if array has any elements before accessing
    if [[ -n "${cleanup_failures[*]:-}" ]]; then
        echo "" >&2
        echo "=== Cleanup Issues Report (non-fatal) ===" >&2
        echo "The following cleanup operations had issues:" >&2
        for failure in "${cleanup_failures[@]}"; do
            echo "  - $failure" >&2
        done
        echo "" >&2
        echo "Note: These cleanup issues did not affect the test result (exit code: $exit_code)" >&2
        echo "They are reported for diagnostic purposes only." >&2
    fi

    # CRITICAL: Exit with the original exit code from the test, not from cleanup
    # Without this, the script would exit with the status of the last cleanup command
    # This is why tests were "failing" in CI even though they succeeded
    exit $exit_code
}

trap cleanup EXIT INT TERM

log() {
    if [[ "$VERBOSE" == "true" ]]; then
        echo "[$(date '+%H:%M:%S')] $*" >&2
    fi
}

error() {
    echo "Error: $*" >&2
    exit 1
}

check_port_available() {
    local port="$1"
    if command -v lsof >/dev/null 2>&1; then
        if lsof -Pi ":$port" -sTCP:LISTEN -t >/dev/null 2>&1; then
            # Port is in use - log which process is using it for diagnostics
            local process_info=$(lsof -Pi ":$port" -sTCP:LISTEN 2>/dev/null | tail -n +2 | head -1)
            log "Port $port is in use by: $process_info"
            return 1  # Port is in use
        fi
    fi
    return 0  # Port is available or lsof not available
}

wait_for_port_release() {
    local port="$1"
    local max_wait=10
    local wait_count=0

    while [[ $wait_count -lt $max_wait ]]; do
        if check_port_available "$port"; then
            log "Port $port is now available"
            return 0
        fi
        log "Port $port still in use, waiting... ($((max_wait - wait_count))s remaining)"
        sleep 1
        ((wait_count++)) || true
    done

    # Force kill processes using the port
    log "Force killing processes on port $port"
    kill_processes_on_port "$port"
    sleep 1
    return 0
}

validate_ports() {
    local config_file="$1"
    log "Validating port availability..."

    local ports=($(get_all_ports "$config_file"))
    
    local port_conflict=false
    for port in "${ports[@]}"; do
        if [[ "$port" =~ ^[0-9]+$ ]]; then
            if ! check_port_available "$port"; then
                echo "Warning: Port $port is in use, attempting cleanup..." >&2
                wait_for_port_release "$port"
                if ! check_port_available "$port"; then
                    echo "Error: Unable to free port $port" >&2
                    port_conflict=true
                fi
            fi
        fi
    done
    
    if [[ "$port_conflict" == "true" ]]; then
        error "Unable to free required ports. Please check for lingering processes."
    fi
    
    log "All required ports are available"
}

parse_config() {
    local config_file="$1"
    
    if [[ ! -f "$config_file" ]]; then
        error "Configuration file '$config_file' not found"
    fi
    
    # Validate JSON format
    if ! jq empty "$config_file" 2>/dev/null; then
        error "Invalid JSON in configuration file"
    fi
    
    TEST_CONFIG="$config_file"
}

setup_network_identities() {
    local test_dir="$1"
    local config_file="$2"
    
    log "Setting up network identities..."
    
    # Create identities directory
    mkdir -p "$test_dir/ids"
    
    # Generate node identities
    local node_count
    node_count=$(get_node_count "$config_file")
    
    for ((i=0; i<node_count; i++)); do
        local node_id
        node_id=$(jq -r ".nodes[$i].id" "$config_file")
        local id_file="$test_dir/ids/$node_id.json"
        
        if [[ ! -f "$id_file" ]]; then
            log "Generating identity for node $node_id"
            if [[ ! -f "$TROUPE_ROOT/p2p-tools/built/mkid.mjs" ]]; then
                error "mkid.mjs not found. Run 'make p2p-tools' first."
            fi
            if ! node "$TROUPE_ROOT/p2p-tools/built/mkid.mjs" --outfile="$id_file"; then
                error "Failed to generate identity for node $node_id"
            fi
        fi
    done
    
    # Generate aliases file
    local aliases_file="$test_dir/aliases.json"
    log "Generating aliases file"
    if [[ ! -f "$TROUPE_ROOT/p2p-tools/built/mkaliases.js" ]]; then
        error "mkaliases.js not found. Run 'make p2p-tools' first."
    fi
    if ! node "$TROUPE_ROOT/p2p-tools/built/mkaliases.js" \
        --include "$test_dir/ids"/*.json \
        --outfile "$aliases_file"; then
        error "Failed to generate aliases file"
    fi
}

ensure_relay_built() {
    local relay_dir="$TROUPE_ROOT/p2p-tools/relay"
    local relay_built="$relay_dir/relay.mjs"
    
    # Check if built file exists
    if [[ -f "$relay_built" ]]; then
        log "Relay is already built"
        return 0
    fi
    
    log "Building relay server..."
    
    # Build the relay
    cd "$relay_dir"
    echo "Building relay in $relay_dir..."
    if ! make build/relay 2>&1; then
        echo "ERROR: Relay build failed. Make sure TypeScript is installed globally." >&2
        echo "Try: npm install -g typescript" >&2
        error "Failed to build relay server"
    fi
    
    cd "$TROUPE_ROOT"
    
    # Verify the build succeeded
    if [[ ! -f "$relay_built" ]]; then
        error "Relay build completed but output file not found"
    fi
    
    log "Relay server built successfully"
}

start_relay() {
    local config_file="$1"
    local use_relay
    use_relay=$(jq -r '.network.use_relay // true' "$config_file")
    
    if [[ "$use_relay" != "true" ]]; then
        log "Relay disabled by configuration"
        return 0
    fi
    
    # Check if relay address is provided in config
    local relay_address
    relay_address=$(jq -r '.network.relay_address // ""' "$config_file")
    
    if [[ -n "$relay_address" ]]; then
        # Use provided relay address
        RELAY_MULTIADDR="$relay_address"
        log "Using configured relay address: $RELAY_MULTIADDR"
        return 0
    fi
    
    # Start local relay
    ensure_relay_built
    
    # Generate relay keys in temp directory
    local relay_keys_dir="$TEMP_DIR/relay-keys"
    mkdir -p "$relay_keys_dir"
    
    log "Generating temporary relay keys..."
    if ! node "$TROUPE_ROOT/p2p-tools/built/mkid.mjs" \
        --privkeyfile="$relay_keys_dir/relay.priv" \
        --idfile="$relay_keys_dir/relay.id" \
         >&2; then
        error "Failed to generate relay keys"
    fi
    
    local relay_port
    relay_port=$(jq -r '.network.relay_port // 5555' "$config_file")
    
    log "Starting relay server on port $relay_port"
    
    # Check if relay port is available first
    if ! check_port_available "$relay_port"; then
        log "Relay port $relay_port is in use, attempting cleanup..."
        wait_for_port_release "$relay_port"
    fi
    
    # Create a temporary file for relay output
    local relay_output="$TEMP_DIR/relay.out"
    touch "$relay_output"  # Ensure file exists before grep
    
    # Verify relay executable exists
    if [[ ! -f "$TROUPE_ROOT/p2p-tools/relay/relay.mjs" ]]; then
        echo "ERROR: Relay executable not found at $TROUPE_ROOT/p2p-tools/relay/relay.mjs" >&2
        echo "The relay needs to be built. This should have been done by ensure_relay_built()" >&2
        error "Relay executable missing"
    fi
    
    # Verify node is available
    if ! which node >/dev/null 2>&1; then
        error "Node.js not found in PATH"
    fi
    
    # Start relay in background
    echo "Starting relay with command:" >&2
    echo "DEBUG=libp2p:circuit-relay:server node $TROUPE_ROOT/p2p-tools/relay/relay.mjs --port=$relay_port --id-file=$relay_keys_dir/relay.id --priv-file=$relay_keys_dir/relay.priv" >&2
    
    # Temporarily disable exit on error for relay startup
    set +e
    
    DEBUG=libp2p:circuit-relay:server node "$TROUPE_ROOT/p2p-tools/relay/relay.mjs" \
        --port="$relay_port" \
        --id-file="$relay_keys_dir/relay.id" \
        --priv-file="$relay_keys_dir/relay.priv" \
        > "$relay_output" 2>&1 &
    
    RELAY_PID=$!
    
    # Give the process a moment to start
    sleep 0.5
    
    # Docker compatibility: Check if PID capture worked
    if [[ "$RELAY_PID" == "\$!" ]] || ! [[ "$RELAY_PID" =~ ^[0-9]+$ ]]; then
        log "Warning: PID capture failed (Docker issue), using alternative method"
        RELAY_PID=""
    fi
    
    # Wait for relay to be ready
    local wait_count=0
    echo "Waiting for relay to be ready..." >&2
    while [[ $wait_count -lt 30 ]]; do
        # Check process status only if we have a valid PID
        if [[ -n "$RELAY_PID" ]] && ! kill -0 "$RELAY_PID" 2>/dev/null; then
            echo "ERROR: Relay process died unexpectedly" >&2
            echo "Relay output file size: $(wc -c < "$relay_output" 2>/dev/null || echo "0") bytes" >&2
            cat "$relay_output" >&2
            error "Relay server failed to start"
        fi
        
        # Check if relay has output its address
        if grep -q "Listening on:" "$relay_output" 2>/dev/null; then
            # Extract the WebSocket multiaddr with peer ID
            RELAY_MULTIADDR=$(grep -A 10 "Listening on:" "$relay_output" | \
                grep "/ws/p2p/" | head -1 | \
                sed 's/.*\(\/ip4\/.*\)/\1/' | xargs)
            
            if [[ -n "$RELAY_MULTIADDR" ]]; then
                # Find relay PID if we don't have it (Docker workaround)
                if [[ -z "$RELAY_PID" ]] || [[ "$RELAY_PID" == "\$!" ]]; then
                    RELAY_PID=$(pgrep -f "relay.mjs.*--port=$relay_port" | head -1)
                fi
                log "Relay server started (PID: ${RELAY_PID:-unknown})"
                log "Relay multiaddr: $RELAY_MULTIADDR"
                # Re-enable exit on error
                set -e
                return 0
            fi
        fi
        
        # Log progress every 2 seconds
        if [[ $((wait_count % 4)) -eq 0 ]]; then
            echo "Still waiting for relay... (${wait_count}s elapsed)" >&2
            echo "Relay output size: $(wc -c < "$relay_output" 2>/dev/null || echo "0") bytes" >&2
        fi
        
        sleep 0.5
        ((wait_count++)) || true
    done
    
    # Re-enable exit on error before erroring out
    set -e
    
    echo "ERROR: Relay failed to start properly after 15 seconds" >&2
    echo "Relay output:" >&2
    cat "$relay_output" >&2
    echo "" >&2
    echo "Possible issues:" >&2
    echo "- Port $relay_port might be in use" >&2
    echo "- Missing npm dependencies" >&2
    echo "- TypeScript compilation errors" >&2
    error "Failed to get relay multiaddr"
}

run_node() {
    local config_file="$1"
    local node_index="$2"
    local test_dir="$3"
    local output_dir="$4"
    local in_parallel="${5:-false}"  # New parameter to indicate parallel mode

    local node_config
    node_config=$(jq -r ".nodes[$node_index]" "$config_file")

    local node_id script port start_delay expected_exit_code extra_argv
    node_id=$(echo "$node_config" | jq -r '.id')
    script=$(echo "$node_config" | jq -r '.script')
    port=$(echo "$node_config" | jq -r '.port')
    start_delay=$(echo "$node_config" | jq -r '.start_delay // 0')
    expected_exit_code=$(echo "$node_config" | jq -r '.expected_exit_code // 0')
    extra_argv=$(echo "$node_config" | jq -r '.extra_argv // ""')
    
    log "Starting node $node_id (delay: ${start_delay}s)"
    
    # Apply start delay
    if [[ "$start_delay" -gt 0 ]]; then
        sleep "$start_delay"
    fi
    
    # Prepare paths
    local script_path="$test_dir/$script"
    if [[ ! -f "$script_path" ]]; then
        error "Script '$script_path' not found for node $node_id"
    fi
    
    local id_file="$test_dir/ids/$node_id.json"
    local aliases_file="$test_dir/aliases.json"
    local output_file="$output_dir/$node_id.out"
    local error_file="$output_dir/$node_id.err"
    
    # Verify network.sh exists
    if [[ ! -x "./network.sh" ]]; then
        error "network.sh not found or not executable in $TROUPE_ROOT"
    fi
    
    # Verify required files exist
    if [[ ! -f "$id_file" ]]; then
        error "Identity file not found for node $node_id: $id_file"
    fi
    if [[ ! -f "$aliases_file" ]]; then
        error "Aliases file not found: $aliases_file"
    fi
    
    # Build command
    local cmd_args=(
        "./network.sh"
        "$script_path"
        "--id" "$id_file"
        "--aliases" "$aliases_file"
        "--port" "$port"
    )
    
    # Add relay parameter if available
    if [[ -n "$RELAY_MULTIADDR" ]]; then
        cmd_args+=("--relay" "$RELAY_MULTIADDR")
    fi
    
    # Add extra arguments if provided
    if [[ -n "$extra_argv" ]]; then
        # Parse extra_argv as shell arguments
        eval "extra_args=($extra_argv)"
        cmd_args+=("${extra_args[@]}")
    fi
    
    log "Executing: ${cmd_args[*]}"
    
    # Run the node
    cd "$TROUPE_ROOT"
    
    local timeout_val
    timeout_val=$(jq -r '.timeout // 30' "$config_file")

    # Apply CI scaling if needed
    local scaled_timeout="$timeout_val"
    if [[ "$TIMEOUT_SCALE" != "1.0" ]]; then
        # Check if bc is available for floating point math
        if command -v bc >/dev/null 2>&1; then
            scaled_timeout=$(echo "scale=0; ($timeout_val * $TIMEOUT_SCALE)/1" | bc)
        else
            # Fallback to integer math (20% increase for 1.2 scale)
            scaled_timeout=$(( (timeout_val * 12) / 10 ))
        fi
        log "Timeout scaled from ${timeout_val}s to ${scaled_timeout}s"
    fi

    if [[ "$VERBOSE" == "true" ]]; then
        # In verbose mode, show prefixed output
        timeout "$scaled_timeout" "${cmd_args[@]}" \
            > >(tee "$output_file" | sed "s/^/[$node_id:out] /" >&2) \
            2> >(tee "$error_file" | sed "s/^/[$node_id:err] /" >&2) &
    else
        # Normal mode: just redirect to files
        timeout "$scaled_timeout" "${cmd_args[@]}" \
            > "$output_file" 2> "$error_file" &
    fi
    
    local node_pid=$!

    # Docker compatibility: Check if PID capture worked
    if [[ "$node_pid" == "\$!" ]] || ! [[ "$node_pid" =~ ^[0-9]+$ ]]; then
        log "Warning: PID capture failed for node $node_id (Docker issue)"
        # Try to find the process using pgrep
        sleep 0.5
        node_pid=$(pgrep -f "timeout.*$script" | head -1)
        if [[ -z "$node_pid" ]]; then
            node_pid="unknown"
        fi
    fi

    # Only add to cleanup PIDs if not in parallel mode
    # In parallel mode, we wait for all processes explicitly
    if [[ "$node_pid" != "unknown" ]] && [[ "$in_parallel" != "true" ]]; then
        CLEANUP_PIDS+=("$node_pid")
    fi
    
    log "Node $node_id started (PID: $node_pid)"
    
    # Wait for node completion
    local actual_exit_code=0
    if [[ "$node_pid" != "unknown" ]] && [[ "$node_pid" =~ ^[0-9]+$ ]]; then
        wait "$node_pid" || actual_exit_code=$?
    else
        # If we don't have a PID, just wait for any background job
        wait || actual_exit_code=$?
    fi
    
    # Handle timeout exit code (124)
    if [[ "$actual_exit_code" == "124" ]]; then
        if [[ "$expected_exit_code" == "124" ]]; then
            log "Node $node_id timed out as expected after ${scaled_timeout}s"
        else
            display_node_error "$node_id" "timeout" "$scaled_timeout" "$expected_exit_code" "$output_file" "$error_file" "$timeout_val"
            if [[ "$in_parallel" == "true" ]]; then
                return 1  # Return error code instead of exiting in parallel mode
            else
                error "Node $node_id timed out unexpectedly"
            fi
        fi
    elif [[ "$actual_exit_code" != "$expected_exit_code" ]]; then
        display_node_error "$node_id" "exit_code" "$actual_exit_code" "$expected_exit_code" "$output_file" "$error_file"
        if [[ "$in_parallel" == "true" ]]; then
            return 1  # Return error code instead of exiting in parallel mode
        else
            error "Node $node_id exited with code $actual_exit_code, expected $expected_exit_code"
        fi
    fi

    log "Node $node_id completed successfully (exit code: $actual_exit_code)"
    return 0  # Explicitly return success
}

merge_outputs() {
    local config_file="$1"
    local output_dir="$2"
    
    local merge_strategy
    merge_strategy=$(jq -r '.output.merge_strategy // "timestamp"' "$config_file")
    
    log "Merging outputs (strategy: $merge_strategy)"
    
    case "$merge_strategy" in
        "timestamp")
            # Merge all outputs in a consistent order
            # Sort by filename to ensure consistent ordering (client before server)
            # Apply filtering to remove timestamps, UUIDs, etc.
            find "$output_dir" -name "*.out" | sort | xargs cat | \
                "$TROUPE_ROOT/tests/_util/filter.sh"
            ;;
        "sequential")
            # Concatenate outputs in node order
            local node_count
            node_count=$(get_node_count "$config_file")
            
            for ((i=0; i<node_count; i++)); do
                local node_id
                node_id=$(jq -r ".nodes[$i].id" "$config_file")
                
                if [[ -f "$output_dir/$node_id.out" ]]; then
                    echo "=== Output from $node_id ==="
                    cat "$output_dir/$node_id.out" | "$TROUPE_ROOT/tests/_util/filter.sh"
                    echo
                fi
            done
            ;;
        "per_node")
            # Output each node separately without filtering
            local node_count
            node_count=$(get_node_count "$config_file")
            
            for ((i=0; i<node_count; i++)); do
                local node_id
                node_id=$(jq -r ".nodes[$i].id" "$config_file")
                
                if [[ -f "$output_dir/$node_id.out" ]]; then
                    echo "NODE:$node_id"
                    cat "$output_dir/$node_id.out"
                fi
            done
            ;;
        *)
            error "Unknown merge strategy: $merge_strategy"
            ;;
    esac
}

run_test() {
    local config_file="$1"
    
    # Set up temporary directory for this test run
    TEMP_DIR=$(mktemp -d -t troupe-multinode-XXXXXX)
    local output_dir="$TEMP_DIR/output"
    mkdir -p "$output_dir"
    
    # Get test directory
    local test_dir
    test_dir=$(dirname "$(realpath "$config_file")")
    
    local test_name
    test_name=$(jq -r '.test_name' "$config_file")
    
    log "Running multi-node test: $test_name"
    log "Test directory: $test_dir"
    log "Output directory: $output_dir"
    
    # Validate ports are available before starting
    validate_ports "$config_file"
    
    # Setup phase
    setup_network_identities "$test_dir" "$config_file"
    start_relay "$config_file"
    
    # Execution phase
    local coordination
    coordination=$(jq -r '.coordination // "parallel"' "$config_file")
    
    local node_count
    node_count=$(get_node_count "$config_file")
    
    case "$coordination" in
        "parallel")
            # Start all nodes simultaneously
            local node_pids=()
            for ((i=0; i<node_count; i++)); do
                run_node "$config_file" "$i" "$test_dir" "$output_dir" "true" &
                node_pids+=($!)
            done

            # Wait for all nodes
            local failed=false
            local failed_nodes=()
            for ((i=0; i<${#node_pids[@]}; i++)); do
                local pid="${node_pids[$i]}"
                if ! wait "$pid"; then
                    failed=true
                    local node_id=$(jq -r ".nodes[$i].id" "$config_file")
                    failed_nodes+=("$node_id")
                fi
            done

            if [[ "$failed" == "true" ]]; then
                echo "Error: The following nodes failed: ${failed_nodes[*]}" >&2
                # Don't call error() here as it triggers cleanup prematurely
                # Just return failure to the caller
                return 1
            fi
            ;;
        "sequential")
            # Start nodes one after another
            for ((i=0; i<node_count; i++)); do
                run_node "$config_file" "$i" "$test_dir" "$output_dir" "false"
            done
            ;;
        *)
            error "Unknown coordination strategy: $coordination"
            ;;
    esac
    
    # Output phase
    merge_outputs "$config_file" "$output_dir"
    
    log "Multi-node test completed successfully"
}

main() {
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            -*)
                error "Unknown option: $1"
                ;;
            *)
                if [[ -z "$TEST_CONFIG" ]]; then
                    TEST_CONFIG="$1"
                else
                    error "Multiple configuration files specified"
                fi
                shift
                ;;
        esac
    done
    
    if [[ -z "$TEST_CONFIG" ]]; then
        error "No configuration file specified"
    fi
    
    parse_config "$TEST_CONFIG"

    # Temporarily disable set -e for run_test since it handles its own errors
    set +e
    run_test "$TEST_CONFIG"
    local test_result=$?
    set -e

    # Exit with the test result (cleanup will be triggered by trap)
    exit $test_result
}

main "$@"