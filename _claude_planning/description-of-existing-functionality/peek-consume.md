# Peek, Consume, and Guard Functions in Troupe's Mailbox System

## Overview

The `peek`, `consume`, and `guard` functions are core operations in Troupe's mailbox processing system, implementing selective message reception with information flow control. These functions work together to enable actors to safely examine, pattern match, and retrieve messages from their mailbox while maintaining security guarantees.

## Function Descriptions

### `peek` Function

**Location**: `/rt/src/MailboxProcessor.mts:107-138`

The `peek` function non-destructively examines a message at a specific index within a security level interval:

```typescript
peek(lev: Level, index: number, lowb: Level, highb: Level)
```

**Parameters**:
- `lev`: The security level context for the operation
- `index`: The logical index of the message to examine (0-based)
- `lowb`: Lower bound of the security level interval
- `highb`: Upper bound of the security level interval

**Behavior**:
1. Only considers messages whose presence level falls within the interval `[lowb, highb]`
2. Returns the message value without removing it from the mailbox
3. The returned value's security level is computed as `lub(message.level, lev)`
4. Implements caching optimization for sequential peeks
5. Blocks the thread if the requested message index is not yet available

### `consume` Function

**Location**: `/rt/src/MailboxProcessor.mts:140-174`

The `consume` function removes and returns a message at a specific index within a security level interval:

```typescript
consume(lev: Level, index: number, lowb: Level, highb: Level)
```

**Parameters**: Same as `peek`

**Behavior**:
1. Similar to `peek` but removes the message from the mailbox
2. Performs security clearance checks before allowing the operation
3. Invalidates the peek cache after consumption
4. Returns the message with security level `lub(message.level, lev)`

### `guard` Function

**Location**: `/rt/src/builtins/receive.mts:147-193`

The `guard` function provides safe pattern matching for message handlers with automatic rollback on failure or side effects:

```typescript
guard(f: Function, taintLimit: Level, default: Value)
```

**Parameters**:
- `f`: The handler function to execute (typically pattern matching on a message)
- `taintLimit`: Maximum security level allowed for the handler execution
- `default`: Default value returned if the handler fails or violates security constraints

**Return Value**:
A tuple `(status, body)` where:
- `status`: 0 if handler succeeded, 1 if it failed or was trapped
- `body`: Either the handler's result (if successful) or a function returning the default value

**Behavior**:
1. Executes the handler function in a sandboxed environment
2. Monitors for side effects during pattern matching
3. Enforces taint limits to prevent information leaks
4. Automatically rolls back state if:
   - The handler performs side effects (e.g., sends messages during pattern matching)
   - The handler violates the taint limit
   - Pattern matching fails
5. Returns control to the caller with success/failure status

## Security Model

All three functions enforce Troupe's information flow control:

### Interval Filtering
Messages are only visible if their presence level `L` satisfies:
```
flowsTo(lowb, L) && flowsTo(L, highb)
```

This ensures that:
- Messages below `lowb` are too sensitive to observe
- Messages above `highb` are not authorized for access

### Clearance Checks (for `consume`)
The `consume` operation requires sufficient mailbox clearance:

1. **Sufficient Clearance**: 
   ```
   flowsTo(lub(highb, pc), lub(lowb, mclear.boost_level))
   ```

2. **No Information Leak**:
   ```
   flowsTo(mclear.pc_at_creation, glb(pc, lowb))
   ```

These checks prevent information leaks through the pattern of message consumption.

### Guard Security (for `guard`)
The `guard` function enforces security through:

1. **Taint Tracking**: 
   - Raises the PC to `lub(f.lev, taintLimit.lev, taintLimit.val)` before execution
   - Monitors that the blocking level stays within bounds during execution

2. **Sandbox Isolation**:
   - Sets handler state to `INHANDLER` to detect side effects
   - Installs a "trapper" that rolls back on violations
   - Restores normal state only on successful completion

3. **Automatic Rollback**:
   - Stack pointer restoration on failure
   - PC and blocking level reset to safe values
   - Returns default value instead of leaking information

## Usage in Receive Operations

The `rcv` function in `/trp-rt/service.trp:27-46` demonstrates the typical usage pattern:

```troupe
fun rcv (l1, l2, hns) = 
    let fun work i = 
        let val v = peek (i, l1, l2)  (* Examine message without removing *)
            fun iterate [] = work (i+1)  (* Try next message *)
              | iterate (h::hns) = 
                    let val (status, body) = guard (fn () => h v, l2, (1, ()))
                    in if status = 0  (* Handler matched *)
                          then body (consume (i, l1, l2))  (* Remove and process *)
                          else iterate hns  (* Try next handler *)
                    end 
        in iterate hns 
        end
    in work 0
    end
```

**Process Flow**:
1. Start with index 0
2. Use `peek` to examine each message in the security interval
3. Try each handler with the `guard` function for safe pattern matching
4. If a handler matches (status = 0), use `consume` to remove the message
5. If no handler matches, increment index and continue
6. Block if no suitable messages are available

### Guard Usage Details

The `guard` call in line 38:
```troupe
let val (status, body) = guard (fn () => h v, l2, (1, ()))
```

- **First argument**: `fn () => h v` - Wraps the handler `h` applied to message `v` in a thunk
- **Second argument**: `l2` - The upper bound level serves as the taint limit
- **Third argument**: `(1, ())` - Default value returned on failure

This ensures that pattern matching in handlers cannot leak information through side effects or exceptions.

## Performance Optimizations

### Peek Cache
The mailbox maintains a cache to optimize sequential peeks:
- `peek_cache_index`: Last successfully peeked index
- `peek_cache_position`: Actual position in mailbox array
- `peek_cache_lowb/highb`: Security bounds used for the peek

This optimization is particularly effective for the receive pattern where messages are examined sequentially until a match is found.

### Cache Invalidation
The cache is reset when:
- A message is consumed
- Security bounds change
- The mailbox is modified

## Example Usage Patterns

### Simple Receive at Current PC
```troupe
receive [
    hn (pattern1) => action1,
    hn (pattern2) => action2
]
```
This uses `peek` and `consume` with `lowb = highb = current_pc`.

### Handler with Side Effects (Will Fail)
```troupe
receive [
    hn (x, sender) when foo(sender) => x  (* foo sends a message - will be trapped *)
]
```
The `guard` function will detect the side effect in `foo` and roll back, trying the next handler.

### Interval Receive
```troupe
rcv (lowLevel, highLevel, handlers)
```
This examines all messages in the security interval `[lowLevel, highLevel]`.

### Point Receive at Specific Level
```troupe
rcvp (level, handlers)
```
This is equivalent to `rcv(level, level, handlers)`.

## Key Design Principles

1. **Non-destructive Examination**: `peek` allows testing messages against multiple handlers without side effects
2. **Atomic Consumption**: `consume` ensures exactly-once message processing
3. **Safe Pattern Matching**: `guard` prevents information leaks through pattern matching side effects
4. **Security by Design**: All operations respect information flow constraints
5. **Efficient Blocking**: Threads block when no matching messages exist, waking when new messages arrive
6. **Cache Optimization**: Sequential access patterns are optimized through caching
7. **Fail-Safe Defaults**: `guard` returns default values rather than exposing failures

## Implementation Notes

### Handler State Management
The `guard` function uses a special `INHANDLER` state to track when pattern matching is occurring. This state:
- Prevents side effects during pattern evaluation
- Enables automatic rollback on violations
- Ensures handlers are pure functions from the message examination perspective

### Stack Management
The guard implementation carefully manages the stack:
- Saves the stack pointer before handler execution
- Pushes a special `guardFrame` to handle return values
- Restores stack pointer on rollback to undo any partial changes

### Information Flow Integration
All three functions work together to maintain information flow security:
- `peek` respects level intervals when exposing message presence
- `guard` prevents handlers from leaking information through effects
- `consume` enforces clearance checks before allowing message removal

This design ensures that the receive operation is both flexible and secure, allowing complex pattern matching while preventing information leaks.