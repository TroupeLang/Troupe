# Password Checker: Confused Deputy Attack Demonstration

This is a demonstration of a **Confused Deputy Attack** in the context of Nonmalleable Information Flow Control (NMIFC), based on Cecchetti et al.'s "Nonmalleable Information Flow Control" (Section 2.1).

## Overview

The program shows how NMIFC can prevent attacks that exploit trusted code to perform unauthorized operations. NMIFC enforcement is on by default, so the attack is stopped by a plain run and succeeds only when it is disabled with `--no-nmifc`.

## Actors

| Actor    | Role                                                      |
|----------|-----------------------------------------------------------|
| Server   | Trusted password-checking service with secret password    |
| Attacker | Malicious actor attempting to authenticate fraudulently   |

## The Confused Deputy Pattern

The key vulnerability is the `ConfusedDeputy` message handler in the server:

```sml
hn (ConfusedDeputy, f) =>
    spawn (fn () => f server_password)
```

The server accepts a function `f` from an untrusted source and **executes it with the server's secret password as an argument**. The server becomes a "confused deputy" - it has legitimate authority but is tricked into misusing it.

This pattern models real-world scenarios where servers accept callbacks, plugins, or code from clients.

## Attack Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                             ATTACK SEQUENCE                                 │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  1. Attacker sends ConfusedDeputy message containing exploit function       │
│                                                                             │
│         Attacker ───────(ConfusedDeputy, exploit_fn)───────► Server         │
│                                                                             │
│  2. Server spawns the exploit, passing it the secret password               │
│                                                                             │
│         Server: spawn (fn () => exploit_fn server_password)                 │
│                                                                             │
│  3. Exploit function receives password and sends AuthRequest                │
│                                                                             │
│         Exploit ───────(AuthRequest, leaked_password)───────► Server        │
│                                                                             │
│  4. Server endorses the guess (NMIFC VIOLATION)                             │
│                                                                             │
│         val endorsed_guess = endorse (guess, authority, `<server;server>`)  │
│                                                                             │
│  5. Authentication succeeds with the stolen password                        │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## The NMIFC Violation

The critical point is the endorsement of the incoming guess:

```sml
val endorsed_guess = endorse (guess, authority, `<server;server>`)
```

The server endorses data that was influenced by untrusted (attacker) code. This violates the nonmalleability property - the attacker has "forged" a value that appears to have server integrity but was actually constructed through illegitimate means.

## Behavior With and Without NMIFC

| Mode          | Command                        | Result                                      |
|---------------|--------------------------------|---------------------------------------------|
| Without NMIFC | `./local.sh program.trp --no-nmifc` | Attack succeeds - password leaks and authenticates |
| With NMIFC (default) | `./local.sh program.trp` | Attack blocked - endorsement is rejected    |

## Key Security Primitives Used

| Primitive                  | Purpose                                                    |
|----------------------------|------------------------------------------------------------|
| `raisembox` / `lowermbox`  | Temporarily adjust mailbox clearance for receiving messages |
| `blockdown`                | Downgrade both PC and blocking labels                      |
| `endorse`                  | Raise integrity of data (the NMIFC-relevant operation)     |
| `declassify`               | Release confidentiality of authentication result           |
| `raisedTo`                 | Attach security labels to values                           |

## Why This Matters

The confused deputy attack is particularly insidious because:

1. **The server code looks correct** - it properly handles authentication requests
2. **The vulnerability is in accepting untrusted code** - a common pattern in extensible systems
3. **Traditional IFC doesn't catch it** - the data flow appears legitimate
4. **NMIFC tracks provenance** - it recognizes that endorsed data was influenced by untrusted sources

This demonstrates why nonmalleability is essential for preventing attacks in systems that combine trusted and untrusted code.
