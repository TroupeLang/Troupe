# What's next 


## Plan 
``` 
             ┌──────────────────────────┐                            
             │      dev-integrity       │   
             └──────────────────────────┘                            
                           │                                         
                           │                                         
                           │  Complete integrity                       
                           │  with quarantines                       
                           │                                         
             ┌─────────────▼────────────┐                            
             │           dev            │  Deprecate dev-integrity
             └──────────────────────────┘  and merge the results into the dev branch 
                           │                                         
                           │                                         
             ┌─────────────┴──────────────────────┐                  
             │                                    │                  
             ▼                                    │                  
┌──────────────────────────┐        ┌─────────────▼────────────┐     
│       dev-modules        │        │       dev-inlining       │     
└──────────────────────────┘        └──────────────────────────┘     
              │                                                      
              │                                                      
              ▼                                                      
┌──────────────────────────┐                                         
│  dev-syntactic-variants  │                                         
└──────────────────────────┘                                         
```
 
## Integrity 
 
- [x] Integrity of blocking and mailboxes
    - [x] First implementation of the blocking labels done?
    - Mailboxes 
      - [x] What is the integrity interpretation of the mailbox clearances?
      - [x] Investigate the syntax to use for mailbox declassification and endorsement. 
            Do we need to rename `lowermbox` to `declassifyMbox` `endorseMbox`. In principle, we can do that, creating some backward compatible code for transition
      - [x] Create examples showcasing the usage of these primitives

    - [ ] Recall checked endorsements of my paper with Andrew; are they relevant here? 

## Quarantining 

- [x] Outline of the quarantining in the security model
- [x] Implementation (quarantine builtins, multinode tests, `--no-quarantine` CLI opt-out)


## NMIFC evaluation
    
        
- [ ] Capability checks in the runtime should check for the ROOT 
    authority, e.g., for privileged operations such as register.

- [ ] Do we need a coalescing primitive for authority (it's sort of but not 
     exactly the opposuite of attenuation) to support quarantining?

- [x] Create tests that are specific to integrity and DC labels

- [ ] Create tests that investigate the integrity of the mailbox 
      clearances

- [ ] Sanitization-inspired example for integrity


- [ ] The implications of NFIC being on by default?

## Frontend

- [ ] Revisit AtPattern AST node. 
        
       See the declaration in the Parser.y 
       and the usage in a few places. We don't appear to be using 
       it in any of the examples, but it may appear useful again 
       in the context of quarantining. 

- [ ] Note that the way the edge labels are printed in by the runtime is
  currently different from how they are supposed to be parsed.  This may or may
  not be okay; we can talk about implementing context-depending parsing as part
  of the frontend

## Infra


[ ] Implement spawn as the top level primitive that does all 
    the pattern matching and then calls into local_spawn when needed.

[ ] Look into making HAMT faster 

[ ] Inline one-time declared joins (maybe? | this needs profiling)

### Other improvements

#### Backend

- [x] Provide a runtime option to NOT use V1 compatible pretty printing
      (done: `--label-format v1|v2|v2-full` runtime option)

## Refactoring


### Runtime 

- [+ongoing+] Consolidate error handling of downgrading
      (declassification rework merged via PR #151; verify whether further
      consolidation is still needed or this can be closed)

- [x] Get rid of lubs in the runtime codebase, because it is redundant, now that we have a multi-arg lub

- [ ] DCLabel caching

- [ ] Refactor the interface for AbstractLevelSystem
  - use `lub2` , `glb2` for binary operations 
  - define abstract overridable `lub` `glb` that do 
    the obvious thing of iterating over the list and 
    computing the results using pairwise applications of 
    lub2, glb2; and allow for improved implementations in the
    classes (e.g., the way it is done in tagsets)
   
    | the only reason this is not done right now 
    | is that it may be difficult to test it exhaustively 
    | and it is not the biggest of the priorities. 

### Compiler

- [x] For the declaration `type DCLabOrConst = Either LabelExp LabelConst`
  change it to a new custom type (easier to track than Left/Right)
  (done: replaced with `LabelComponent` data type)

## Dependency management

- [x] Upgrade all libp2p dependencies (done: upgraded to libp2p v3 ecosystem)
- [ ] "skipLibCheck" in tsconfig should be set back to false (or removed).

## Serialization 

- [ ] Using new security model in serialization
- [ ] Using efficient serialization engine, e.g., protobufs

## Minimizing the built-in surface

- [ ] Implement `_pc` as `_pc () = levelOf ()` intsead of 
  another built-in, in the Missing library.



