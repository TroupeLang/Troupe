# Testing infrastructure for Troupe

We assume that the existence of `tests` directory with the following structure:
    
        tests
        ├── cmp                   (* negative tests for the compiler *)
        └── rt                    (* other tests; all should be ok by compiler *)
            ├── neg               (* negative runtime tests *)
            │   ├── core
            │   └── ifc
            ├── pos               (* positive runtime tests *)
            │   ├── core
            │   └── ifc
            └── timeout
                ├── blocking
                └── diverging
                
We further distinguish between core and ifc tests, in each category of positive and negative tests. 
There is a fair amount of redundancy in the core positive runtime tests, and they would benefit from
a cleanup pass. 

## Negative tests for the compiler

The compiler should return the exit code 1 when it fails

## Negative tests for the runtime

Error messages from the runtime are generally printed out in the console
as part of the normal output.

## Termination of the runtime

We invoke the tests using `./local.sh` script that does not activate
the p2p infrastructure. 
