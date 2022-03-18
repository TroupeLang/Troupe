import { Thread } from "./Thread";
import colors = require('colors/safe');
import { SchedulerInterface } from "./SchedulerInterface";

export abstract class TroupeError extends Error {
    abstract handleError (sched: SchedulerInterface) : void 
}

export abstract class ThreadError extends TroupeError {
    abstract errorMessage: string
    thread: Thread
    constructor (thread:Thread) {
        super ()
        this.thread = thread;
    }
}

export abstract class StopThreadError extends ThreadError {
    handleError (sched) {
        console.log (colors.red ( "Runtime error in thread " + this.thread.tidErrorStringRep()))
        console.log (colors.red ( ">> " + this.errorMessage));
        sched.stopThreadWithErrorMessage(this.thread, this.errorMessage);
    }
}

export class StrThreadError extends StopThreadError {
    errstr: string;
    get errorMessage () { return this.errstr }
    constructor (thread:Thread, errstr:string ) {
        super (thread) ;
        this.errstr = errstr;        
    } 
}

export class HandlerError extends ThreadError {
    constructor (thread: Thread, errstr: string) {
        super (thread);
        this.errstr = errstr; 
    }
    errstr: string 
    get errorMessage () { return this.errstr }
    handleError( sched:SchedulerInterface  ) {
          // we have an error inside of an receive pattern or guard;
          // we are discarding the rest of the current thread and are
          // scheduling the execution of the handler 
          console.log (colors.yellow (`Warning: runtime exception in the handler or sandbox: ${this.errstr}`))
          this.thread.next = this.thread.handlerState.getTrapper();
          sched.scheduleThread(this.thread)
    }
}

export class ImplementationError extends Error { // observe that this does not inherit from TroupeError
    errstr :string
    constructor (s: string) {
        super ()
        this.errstr = s 
    }
}