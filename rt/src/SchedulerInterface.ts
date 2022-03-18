import { Thread } from "./Thread";

export interface SchedulerInterface {
    // tailToTroupeFun(f: any, arg:any)
    // tailToTroupeFun_raw(f: any)
    // stepThread();
    resetScheduler();
    __alive: any;
    scheduleNewThreadAtLevel(fun: any, arg: any, pc: any, blockingTopLev: any);        
    scheduleThread(theThread: any);
    resumeLoopAsync();
    blockThread(__currentThread: Thread);
    isAlive(toPid: any);
    getThread(toPid: any);
    unblockThread(toPid: any);
    schedule(fun: any, args: any[], namespace: any);
    __currentThread: Thread;
    stopThreadWithErrorMessage (t:Thread, s:string)
    
}