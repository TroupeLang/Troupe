import { SchedulerInterface } from "./SchedulerInterface.mjs";
import { Thread } from "./Thread.mjs";
import { LVal } from './Lval.mjs'
import { MailboxInterface } from "./MailboxInterface.mjs";

export interface RuntimeInterface {
    cleanup(): Promise<void>
    mkLabel(levid: any): any;
    rt_mkuuid();
    spawnAtNode(arg0: any, arg1: any);
    $t: Thread;   
    $service: any; // todo 2021-06-13; identify what the right interface here should be     
    debug(arg0: string);
    __sched: SchedulerInterface
    __mbox : MailboxInterface
    sendMessageNoChecks(toPid: any, message: import("./Lval.mjs").LVal, arg2?: boolean): any;
    ret(arg0: any);
    // ret_raw ()
    // tailcall(funclos: any, __unit: any);
    persist (obj, path)    
    xconsole: Console
}