'use strict'
import levels = require ('./options')


// const LVal = require('./Lval.js');
import { ThreadError } from './ThreadError.js'

import colors = require ('colors/safe')

import { StackCallItem } from './StackCallItem.js'
import { StackGuardItem}  from './StackGuardItem.js'

import { BaseFunction } from './BaseFunction.js'
import { Level } from './Level';
import { StackItem } from './StackItem';
import { Authority } from './Authority';
import { HandlerState } from  './SandboxStatus';

let lub = levels.lub;

class BlockingLev {
    lev: Level
    auth: Authority
    cap : any
}

class Mailbox extends Array  {
    newMessage (x) {
        this.push(x);
    }
}

export class Thread {
    tid: any;
    blockinglev: BlockingLev[];
    threadstack: StackItem[];
    handlerState: any;
    mailbox: Mailbox;
    next: any;
    
    pc: Level;
    blockingdepth: any;
    constructor(tid: any, blockinglev: BlockingLev[], retStackItem: StackItem, handlerState: any) {
        this.tid = tid;
        this.blockinglev = blockinglev;
        this.threadstack = [retStackItem];        
        this.handlerState = handlerState;


        this.mailbox = new Mailbox ();        

        this.next = null;        
    }


    tail(f: (_: any) => any, arg: any) {
        this.next = () => {
            f(arg);
        }
    }


    call(f: any) {
        this.threadstack.push(new StackCallItem(this.pc, f));
    }


    returnInThread(arg: any) {
        let stackItem = this.threadstack.shift();
        
        if (!(stackItem instanceof StackCallItem)) {
            this.threadError("invalid stack state");
            return;
        }

        let stackCallItem = stackItem as StackCallItem;

        this.pc = stackCallItem.pc;

        this.next = () => {
            stackCallItem.f(arg);
        }
    }


    threadError(s: string) {
        console.error(s);
    }
    

    pinipush (l:Authority, cap) {
        this.blockingdepth ++;
        this.blockinglev.unshift ( { lev : this.blockinglev[0].lev, auth : l, cap: cap } );
    }


    pinipop (cap) {
        this.blockingdepth -- ;
        if (this.blockingdepth <= 0) {
            this.threadError ("unmatched pinipop");
        }
        let r = this.blockinglev.shift();
        return r
    }


    raiseBlockingThreadLev (l:Level) {        
        // OBS: 2019-02-27: important that we create a new object;
        // otherwise we get into aliasing issues when saving/restoring
        // thread state in sandboxing; AA 
        this.blockinglev[0] = { lev : lub (this.blockinglev[0].lev, l), 
                                auth: this.blockinglev[0].auth , 
                                cap : this.blockinglev[0].cap
                              }
    }

    raiseCurrentThreadPCToBlockingLev () {        
        this.pc = lub(this.pc, this.blockinglev[0].lev ) ;
    }

    raiseCurrentThreadPC (l: Level)  {        
        this.pc = lub( this.pc, l )
        
        this.raiseBlockingThreadLev(this.pc); 
            // 2018-11-29: AA; observe that we are raise the blocking level
            // automaticaly every time we raise the PC level.
    }
    
}
