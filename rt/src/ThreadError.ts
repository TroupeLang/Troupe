export class ThreadError extends Error {
    errstr: string;
    constructor (errstr:string) {
        super () ;
        this.errstr = errstr;        
    }
}

