import { RuntimeInterface } from "./RuntimeInterface";

let __state: RuntimeInterface = null;

export function setRuntimeObject (x) {
    if (__state) {
        throw new Error ("Runtime object is already set")
    }
    __state = x
}


export function getRuntimeObject () {
    return __state;
}
