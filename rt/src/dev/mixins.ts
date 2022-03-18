type Constructor<T = {}> = new (...args: any[]) => T;


class XX {
    field = () => { return 10 + this.x }
    x : number 
    constructor (x: number) {
        this.x = x 
    }
}


function AddFunctionalityOne<TBase extends Constructor<XX>> (Base:TBase) {
    return class extends Base {
        timestamp = Date.now ()
        bar () { return this.x  + 1}
    }
}

function AddFunctionalityTwo<TBase extends Constructor<XX>> (Base:TBase) {
    return class extends Base {                
        foo () { return this.x  + 3}
    }
}

const AA = AddFunctionalityTwo (AddFunctionalityOne (XX))


const a = new AA (19)


console.log ("Mixins example", a.foo() + a.bar())

