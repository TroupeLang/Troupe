import { UserRuntimeZero, Constructor, mkBase } from './UserRuntimeZero'
import { LCopyVal } from '../Lval';
import { assertIsNTuple, assertIsAuthority, assertIsLevel } from '../Asserts';
import { __unit } from '../UnitVal';
import options, { lubs } from '../options'

const levels = options;
const { lub, flowsTo } = levels

export function BuiltinDeclassify<TBase extends Constructor<UserRuntimeZero>>(Base: TBase) {
    return class extends Base {
        declassify = mkBase((arg) => {
            //  assertDeclassificationAllowed()// 2019-03-06: AA: allowing declassification everywhere?
            assertIsNTuple(arg, 3);

            let argv = arg.val;
            let data = argv[0];

            let auth = argv[1];
            assertIsAuthority(auth);

            let toLevV = argv[2];
            assertIsLevel(toLevV);

            let pc = this.runtime.$t.pc;

            let levFrom = data.lev;

            let bl = this.runtime.$t.bl ;

            let lev_to = toLevV.val 

            let block_is_low = flowsTo (bl, lev_to)
            if (!block_is_low) {
                let errorMessage = 
                  "Current blocking level does not flow to the target level of the declassification\n" + 
                     ` | target level of the declassification: ${lev_to.stringRep()}\n` + 
                     ` | current blocking level: ${bl.stringRep()}`
                this.runtime.$t.threadError (errorMessage)
            }
 
            // check that levFrom ⊑ auth ⊔ levTo
            let _l = lubs([auth.val.authorityLevel, lev_to]);


            let ok_to_declassify =
                flowsTo(levFrom, _l)

            if (ok_to_declassify) {
                // we need to collect all the restrictions
                let r = new LCopyVal(data, lubs([lev_to, pc, arg.lev, auth.lev]));

                return this.runtime.ret(r) // schedule the return value
            } else {
                let errorMessage =
                    "Not enough authority for declassification\n" +
                    ` | level of the data: ${data.lev.stringRep()}\n` +
                    ` | level of the authority: ${auth.val.authorityLevel.stringRep()}\n` +
                    ` | target level of the declassification: ${lev_to.stringRep()}`
                this.runtime.$t.threadError(errorMessage);

            }

        }, "declassify")
    }
}