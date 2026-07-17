import {UserRuntimeZero, Constructor, mkBase} from './UserRuntimeZero.mjs'
import { assertIsString, assertIsNumber, assertIsNTuple } from '../Asserts.mjs'
import { __unit } from '../UnitVal.mjs';
import { TroupeType } from '../TroupeTypes.mjs';
import Table from 'cli-table3';
import { Record } from '../Record.mjs';


export function BuiltinDebugUtils <TBase extends Constructor<UserRuntimeZero>> (Base:TBase) {
    return class extends Base {
        _setProcessDebuggingName = mkBase((arg) => {
            assertIsString(arg)
            this.runtime.$t.processDebuggingName = arg.val
            return this.runtime.ret(__unit)
        })

        debugpc = mkBase((arg) => {
            this.runtime.debug("");
            // this.runtime.$t.showStack()
            return this.runtime.ret(__unit);
        }, "debugpc")

        _debug = mkBase ((arg) => {
            console.log (arg.stringRep(true))
            return this.runtime.ret(__unit);
        })

        _setFailureRate = mkBase((arg) => {
            let _tt = arg.getTroupeType;
            switch (_tt) {
                case TroupeType.NUMBER:
                    this.runtime.$t.failureRate = arg.val
                    this.runtime.$t.failureStartTime = 0;
                    break;
                case TroupeType.TUPLE:
                    assertIsNTuple(arg, 2)
                    assertIsNumber (arg.val[0])
                    assertIsNumber (arg.val[1])
                    this.runtime.$t.failureRate = arg.val[0].val
                    this.runtime.$t.failureStartTime = Date.now() + arg.val[1].val
                    break;
                default:
                    this.runtime.$t.threadError ("Invalid argument type in function _setFailureRate");
            }
            return this.runtime.ret(__unit);
        })

        debugMbox = mkBase((_arg) => {
            const thread = this.runtime.$t;
            const mailbox = thread.mailbox;
            const maxMessages = 10;
            const valueColWidth = 100;  // width of value column in tables

            const boxChars = {
                'top': '═', 'top-mid': '╤', 'top-left': '╔', 'top-right': '╗',
                'bottom': '═', 'bottom-mid': '╧', 'bottom-left': '╚', 'bottom-right': '╝',
                'left': '║', 'left-mid': '╟', 'mid': '─', 'mid-mid': '┼',
                'right': '║', 'right-mid': '╢', 'middle': '│'
            };

            // Metadata table
            const metaTable = new Table({
                chars: boxChars,
                style: { head: [], border: [], 'padding-left': 1, 'padding-right': 1 },
                colWidths: [18, valueColWidth],
                wordWrap: true,
                wrapOnWordBoundary: false
            });

            const mclear = mailbox.mclear;
            metaTable.push(
                [{ colSpan: 2, content: 'MAILBOX DEBUG INFO', hAlign: 'center' }],
                ['Thread ID', thread.tidErrorStringRep()],
                ['Total messages', String(mailbox.logicalSize)],
                ['Mbox clearance', mclear.stringRep()]
            );

            console.log("");
            console.log(metaTable.toString());

            if (mailbox.logicalSize === 0) {
                const emptyTable = new Table({
                    chars: boxChars,
                    style: { head: [], border: [] },
                    colWidths: [18 + valueColWidth],
                    wordWrap: true
                });
                emptyTable.push([{ content: '(mailbox is empty)', hAlign: 'center' }]);
                console.log(emptyTable.toString());
                console.log("");
                return this.runtime.ret(__unit);
            }

            // Show the last N messages (most recent)
            const startIdx = Math.max(mailbox.head, mailbox.length - maxMessages);

            if (startIdx > mailbox.head) {
                console.log(`(showing last ${maxMessages} of ${mailbox.logicalSize} messages)`);
            }

            // Display each message in vertical layout
            for (let i = startIdx; i < mailbox.length; i++) {
                const mbVal = mailbox[i];
                // mbVal.val is a tuple: [msg, metadata_record_lval]
                const msgTuple = mbVal.val;
                const actualMsg = msgTuple[0];  // The actual message
                const metadataLVal = msgTuple[1];  // LVal containing metadata Record
                const metadata = metadataLVal.val as Record;

                const msgStr = actualMsg.stringRep(true);
                const levStr = mbVal.lev.stringRep();

                // Get senderNode (always present) - display with label
                const senderLVal = metadata.getField('senderNode');
                const senderStr = senderLVal.stringRep();

                // Get quarantineAuth (optional) - display with label
                let quarantineStr: string | null = null;
                if (metadata.hasField('quarantineAuth')) {
                    const authLVal = metadata.getField('quarantineAuth');
                    quarantineStr = authLVal.stringRep();
                }

                // Create vertical table for this message
                const msgTable = new Table({
                    chars: boxChars,
                    style: { head: [], border: [], 'padding-left': 1, 'padding-right': 1 },
                    colWidths: [18, valueColWidth],
                    wordWrap: true,
                    wrapOnWordBoundary: false  // wrap anywhere, not just word boundaries
                });

                msgTable.push(
                    [{ colSpan: 2, content: `MESSAGE #${i}`, hAlign: 'center' }],
                    ['Value', { content: msgStr }],
                    ['Label', { content: levStr }],
                    ['Sender', { content: senderStr }]
                );

                if (quarantineStr !== null) {
                    msgTable.push(['Quarantine', { content: quarantineStr }]);
                }

                console.log(msgTable.toString());
            }

            console.log("");

            return this.runtime.ret(__unit);
        }, "debugMbox")
    }
}

