module IR2JS where 

import Data.ByteString.Lazy (ByteString)
import IR
import qualified IR2Raw (ir2raw)
import qualified RawOpt 
import qualified Raw2Stack (raw2Stack)
import qualified Stack 
import qualified Stack2JS


-- RT calls this to compile received code.
ir2Stack :: SerializationUnit -> Stack.StackUnit
ir2Stack = Raw2Stack.raw2Stack . RawOpt.rawopt . IR2Raw.ir2raw 


irToJSString :: SerializationUnit -> String
irToJSString = Stack2JS.stack2JSString  . ir2Stack 


irToJSON :: SerializationUnit -> ByteString
irToJSON = Stack2JS.stack2JSON . ir2Stack 

