-- | Uniform reporting of internal compiler invariant violations.
--
-- Use 'internalError' instead of a bare 'error' for conditions that
-- indicate a compiler bug (never a user error), so such failures are
-- uniformly labelled and distinguishable from user-facing diagnostics.
module InternalError (internalError) where

import GHC.Stack (HasCallStack)

internalError :: HasCallStack => String -> a
internalError msg = error ("internal compiler error (please report): " ++ msg)
