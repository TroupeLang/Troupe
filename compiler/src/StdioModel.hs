-- | The stdio enforcement model a compile targets.
--
-- The model is a property of a run, selected by @--stdio-model@ and understood
-- by both the compiler and the runtime. The compiler's only use for it is the
-- ambient wrappers ('AddAmbientMethods'):
--
--   * 'Capability' — the wrappers are injected. Each acquires its descriptor
--     with the program's own @authority@, which the runtime checks against the
--     channel level.
--
--   * 'Ifc' — the wrappers are not injected. The same names resolve to the
--     runtime's builtins, which acquire nothing and carry the channel check on
--     the write, so no authority is threaded.
module StdioModel
  ( StdioModel(..)
  , parseStdioModel
  , stdioModelName
  , defaultStdioModel
  ) where

data StdioModel = Capability | Ifc
  deriving (Eq, Show)

-- | The model a compile targets when @--stdio-model@ is absent. It matches the
-- runtime's default so that an unflagged compile and an unflagged run agree.
defaultStdioModel :: StdioModel
defaultStdioModel = Capability

-- | Parse the flag's argument; 'Nothing' for anything else.
parseStdioModel :: String -> Maybe StdioModel
parseStdioModel "capability" = Just Capability
parseStdioModel "ifc"        = Just Ifc
parseStdioModel _            = Nothing

-- | The spelling the flag accepts, for messages.
stdioModelName :: StdioModel -> String
stdioModelName Capability = "capability"
stdioModelName Ifc        = "ifc"
