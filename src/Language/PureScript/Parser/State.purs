
module Language.PureScript.Parser.State where

import Prelude

type Column = Int

-- | State for the parser monad
newtype ParseState = ParseState {
    -- | The most recently marked indentation level
    indentationLevel :: Column
  }
-- deriving Show

unParseState :: ParseState -> { indentationLevel :: Column }
unParseState (ParseState p) = p

initialParseState :: ParseState
initialParseState = ParseState { indentationLevel: 0 }
