
module Language.PureScript.Parser.Lexer where

import Prelude

import Control.Alt
import Control.Apply
import Control.MonadPlus
import Control.Monad.State
import Control.Plus
import Data.Array as Array
import Data.Char.Unicode
import Data.Either
import Data.Foldable
import Data.Functor
import Data.Generic
import Data.Identity
import Data.List as List
import Data.Maybe
import Data.String
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as P
import Text.Parsing.Parser.Pos as P
import Text.Parsing.Parser.String as P
import Text.Parsing.Parser.Token as PT

import Language.PureScript.Comments
import Language.PureScript.Parser.State

data Token
  = LParen
  | RParen
  | LBrace
  | RBrace
  | LSquare
  | RSquare
  | Indent Int
  | LArrow
  | RArrow
  | LFatArrow
  | RFatArrow
  | Colon
  | DoubleColon
  | Equals
  | Pipe
  | Tick
  | Dot
  | Comma
  | Semi
  | At
  | Underscore
  | LName String
  | UName String
  | Qualifier String
  | Symbol String
  | CharLiteral Char
  | StringLiteral String
  | NumberToken (Either Int Number)

derive instance genericToken :: Generic Token
instance showToken :: Show Token where show = gShow
instance eqToken :: Eq Token where eq = gEq

prettyPrintToken :: Token -> String
prettyPrintToken LParen            = "("
prettyPrintToken RParen            = ")"
prettyPrintToken LBrace            = "{"
prettyPrintToken RBrace            = "}"
prettyPrintToken LSquare           = "["
prettyPrintToken RSquare           = "]"
prettyPrintToken LArrow            = "<-"
prettyPrintToken RArrow            = "->"
prettyPrintToken LFatArrow         = "<="
prettyPrintToken RFatArrow         = "=>"
prettyPrintToken Colon             = ":"
prettyPrintToken DoubleColon       = "::"
prettyPrintToken Equals            = "="
prettyPrintToken Pipe              = "|"
prettyPrintToken Tick              = "`"
prettyPrintToken Dot               = "."
prettyPrintToken Comma             = ","
prettyPrintToken Semi              = ";"
prettyPrintToken At                = "@"
prettyPrintToken Underscore        = "_"
prettyPrintToken (Indent n)        = "indentation at level " ++ show n
prettyPrintToken (LName s)         = show s
prettyPrintToken (UName s)         = show s
prettyPrintToken (Qualifier _)     = "qualifier"
prettyPrintToken (Symbol s)        = s
prettyPrintToken (CharLiteral c)   = show c
prettyPrintToken (StringLiteral s) = show s
prettyPrintToken (NumberToken n)        = either show show n

newtype PositionedToken = PositionedToken
  { ptSourcePos :: P.Position
  , ptToken     :: Token
  , ptComments  :: Array Comment
  }
instance showPositionedToken :: Show PositionedToken where
    show (PositionedToken posTok) = "PositionedToken { ptSourcePos = " <> show posTok.ptSourcePos <>
                                                   " , ptToken = " <> prettyPrintToken posTok.ptToken <>
                                                   " , ptComments = " <> show posTok.ptComments <>
                                                   " }"


unPositionedToken :: PositionedToken -> { ptSourcePos :: P.Position, ptToken :: Token, ptComments :: Array Comment }
unPositionedToken (PositionedToken posTok) = posTok

lex :: String -> Either P.ParseError (Array PositionedToken)
lex s = P.runParser s parseTokens

parseTokens :: P.Parser String (Array PositionedToken)
parseTokens = whitespace *> Array.many parsePositionedToken <* P.skipMany parseComment <* P.eof

whitespace :: P.Parser String Unit
whitespace = P.skipMany (P.satisfy isSpace)

parseComment :: P.Parser String Comment
parseComment = (BlockComment <$> blockComment <|> LineComment <$> lineComment) <* whitespace
  where
    blockComment :: P.Parser String String
    blockComment = P.try $ P.string "{-" *> manyTillString (P.try (P.string "-}"))

    lineComment :: P.Parser String String
    lineComment = P.try $ P.string "--" *> manyTillString (P.try (void (P.char '\n') <|> P.eof))

parsePositionedToken :: P.Parser String PositionedToken
parsePositionedToken = P.try do
  comments <- Array.many parseComment
  pos <- getPosition
  tok <- parseToken
  return $ PositionedToken { ptSourcePos: pos, ptToken: tok, ptComments: comments }

parseToken :: P.Parser String Token
parseToken = P.choice
    [ P.try $ P.string "<-" *> P.notFollowedBy symbolChar *> pure LArrow
    , P.try $ P.string "<=" *> P.notFollowedBy symbolChar *> pure LFatArrow
    , P.try $ P.string "->" *> P.notFollowedBy symbolChar *> pure RArrow
    , P.try $ P.string "=>" *> P.notFollowedBy symbolChar *> pure RFatArrow
    , P.try $ P.string "::" *> P.notFollowedBy symbolChar *> pure DoubleColon
    , P.try $ P.char '('    *> pure LParen
    , P.try $ P.char ')'    *> pure RParen
    , P.try $ P.char '{'    *> pure LBrace
    , P.try $ P.char '}'    *> pure RBrace
    , P.try $ P.char '['    *> pure LSquare
    , P.try $ P.char ']'    *> pure RSquare
    , P.try $ P.char '`'    *> pure Tick
    , P.try $ P.char ','    *> pure Comma
    , P.try $ P.char '='    *> P.notFollowedBy symbolChar *> pure Equals
    , P.try $ P.char ':'    *> P.notFollowedBy symbolChar *> pure Colon
    , P.try $ P.char '|'    *> P.notFollowedBy symbolChar *> pure Pipe
    , P.try $ P.char '.'    *> P.notFollowedBy symbolChar *> pure Dot
    , P.try $ P.char ';'    *> P.notFollowedBy symbolChar *> pure Semi
    , P.try $ P.char '@'    *> P.notFollowedBy symbolChar *> pure At
    , P.try $ P.char '_'    *> P.notFollowedBy identLetter *> pure Underscore
    , LName         <$> parseLName
    , do
        uName <- parseUName
        let guard' = guard (validModuleName uName) *> (Qualifier uName <$ P.char '.')
        guard' <|> pure (UName uName)
    , Symbol        <$> parseSymbol
    , CharLiteral   <$> parseCharLiteral
    , StringLiteral <$> parseStringLiteral
    , NumberToken   <$> parseNumber
  ] <* whitespace

  where
    parseLName :: P.Parser String String
    parseLName = map fromCharArray $ Array.cons <$> identStart <*> Array.many identLetter

    parseUName :: P.Parser String String
    parseUName = map fromCharArray $ Array.cons <$> upper <*> Array.many uidentLetter

    parseSymbol :: P.Parser String String
    parseSymbol = fromCharArray <$> Array.some symbolChar

    identStart :: P.Parser String Char
    identStart = lower <|> P.char '_'

    identLetter :: P.Parser String Char
    identLetter = alphaNum <|> P.oneOf ['_', '\'']

    uidentLetter :: P.Parser String Char
    uidentLetter = alphaNum <|> P.char '_'

    symbolChar :: P.Parser String Char
    symbolChar = P.satisfy isSymbolChar

    parseCharLiteral :: P.Parser String Char
    parseCharLiteral = tokenParser.charLiteral

    parseStringLiteral :: P.Parser String String
    parseStringLiteral = blockString <|> tokenParser.stringLiteral
      where
        delimiter :: P.Parser String String
        delimiter   = P.try (P.string "\"\"\"")

        blockString :: P.Parser String String
        blockString = delimiter *> manyTillString delimiter

    parseNumber :: P.Parser String (Either Int Number)
    parseNumber = ( consumeLeadingZero *> empty )
              <|> ( Right <$> P.try tokenParser.float <|> Left <$> P.try tokenParser.natural )
              P.<?> "number"
      where
        -- lookAhead doesn't consume any input if its parser succeeds
        -- if notFollowedBy fails though, the consumed '0' will break the choice chain
        consumeLeadingZero = P.lookAhead
            ( P.char '0' *> ( P.notFollowedBy PT.digit P.<?> "no leading zero in number literal" ) )

-- |
-- We use Text.Parsec.Token to implement the string and number lexemes
--
langDef :: PT.GenLanguageDef String Identity
langDef = PT.LanguageDef
  { reservedNames:    []
  , reservedOpNames:  []
  , commentStart:     ""
  , commentEnd:       ""
  , commentLine:      ""
  , nestedComments:   true
  , identStart:       P.fail "Identifiers not supported"
  , identLetter:      P.fail "Identifiers not supported"
  , opStart:          P.fail "Operators not supported"
  , opLetter:         P.fail "Operators not supported"
  , caseSensitive:    true
  }

-- |
-- A token parser based on the language definition
--
tokenParser :: PT.GenTokenParser String Identity
tokenParser = PT.makeTokenParser langDef

newtype ParseStateWrapper = ParseStateWrapper { parseState :: ParseState
                                              , tokens :: List.List PositionedToken
                                              }

unParseStateWrapper :: ParseStateWrapper -> { parseState :: ParseState, tokens :: List.List PositionedToken }
unParseStateWrapper (ParseStateWrapper p) = p

type TokenParser a = P.Parser ParseStateWrapper a

anyToken :: TokenParser PositionedToken
anyToken =
    token' (\(PositionedToken posTok) -> posTok.ptSourcePos)
           (\(ParseStateWrapper wrapper) -> wrapper.tokens)
           (\(ParseStateWrapper wrapper) tokens -> ParseStateWrapper wrapper { tokens = tokens })

token :: forall a . (Token -> Maybe a) -> TokenParser a
token f = do
    posTok <- anyToken
    case f <<< _.ptToken $ unPositionedToken posTok of
         Nothing -> P.fail $ "unexpected error: " <> show posTok
         Just a -> pure a


-- | Create a parser which returns the first token in the stream.
token' :: forall m a s . (Monad m) => (a -> P.Position) -> (s -> List.List a) -> (s -> List.List a -> s) -> P.ParserT s m a
token' tokpos getTokList createNewS = P.ParserT $ \(P.PState { input: toks, position: pos }) ->
  pure $ case getTokList toks of
    List.Cons x xs -> { consumed: true, input: createNewS toks xs, result: Right x, position: tokpos x }
    _ -> P.parseFailed toks pos "expected token, met EOF"

match :: Token -> TokenParser Unit
match tok = token (\tok' -> if tok == tok' then Just unit else Nothing) P.<?> prettyPrintToken tok

lparen :: TokenParser Unit
lparen = match LParen

rparen :: TokenParser Unit
rparen = match RParen

parens :: forall a . TokenParser a -> TokenParser a
parens = P.between lparen rparen

lbrace :: TokenParser Unit
lbrace = match LBrace

rbrace :: TokenParser Unit
rbrace = match RBrace

braces :: forall a . TokenParser a -> TokenParser a
braces = P.between lbrace rbrace

lsquare :: TokenParser Unit
lsquare = match LSquare

rsquare :: TokenParser Unit
rsquare = match RSquare

squares :: forall a . TokenParser a -> TokenParser a
squares = P.between lsquare rsquare

indent :: TokenParser Int
indent = token go P.<?> "indentation"
  where
  go (Indent n) = Just n
  go _ = Nothing

indentAt :: Column -> TokenParser Unit
indentAt n = token go P.<?> "indentation at level " <> show n
  where
  go (Indent n') | n == n' = Just unit
  go _ = Nothing

larrow :: TokenParser Unit
larrow = match LArrow

rarrow :: TokenParser Unit
rarrow = match RArrow

lfatArrow :: TokenParser Unit
lfatArrow = match LFatArrow

rfatArrow :: TokenParser Unit
rfatArrow = match RFatArrow

colon :: TokenParser Unit
colon = match Colon

doubleColon :: TokenParser Unit
doubleColon = match DoubleColon

equals :: TokenParser Unit
equals = match Equals

pipe :: TokenParser Unit
pipe = match Pipe

tick :: TokenParser Unit
tick = match Tick

dot :: TokenParser Unit
dot = match Dot

comma :: TokenParser Unit
comma = match Comma

semi :: TokenParser Unit
semi = match Semi

at :: TokenParser Unit
at = match At

underscore :: TokenParser Unit
underscore = match Underscore

-- |
-- Parse zero or more values separated by semicolons
--
semiSep :: forall a . TokenParser a -> TokenParser (List.List a)
semiSep = flip P.sepBy semi

-- |
-- Parse one or more values separated by semicolons
--
semiSep1 :: forall a . TokenParser a -> TokenParser (List.List a)
semiSep1 = flip P.sepBy1 semi

-- |
-- Parse zero or more values separated by commas
--
commaSep :: forall a . TokenParser a -> TokenParser (List.List a)
commaSep = flip P.sepBy comma

-- |
-- Parse one or more values separated by commas
--
commaSep1 :: forall a . TokenParser a -> TokenParser (List.List a)
commaSep1 = flip P.sepBy1 comma

lname :: TokenParser String
lname = token go P.<?> "identifier"
  where
  go (LName s) = Just s
  go _ = Nothing

qualifier :: TokenParser String
qualifier = token go P.<?> "qualifier"
  where
  go (Qualifier s) = Just s
  go _ = Nothing

reserved :: String -> TokenParser Unit
reserved s = token go P.<?> show s
  where
  go (LName s') | s == s' = Just unit
  go _ = Nothing

uname :: TokenParser String
uname = token go P.<?> "proper name"
  where
  go (UName s) = Just s
  go _ = Nothing

mname :: TokenParser String
mname = token go P.<?> "module name"
  where
  go (UName s) | validModuleName s = Just s
  go _ = Nothing

uname' :: String -> TokenParser Unit
uname' s = token go P.<?> show s
  where
  go (UName s') | s == s' = Just unit
  go _ = Nothing

symbol :: TokenParser String
symbol = token go P.<?> "symbol"
  where
  go (Symbol s) = Just s
  go Colon      = Just ":"
  go LFatArrow  = Just "<="
  go At         = Just "@"
  go _ = Nothing

symbol' :: String -> TokenParser Unit
symbol' s = token go P.<?> show s
  where
  go (Symbol s') | s == s'   = Just unit
  go Colon       | s == ":"  = Just unit
  go LFatArrow   | s == "<=" = Just unit
  go _ = Nothing

charLiteral :: TokenParser Char
charLiteral = token go P.<?> "char literal"
  where
  go (CharLiteral c) = Just c
  go _ = Nothing

stringLiteral :: TokenParser String
stringLiteral = token go P.<?> "string literal"
  where
  go (StringLiteral s) = Just s
  go _ = Nothing

number :: TokenParser (Either Int Number)
number = token go P.<?> "number"
  where
  go (NumberToken n) = Just n
  go _ = Nothing

natural :: TokenParser Int
natural = token go P.<?> "natural"
  where
  go (NumberToken (Left n)) = Just n
  go _ = Nothing

identifier :: TokenParser String
identifier = token go P.<?> "identifier"
  where
  go (LName s) | s `notElem` reservedPsNames = Just s
  go _ = Nothing

validModuleName :: String -> Boolean
validModuleName s = not $ "_" `contains` s

-- | A list of purescript reserved identifiers
reservedPsNames :: Array String
reservedPsNames = [ "data"
                  , "newtype"
                  , "type"
                  , "foreign"
                  , "import"
                  , "infixl"
                  , "infixr"
                  , "infix"
                  , "class"
                  , "instance"
                  , "derive"
                  , "module"
                  , "case"
                  , "of"
                  , "if"
                  , "then"
                  , "else"
                  , "do"
                  , "let"
                  , "true"
                  , "false"
                  , "in"
                  , "where"
                  ]

reservedTypeNames :: Array String
reservedTypeNames = [ "forall", "where" ]

manyTillString :: forall a . P.Parser String a -> P.Parser String String
manyTillString p = fromCharArray <<< List.toUnfoldable <$> P.manyTill P.anyChar p

lower :: P.Parser String Char
lower = P.satisfy isLower

upper :: P.Parser String Char
upper = P.satisfy isUpper

alphaNum :: P.Parser String Char
alphaNum = P.satisfy isAlphaNum

-- |
-- The characters allowed for use in operators
--
isSymbolChar :: Char -> Boolean
isSymbolChar c =
    let chars = [':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']
    in (c `elem` chars) || (not (isAscii c) && isSymbol c)

getPosition :: forall s m . (Applicative m) => P.ParserT s m P.Position
getPosition = P.ParserT \(P.PState { input: s, position: pos }) ->
    pure { input: s, result: Right pos, consumed: false, position: pos }

getState :: forall m . TokenParser ParseState
getState = _.parseState <<< unParseStateWrapper <$> get

updateIndentationLevel :: Int -> TokenParser Unit
updateIndentationLevel level = modify go
  where
    go :: ParseStateWrapper -> ParseStateWrapper
    go (ParseStateWrapper { parseState: (ParseState parseState), tokens: tokens}) =
        ParseStateWrapper { parseState: (ParseState { indentationLevel: level })
                          , tokens: tokens
                          }
