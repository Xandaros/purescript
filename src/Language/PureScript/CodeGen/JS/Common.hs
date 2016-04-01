-- |
-- Common code generation utility functions
--
module Language.PureScript.CodeGen.JS.Common where

import Data.Char
import Data.List (intercalate)

import Language.PureScript.Crash
import Language.PureScript.Names

moduleNameToJs :: ModuleName -> String
moduleNameToJs (ModuleName pns) =
  let name = intercalate "_" (runProperName `map` pns)
  in if nameIsJsBuiltIn name then "__" ++ name else name

-- |
-- Convert an Ident into a valid Javascript identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
--
--  * Symbols are prefixed with '$' followed by a symbol name or their ordinal value.
--
identToJs :: Ident -> String
identToJs (Ident name)
  | nameIsJsReserved name || nameIsJsBuiltIn name = "__" ++ name
  | otherwise = concatMap identCharToString name
identToJs (Op op) = concatMap identCharToString op
identToJs (GenIdent _ _) = internalError "GenIdent in identToJs"

-- |
-- Test if a string is a valid JS identifier without escaping.
--
identNeedsEscaping :: String -> Bool
identNeedsEscaping s = s /= identToJs (Ident s)

-- |
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToString :: Char -> String
identCharToString c | isAlphaNum c = [c]
identCharToString '_'  = "_"
identCharToString '.'  = "_dot"
identCharToString '$'  = "_dollar"
identCharToString '~'  = "_tilde"
identCharToString '='  = "_eq"
identCharToString '<'  = "_less"
identCharToString '>'  = "_greater"
identCharToString '!'  = "_bang"
identCharToString '#'  = "_hash"
identCharToString '%'  = "_percent"
identCharToString '^'  = "_up"
identCharToString '&'  = "_amp"
identCharToString '|'  = "_bar"
identCharToString '*'  = "_times"
identCharToString '/'  = "_div"
identCharToString '+'  = "_plus"
identCharToString '-'  = "_minus"
identCharToString ':'  = "_colon"
identCharToString '\\' = "_bslash"
identCharToString '?'  = "_qmark"
identCharToString '@'  = "_at"
identCharToString '\'' = "_prime"
identCharToString c = '_' : show (ord c)

-- |
-- Checks whether an identifier name is reserved in Javascript.
--
nameIsJsReserved :: String -> Bool
nameIsJsReserved name =
  name `elem` jsAnyReserved

-- |
-- Checks whether a name matches a built-in value in Javascript.
--
nameIsJsBuiltIn :: String -> Bool
nameIsJsBuiltIn name =
  name `elem`
    [ "collectgarbage"
    , "coroutine"
    , "pcall"
    , "utf8"
    , "error"
    , "tostring"
    , "package"
    , "next"
    , "assert"
    , "io"
    , "module"
    , "ipairs"
    , "loadstring"
    , "select"
    , "_VERSION"
    , "xpcall"
    , "debug"
    , "loadfile"
    , "load"
    , "_G"
    , "string"
    , "type"
    , "setmetatable"
    , "bit32"
    , "arg"
    , "tonumber"
    , "os"
    , "print"
    , "table"
    , "pairs"
    , "unpack"
    , "rawget"
    , "rawset"
    , "dofile"
    , "getmetatable"
    , "rawequal"
    , "rawlen"
    , "require"
    , "math"
    ]

jsAnyReserved :: [String]
jsAnyReserved =
  concat
    [ jsKeywords
    , jsLiterals
    ]

jsKeywords :: [String]
jsKeywords =
  [ "and"
  , "break"
  , "do"
  , "else"
  , "elseif"
  , "end"
  , "for"
  , "function"
  , "goto"
  , "if"
  , "in"
  , "local"
  , "not"
  , "or"
  , "repeat"
  , "return"
  , "then"
  , "until"
  , "while"
  ]

jsLiterals :: [String]
jsLiterals =
  [ "nil"
  , "true"
  , "false"
  ]
