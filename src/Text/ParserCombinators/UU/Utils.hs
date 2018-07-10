-- | This module provides some higher-level types and infrastructure to  make it easier to use.

{-# LANGUAGE PatternGuards, ScopedTypeVariables, NoMonomorphismRestriction,FlexibleInstances,  FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
-- {-# LANGUAGE  MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}

module Text.ParserCombinators.UU.Utils (
   -- * Single-char parsers
  pCR,
  pLF,
  pLower,
  pUpper,
  pLetter,
  pAscii,
  pDigit,
  pDigitAsNum,
  pAnySym,

  -- * Whitespace and comments (comments - not yet supported)
  pSpaces, -- This should not be used very often. In general
           -- you may want to use it to skip initial whitespace
           -- at the start of all input, but after that you
           -- should rely on Lexeme parsers to skip whitespace
           -- as needed. (This is the same as the strategy used
           -- by Parsec).

  -- * Lexeme parsers (as opposed to 'Raw' parsers)
  lexeme,
  pDot,
  pComma,
  pDQuote,
  pLParen,
  pRParen,
  pLBracket,
  pRBracket,
  pLBrace,
  pRBrace,
  pSymbol,

  -- * Raw parsers for numbers
  pNaturalRaw,
  pIntegerRaw,
  pDoubleRaw,
  pDoubleStr,

  -- * Lexeme parsers for numbers
  pNatural,
  pInteger,
  pDouble,
  pPercent,

  -- * Parsers for Enums
  pEnumRaw,
  pEnum,
  pEnumStrs,

  -- * Parenthesized parsers
  pParens,
  pBraces,
  pBrackets,
  listParser,
  tupleParser,
  pTuple,

  -- * Lexeme parsers for `Date`-s
  pDay,
  pDayMonthYear,

  -- * Lexeme parser for quoted `String`-s
  pParentheticalString,
  pQuotedString,

  -- * Read-compatability
  parserReadsPrec,
  
  -- * Basic facility for runninga parser, getting at most a single error message
  execParser,
  runParser
)
where

import Data.Char
import Data.List
import Data.Time
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived 
import Text.Printf
import qualified Data.ListLike  as LL

------------------------------------------------------------------------

--  Single Char parsers

pCR :: Parser Char
pCR       = pSym '\r'

pLF :: Parser Char
pLF       = pSym '\n'

pLower :: Parser Char
pLower  = pRange ('a','z')

pUpper :: Parser Char
pUpper  = pRange ('A','Z')

pLetter:: Parser Char
pLetter = pUpper <|> pLower

pAscii :: Parser Char
pAscii = pRange ('\000', '\254')

pDigit :: Parser Char
pDigit  = pRange ('0','9')


pDigitAsNum ::  Num a => Parser a
pDigitAsNum =
  digit2Int <$> pDigit
  where
  digit2Int a = fromInteger $ toInteger $ ord a - ord '0'

pAnySym ::  (IsLocationUpdatedBy loc Char, LL.ListLike state Char) => String -> P (Str Char state loc) Char
pAnySym = pAny pSym

-- * Dealing with Whitespace
pSpaces :: Parser String
pSpaces = pMunch (`elem` " \r\n\t") <?> "Whitespace"

-- | Lexeme Parsers skip trailing whitespace (this terminology comes from Parsec)
lexeme :: ParserTrafo a a
lexeme p = p <* pSpaces

pDot, pComma, pDQuote, pLParen, pRParen, pLBracket, pRBracket, pLBrace, pRBrace :: Parser Char
pDot      = lexeme $ pSym '.'
pComma    = lexeme $ pSym ','
pDQuote   = lexeme $ pSym '"'
pLParen   = lexeme $ pSym '('
pRParen   = lexeme $ pSym ')'
pLBracket = lexeme $ pSym '['
pRBracket = lexeme $ pSym ']'
pLBrace   = lexeme $ pSym '{'
pRBrace   = lexeme $ pSym '}'

pSymbol :: (IsLocationUpdatedBy loc Char, LL.ListLike state Char) => String -> P (Str Char state loc)  String
pSymbol   = lexeme . pToken

-- * Parsers for Numbers
-- ** Raw (non lexeme) parsers
pNaturalRaw :: (Num a) => Parser a
pNaturalRaw = foldl (\a b -> a * 10 + b) 0 <$> pList1 pDigitAsNum <?> "Natural"

pIntegerRaw :: (Num a) => Parser a
pIntegerRaw = pSign <*> pNaturalRaw <?> "Integer"

pDoubleRaw :: (Read a) => Parser a
pDoubleRaw = read <$> pDoubleStr

pDoubleStr :: Parser  [Char]
pDoubleStr = pOptSign <*> (pToken "Infinity" <|> pPlainDouble)
             <?> "Double (eg -3.4e-5)"
  where
    pPlainDouble = (++) <$> ((++) <$> pList1 pDigit <*> (pFraction `opt` [])) <*> pExponent
    pFraction = (:) <$> pSym '.' <*> pList1 pDigit
    pExponent = ((:) <$> pAnySym "eE" <*> (pOptSign <*> pList1 pDigit)) `opt` []
    pOptSign = ((('+':) <$ (pSym '+')) <|> (('-':) <$ (pSym '-'))) `opt` id

-- | NB - At present this is /not/ a lexeme parser, hence we don't
--   support @- 7.0@, @- 7@, @+ 7.0@ etc.
--   It's also currently private - ie local to this module.
pSign :: (Num a) => Parser (a -> a)
pSign = (id <$ (pSym '+')) <|> (negate <$ (pSym '-')) `opt` id

pPercentRaw ::Parser Double
pPercentRaw = (/ 100.0) . read <$> pDoubleStr <* pSym '%' <?> "Double%"

pPctOrDbl = pPercentRaw <|> pDoubleRaw

-- ** Lexeme Parsers for Numbers

pNatural :: Num a => Parser a
pNatural = lexeme pNaturalRaw

pInteger :: Num a => Parser a
pInteger = lexeme pIntegerRaw

pDouble :: Parser Double
pDouble = lexeme pDoubleRaw

pPercent :: Parser Double
pPercent = lexeme pPctOrDbl

-- * Parsers for Enums

pEnumRaw :: forall a . ((Enum a, Show a)=> Parser  a)
pEnumRaw = foldr (\ c r -> c <$ pToken (show c) <|> r) pFail enumerated
           <?> (printf "Enum (eg %s or ... %s)" (show (head enumerated)) (show (last enumerated)))
            -- unless it is an empty data decl we will always have a head/last (even if the same)
            -- if it is empty, you cannot use it anyhow...
  where
    enumerated :: [a]
    enumerated = [toEnum 0..] 
--    pToken :: Provides st s s => [s] -> P st [s]
--    pToken []     = pure []
--    pToken (a:as) = (:) <$> pSym a <*> pToken as

pEnum ::  (Enum a, Show a) => Parser a
pEnum = lexeme pEnumRaw

pEnumStrs :: [String]-> Parser String
pEnumStrs xs = pAny (\t -> pSpaces *> pToken t <* pSpaces) xs <?> "enumerated value in " ++ show xs


-- * Parenthesized structures
pParens :: ParserTrafo a a
pParens p = pLParen *> p <* pRParen

pBraces ::  ParserTrafo a a
pBraces p = pLBrace *> p <* pRBrace

pBrackets ::  ParserTrafo a a
pBrackets p = pLBracket *> p <* pRBracket

-- * Lists and tuples
-- | eg [1,2,3]
listParser :: ParserTrafo a [a]
listParser = pBrackets . pListSep pComma

-- | eg (1,2,3)
tupleParser :: ParserTrafo a [a]
tupleParser = pParens . pListSep pComma

pTuple :: (IsLocationUpdatedBy loc Char, LL.ListLike state Char) => [P (Str Char state loc) a] -> P (Str Char state loc) [a]
pTuple []     = [] <$ pParens pSpaces
pTuple (p:ps) = pParens $ (:) <$> lexeme p <*> mapM ((pComma *>) . lexeme) ps

-- * Lexeme parsers for Dates

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
           deriving (Enum, Bounded, Eq, Show, Ord)

pDayMonthYear :: (Num d, Num y) => Parser (d, Int, y)
pDayMonthYear = lexeme $ (,,) <$> pDayNum <*> (pSym '-' *> pMonthNum) <*> (pSym '-' *> pYearNum)
  where
    pMonthNum = ((+1) . (fromEnum :: Month -> Int)) <$> pEnumRaw <?> "Month (eg Jan)"
    pDayNum   = pNaturalRaw <?> "Day (1-31)"
    pYearNum  = pNaturalRaw <?> "Year (eg 2019)"

pDay :: Parser Day
pDay = (\(d,m,y) -> fromGregorian y m d) <$> pDayMonthYear

-- * Quoted Strings

pParentheticalString :: Char -> Parser String

pParentheticalString d = lexeme $ pSym d *> pList pNonQuoteVChar <* pSym d
  where
    pNonQuoteVChar = pSatisfy (\c -> visibleChar c && c /= d) 
                              (Insertion  "Character in a string set off from main text by delimiter, e.g. double-quotes or comment token" 'y' 5)
    -- visibleChar :: Char -> Bool
    visibleChar c = '\032' <= c && c <= '\126'

pQuotedString :: Parser String
pQuotedString = pParentheticalString '"'


-- * Read-compatability

-- | Converts a UU Parser into a read-style one.
--
-- This is intended to facilitate migration from read-style
-- parsers to UU-based ones.
parserReadsPrec :: Parser a -> Int -> ReadS a
parserReadsPrec p _ s = [parse ((,) <$> p <*> pMunch (const True)) . createStr (0::Int) $ s]



-- * Running parsers straightforwardly

-- | The lower-level interface. Returns all errors. 
execParser :: Parser a -> String -> (a, [Error LineColPos])
execParser p = parse_h ((,) <$> p <*> pEnd) . createStr (LineColPos 0 0 0)

-- | The higher-level interface. (Calls 'error' with a simplified error).  
--   Runs the parser; if the complete input is accepted without problems  return the
--   result else fail with reporting unconsumed tokens
runParser :: String -> Parser a -> String -> a
runParser inputName p s | (a,b) <- execParser p s =
    if null b
    then a
    else error (printf "Failed parsing '%s' :\n%s\n" inputName (pruneError s b))
         -- We do 'pruneError' above because otherwise you can end
         -- up reporting huge correction streams, and that's
         -- generally not helpful... but the pruning does discard info...
    where -- | Produce a single simple, user-friendly error message
          pruneError :: String -> [Error LineColPos] -> String
          pruneError _ [] = ""
          pruneError _ (DeletedAtEnd x     : _) = printf "Unexpected '%s' at end." x
          pruneError s (Inserted _ pos exp : _) = prettyError s exp pos
          pruneError s (Deleted  _ pos exp : _) = prettyError s exp pos
          prettyError :: String -> [String] -> LineColPos -> String
          prettyError s exp p@(LineColPos line c abs) = printf "Expected %s at %s :\n%s\n%s\n%s\n"
                                                           (show_expecting p exp)
                                                           (show p)
                                                           aboveString
                                                           inputFrag
                                                           belowString
                             where
                                s' = map (\c -> if c=='\n' || c=='\r' || c=='\t' then ' ' else c) s
                                aboveString = replicate 30 ' ' ++ "v"
                                belowString = replicate 30 ' ' ++ "^"
                                inputFrag   = replicate (30 - abs) ' ' ++ (take 71 $ drop (abs - 30) s')

