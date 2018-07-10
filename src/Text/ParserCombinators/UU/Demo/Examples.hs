{-# OPTIONS_HADDOCK  ignore-exports #-}
{-# LANGUAGE  FlexibleInstances,
              TypeSynonymInstances,
              MultiParamTypeClasses,
              Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
              CPP  #-}

-- | This module contains a lot of examples of the typical use of our parser combinator library. 
--   We strongly encourage you to take a look at the source code.
--   At the end you find a @`main`@ function which demonstrates the main characteristics. 
--   Only the @`run`@ function is exported since it may come in handy elsewhere.

module Text.ParserCombinators.UU.Demo.Examples  where
import Data.Char
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import System.IO
import GHC.IO.Handle.Types
import qualified Data.ListLike as LL

-- import Control.Monad

#define DEMO(p,i) demo "p" i p

type Parser a = P (Str Char String LineColPos) a

justamessage = "justamessage"

-- | Running the function `show_demos` should give the following output:
--
-- >>>   run pa  "a"
--  Result: "a"
-- 
-- >>>   run pa  ""
--  Result: "a"
--  Correcting steps: 
--    Inserted  'a' at position LineColPos 0 0 0 expecting 'a'
-- 
-- >>>   run pa  "b"
--  Result: "a"
--  Correcting steps: 
--    Deleted   'b' at position LineColPos 0 0 0 expecting 'a'
--    Inserted  'a' at position LineColPos 0 1 1 expecting 'a'
-- 
-- >>>   run ((++) <$> pa <*> pa)  "bbab"
--  Result: "aa"
--  Correcting steps: 
--    Deleted   'b' at position LineColPos 0 0 0 expecting 'a'
--    Deleted   'b' at position LineColPos 0 1 1 expecting 'a'
--    Deleted   'b' at position LineColPos 0 3 3 expecting 'a'
--    Inserted  'a' at position LineColPos 0 4 4 expecting 'a'
-- 
-- >>>   run pa  "ba"
--  Result: "a"
--  Correcting steps: 
--    Deleted   'b' at position LineColPos 0 0 0 expecting 'a'
-- 
-- >>>   run pa  "aa"
--  Result: "a"
--  Correcting steps: 
--    The token 'a' was not consumed by the parsing process.
-- 
-- >>>   run (pCount pa :: Parser Int)  "aaa"
--  Result: 3
-- 
-- >>>   run (do  {l <- pCount pa; pExact l pb})  "aaacabbbbb"
--  Result: ["b","b","b","b"]
--  Correcting steps: 
--    Deleted   'c' at position LineColPos 0 3 3 expecting one of ['b', 'a']
--    The token 'b' was not consumed by the parsing process.
-- 
-- >>>   run (amb ( (++) <$> pa2 <*> pa3 <|> (++) <$> pa3 <*> pa2))  "aaaaa"
--  Result: ["aaaaa","aaaaa"]
-- 
-- >>>   run (pList pLower)  "doaitse"
--  Result: "doaitse"
-- 
-- >>>   run paz  "abc2ez"
--  Result: "abcez"
--  Correcting steps: 
--    Deleted   '2' at position LineColPos 0 3 3 expecting 'a'..'z'
-- 
-- >>>   run (max <$> pParens ((+1) <$> wfp) <*> wfp `opt` 0)  "((()))()(())"
--  Result: 3
-- 
-- >>>   run (pa <|> pb <?> justamessage)  "c"
--  Result: "b"
--  Correcting steps: 
--    Deleted   'c' at position LineColPos 0 0 0 expecting justamessage
--    Inserted  'b' at position LineColPos 0 1 1 expecting 'b'
-- 
-- >>>   run (amb (pEither  parseIntString  pIntList))  "(123;456;789)"
--  Result: [Left ["123","456","789"],Right [123,456,789]]
-- 
show_demos :: IO ()
show_demos = 
       do DEMO(pa,  "a")
          DEMO(pa,  "" )
          DEMO(pa,  "b")
          DEMO(((++) <$> pa <*> pa), "bbab")
          DEMO(pa,  "ba")
          DEMO(pa,  "aa")
          DEMO((pCount pa :: Parser Int),                                 "aaa")
          DEMO((do  {l <- pCount pa; pExact l pb}),                       "aaacabbbbb")
          DEMO((amb ( (++) <$> pa2 <*> pa3 <|> (++) <$> pa3 <*> pa2)),    "aaaaa")
          DEMO((pList pLower),                                            "doaitse")
          DEMO(paz,                                                       "abc2ez")
          DEMO((max <$> pParens ((+1) <$> wfp) <*> wfp `opt` 0),          "((()))()(())")
          DEMO((pa <|> pb <?> justamessage),                              "c")
          DEMO((amb (pEither  parseIntString  pIntList)),                 "(123;456;789)")
--          DEMO((pa *> pMunch ( `elem` "^=*") <* pb),                      "a^=^**^^b")

-- | The fuction @`run`@ runs the parser and shows both the result, and the correcting steps which were taken during the parsing process.
run :: Show t =>  Parser t -> String -> IO ()
run p inp = do  let r@(a, errors) =  parse ( (,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
                putStrLn ("--  Result: " ++ show a)
                if null errors then  return ()
                               else  do putStr ("--  Correcting steps: \n")
                                        show_errors errors
                putStrLn "-- "
             where show_errors :: (Show a) => [a] -> IO ()
                   show_errors = sequence_ . (map (putStrLn . show))
run' p inp = do let r@(a, errors) =  parse ( (,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
                if null errors then  return ()
                      else  do putStr ("--  Correcting steps: \n")
                               show_errors errors               
                putStrLn ("--  Result: " ++ show a)                
                putStrLn "-- "
             where show_errors :: (Show a) => [a] -> IO ()
                   show_errors = sequence_ . (map (putStrLn . show))

-- | Our first two parsers are simple; one recognises a single 'a' character and the other one a single 'b'. Since we will use them later we 
--   convert the recognised character into `String` so they can be easily combined.
pa  ::Parser String 
pa  = lift <$> pSym 'a'
pb  :: Parser String 
pb = lift <$> pSym 'b'
pc  :: Parser String 
pc = lift <$> pSym 'c'
lift a = [a]

(<++>) :: Parser String -> Parser String -> Parser String
p <++> q = (++) <$> p <*> q
pa2 =   pa <++> pa
pa3 =   pa <++> pa2

paz :: Parser String
paz = pList (pSatisfy (\t -> 'a' <= t && t <= 'z') (Insertion "'a'..'z'" 'k' 5)) 

-- | The applicative style makes it very easy to merge recogition and computing a result. 
--   As an example we parse a sequence of nested well formed parentheses pairs and
--   compute the maximum nesting depth with @`wfp`@: 
wfp :: Parser Int
wfp =  max <$> pParens ((+1) <$> wfp) <*> wfp `opt` 0

-- | It is very easy to recognise infix expressions with any number of priorities and operators:
--
-- > operators       = [[('+', (+)), ('-', (-))],  [('*' , (*))], [('^', (^))]]
-- > same_prio  ops  = msum [ op <$ pSym c | (c, op) <- ops]
-- > expr            = foldr pChainl ( pNatural <|> pParens expr) (map same_prio operators) -- 
--
-- which we can call:  
--
-- > run expr "15-3*5+2^5"
--
-- > Result: 32
--
-- Note that also here correction takes place: 
--
-- > run expr "2 + + 3 5"
--
-- > Result: 37
-- > Correcting steps: 
-- >    Deleted  ' ' at position 1 expecting one of ['0'..'9', '^', '*', '-', '+']
-- >    Deleted  ' ' at position 3 expecting one of ['(', '0'..'9']
-- >    Inserted '0' at position 4 expecting '0'..'9'
-- >    Deleted  ' ' at position 5 expecting one of ['(', '0'..'9']
-- >    Deleted  ' ' at position 7 expecting one of ['0'..'9', '^', '*', '-', '+']
-- 


test11 = run expr "15-3*5"
expr :: Parser Int
operators       = [[('+', (+)), ('-', (-))],  [('*' , (*))], [('^', (^))]]
same_prio  ops  = foldr (<|>) empty [ op <$ pSym c | (c, op) <- ops]
expr            = foldr pChainl ( pNatural <|> pParens expr) (map same_prio operators) 


-- | A common case where ambiguity arises is when we e.g. want to recognise identifiers, 
--   but only those which are not keywords. 
--   The combinator `micro` inserts steps with a specfied cost in the result 
--   of the parser which can be used to disambiguate:
--
-- > 
-- > ident ::  Parser String
-- > ident = ((:) <$> pSym ('a','z') <*> pMunch (\x -> 'a' <= x && x <= 'z') `micro` 2) <* spaces
-- > idents = pList1 ident
-- > pKey keyw = pToken keyw `micro` 1 <* spaces
-- > spaces :: Parser String
-- > spaces = pMunch (==' ')
-- > takes_second_alt =   pList ident 
-- >                \<|> (\ c t e -> ["IfThenElse"] ++  c   ++  t  ++  e) 
-- >                    \<$ pKey "if"   <*> pList_ng ident 
-- >                    \<* pKey "then" <*> pList_ng ident
-- >                    \<* pKey "else" <*> pList_ng ident  
--
--  A keyword is followed by a small cost @1@, which makes sure that 
--  identifiers which have a keyword as a prefix win over the keyword. Identifiers are however
--   followed by a cost @2@, with as result that in this case the keyword wins. 
--   Note that a limitation of this approach is that keywords are only recognised as such when expected!
-- 
-- > test13 = run takes_second_alt "if a then if else c"
-- > test14 = run takes_second_alt "ifx a then if else c"
-- 
-- with results for @test13@ and @test14@:
--
-- > Result: ["IfThenElse","a","if","c"]
-- > Result: ["ifx","a","then","if", "else","c"]
-- 

-- | A mistake which is made quite often is to construct  a parser which can recognise a sequence of elements using one of the 
--  derived combinators (say @`pList`@), but where the argument parser can recognise the empty string. 
--  The derived combinators check whether this is the case and terminate the parsing process with an error message:
--
-- > run (pList spaces) ""
-- > Result: *** Exception: The combinator pList
-- >  requires that it's argument cannot recognise the empty string
--
-- > run (pMaybe spaces) " "
-- > Result: *** Exception: The combinator pMaybe
-- > requires that it's argument cannot recognise the empty string
test16 :: IO ()
test16 = run (pList spaces) "  "

ident = ((:) <$> pRange ('a','z') <*> pMunch (\x -> 'a' <= x && x <= 'z') `micro` 2) <* spaces
idents = pList1 ident

pKey keyw = pToken keyw `micro` 1 <* spaces
spaces :: Parser String
spaces = pMunch (`elem` " \n")
 
takes_second_alt =   pList ident 
              <|> (\ c t e -> ["IfThenElse"] ++  c   ++  t  ++  e) 
                  <$ pKey "if"   <*> pList_ng ident 
                  <* pKey "then" <*> pList_ng ident
                  <* pKey "else" <*> pList_ng ident  
test13 = run takes_second_alt "if a then if else c"
test14 = run takes_second_alt "ifx a then if else c"



pManyTill :: P st a -> P st b -> P st [a]
pManyTill p end = [] <$ end 
                  <<|> 
                  (:) <$> p <*> pManyTill p end
simpleComment   =  string "<!--"  *>  pManyTill pAscii  (string "-->")


string ::(IsLocationUpdatedBy loc Char, LL.ListLike state Char) => String -> P (Str Char state loc)  String
string = pToken


pVarId  = (:) <$> pLower <*> pList pIdChar
pConId  = (:) <$> pUpper <*> pList pIdChar
pIdChar = pLower <|> pUpper <|> pDigit <|> pAnySym "='"

pAnyToken :: (IsLocationUpdatedBy loc Char, LL.ListLike state Char) => [String] -> P (Str Char state loc)  String 
pAnyToken = pAny pToken

-- parsing two alternatives and returning both rsults
pIntList :: Parser [Int]
pIntList       =  pParens ((pSym ';') `pListSep` (read <$> pList1 (pRange ('0', '9'))))
parseIntString :: Parser [String]
parseIntString =  pParens ((pSym ';') `pListSep` (         pList1 (pRange('0', '9'))))




demo :: Show r => String -> String -> P (Str Char String LineColPos) r -> IO ()
demo str  input p= do putStr ("-- >>>   run " ++ str ++ "  " ++ show input ++ "\n")
                      run p input
