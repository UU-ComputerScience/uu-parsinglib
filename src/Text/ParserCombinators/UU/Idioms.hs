{-# LANGUAGE RankNTypes,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances,
             FlexibleContexts,
             CPP #-}

#define DEMO(p,i) demo "p" i p

module Text.ParserCombinators.UU.Idioms where

import Text.ParserCombinators.UU
import  Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.Demo.Examples hiding (show_demos)
import qualified Data.ListLike as LL
import Control.Applicative 

data IF = IF
data THEN = THEN
data ELSE = ELSE
data FI = FI
data OR = OR


data String' = String' {fromStr :: String}

-- | The  `Ii` is to be pronounced as @stop@
data Ii = Ii 

-- | The function `iI` is to be pronounced as @start@
iI ::Idiomatic  i (a -> a) g => g
iI = idiomatic (pure id)

class Idiomatic st f g  | g -> f st  where
    idiomatic :: P st f -> g
instance  Idiomatic st x  (Ii -> P st x) where
    idiomatic ix Ii = ix
{-
instance Idiomatic st (a->a) g   => Idiomatic st g OR  where
    idiomatic ix OR = (ix <|>) . idiomatic (pure id)
-}

instance  Idiomatic st f g  => Idiomatic  st (a -> f) (P  st a -> g) where
    idiomatic isf is = idiomatic (isf <*> is)



instance Idiomatic st f g => Idiomatic st ((a -> b) -> f)  ((a -> b) -> g) where
    idiomatic isf f = idiomatic (isf <*> (pure f))
instance (Idiomatic  (Str Char state loc) f g, IsLocationUpdatedBy loc Char, LL.ListLike state Char) 
       => Idiomatic  (Str Char state loc) f (String -> g) where
    idiomatic isf str = idiomatic (isf <* lexeme (pToken str))
instance  (Idiomatic (Str Char state loc) f g, IsLocationUpdatedBy loc Char, LL.ListLike state Char) 
      =>   Idiomatic (Str Char state loc) f (Char -> g) where
    idiomatic isf c = idiomatic (isf <* lexeme (pSym c))
instance Idiomatic st f g =>    Idiomatic st (a -> f) (IF -> Bool -> THEN -> P st a -> ELSE -> P st a -> FI -> g) where
    idiomatic isf IF b THEN t ELSE e FI = idiomatic (isf <*> (if b then t else e))
 

-- | The idea of the Idiom concept is that  sequential composition operators can be inferred from the type 
--   of the various operands
--
-- >>> run (iI (+) '(' pNatural "plus"  pNatural ')' Ii) "(2 plus 3"
--   Result: 5
--    Correcting steps: 
--      Inserted  ')' at position LineColPos 0 4 4 expecting one of [')', Whitespace, '0'..'9']
--
pNat :: Parser Int
pNat = pNatural

show_demos :: IO ()
show_demos =  demo  "(+) <$> (iI (+) '(' pNat \"plus\" IF True THEN pNat ELSE pNat FI ')' Ii)  <* lexeme (pSym '+') <*>  pNat)" "(2 plus 3) + 8"  
                    ((+) <$> (iI (+) '(' pNat  "plus"  IF True THEN pNat ELSE pNat FI ')' Ii) <* lexeme (pSym '+') <*>  pNat)
