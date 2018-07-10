{-# LANGUAGE ExistentialQuantification,
             FlexibleInstances #-}

-- | This module contains the additional data types, instance definitions and functions to run parsers in an interleaved way.
--   If all the interleaved parsers recognise a single connected piece of the input text this incorporates the permutation parsers.
--   For some examples see the module "Text.ParserCombinators.UU.Demo.MergeAndPermute".

module Text.ParserCombinators.UU.Interleaved where
import Control.Applicative.Interleaved hiding (mkP)
import Text.ParserCombinators.UU.Core

mkP :: Gram (P st) a -> P st a
mkP (Gram ls le) = foldr (\ p pp -> doNotInterpret p <|> pp) (maybe empty pure le) (map mkParserAlt ls)
   where mkParserAlt (p   `Seq`  pp  ) = p <*> mkP pp
         mkParserAlt (fc  `Bind` c2fa) = fc >>=  (mkP . c2fa)

instance Splittable (P st) where
  getPure    = getZeroP
  getNonPure = getOneP

instance Functor f => ExtAlternative (Gram f) where
  p <<|> q                    = p <|> q
  p <?> s                     = error "No <?> defined for Grammars yet. If you need ask for it"
  must_be_non_empty msg (Gram _ (Just _)) _
    = error ("The combinator " ++ msg ++  " requires that it's argument cannot recognise the empty string\n")
  must_be_non_empty _ _  q  = q
  must_be_non_empties  msg (Gram _ (Just _)) (Gram _ (Just _)) _ 
    = error ("The combinator " ++ msg ++  " requires that not both arguments can recognise the empty string\n")
  must_be_non_empties  msg _  _ q = q

-- | `doNotInterpret` forgets the computed minimal number of tokens recognised by this parser
--    which  makes a parser opaque for abstract interpretation; used when interleaving parsers
--    where we do not want to compare lengths.

doNotInterpret :: P st a -> P st a
doNotInterpret (P t nep e _) = P t nep e Unspecified

instance  IsParser (Gram (P st))

