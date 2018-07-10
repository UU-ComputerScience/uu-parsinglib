{-# LANGUAGE  RankNTypes, 
              GADTs,
              MultiParamTypeClasses,
              FunctionalDependencies, 
              FlexibleInstances, 
              FlexibleContexts, 
              UndecidableInstances,
              NoMonomorphismRestriction #-}

-- | This module contains a large variety of combinators for list-like structures. the extension @_ng@ indicates that 
--   that variant is the non-greedy variant.
--   See the "Text.ParserCombinators.UU.Demo.Examples" module for some examples of their use.

module Text.ParserCombinators.UU.Derived where
import Text.ParserCombinators.UU.Core

-- * Some aliases for oft occurring constructs

-- | @`pReturn`@ is defined for upwards compatibility
--
pReturn :: Applicative p => a -> p  a
pReturn  = pure

-- | @`pFail`@ is defined for upwards compatibility, and is the unit for @<|>@
--
pFail :: Alternative  p => p  a
pFail    = empty

-- | `pMaybe` greedily recognises its argument. If not @Nothing@ is returned.
--
pMaybe :: IsParser p => p a -> p (Maybe a)
pMaybe p = must_be_non_empty "pMaybe" p (Just <$> p `opt` Nothing) 

-- | `pEither` recognises either one of its arguments.
--
pEither :: IsParser p => p a -> p b -> p (Either a b)
pEither p q = Left <$> p <|> Right <$> q
                                                
-- | `<$$>` is the version of `<$>` which flips the function argument 
--
(<$$>)    ::  IsParser p => (a -> b -> c) -> p b -> p (a -> c)
f <$$> p  =  flip f <$> p

-- | `<??>` parses an optional postfix element and applies its result to its left hand result
--
(<??>) :: IsParser p => p a -> p (a -> a) -> p a
p <??> q        = must_be_non_empty "<??>" q (p <**> (q `opt` id))


-- | `<.>` functional composition of two parsers
--
(<.>) :: IsParser p => p (b -> c) -> p (a -> b) -> p (a -> c)
f <.> g = (.) <$> f <*> g

-- | `<..>` functional composition of two parsers with the arguments reversed
--
(<..>) :: IsParser p => p (a -> b) -> p (b -> c) -> p (a -> c)
g <..> f = (.) <$> f <*> g


infixl 4  <??>

-- | `pMany` is equivalent to the `many` from "Control.Applicative". We want however all our parsers to start with a lower case @p@.
pMany :: IsParser p => p a -> p [a]
pMany p = pList p

-- | `pSome` is equivalent to the `some` from "Control.Applicative". We want however all our parsers to start with a lower case @p@.
pSome :: (IsParser f) => f a -> f [a]
pSome p = (:) <$> p <*> pList p


-- | @`pPacked`@ surrounds its third parser with the first and the second one, returning only the middle result
pPacked :: IsParser p => p b1 -> p b2 -> p a -> p a
pPacked l r x   =   l *>  x <*   r

-- * Iterating combinators, all in a greedy (default) and a non-greedy (ending with @_ng@) variant

-- ** Recognising  list like structures
pFoldr    :: IsParser p => (a -> a1 -> a1, a1) -> p a -> p a1
pFoldr         alg@(op,e)     p =  must_be_non_empty "pFoldr" p pfm
                                   where pfm = (op <$> p <*> pfm) `opt` e

pFoldr_ng ::  IsParser p => (a -> a1 -> a1, a1) -> p a -> p a1
pFoldr_ng      alg@(op,e)     p =  must_be_non_empty "pFoldr_ng" p pfm 
                                   where pfm = (op <$> p <*> pfm)  <|> pure e


pFoldr1    :: IsParser p => (v -> b -> b, b) -> p v -> p b
pFoldr1        alg@(op,e)     p =  must_be_non_empty "pFoldr1"    p (op <$> p <*> pFoldr     alg p) 

pFoldr1_ng ::  IsParser p => (v -> b -> b, b) -> p v -> p b
pFoldr1_ng     alg@(op,e)     p =  must_be_non_empty "pFoldr1_ng" p (op <$> p <*> pFoldr_ng  alg p)


list_alg :: (a -> [a] -> [a], [a1])
list_alg = ((:), [])

pList    ::    IsParser p => p a -> p [a]
pList         p =  must_be_non_empty "pList"    p (pFoldr        list_alg   p)
pList_ng ::    IsParser p => p a -> p [a]
pList_ng      p =  must_be_non_empty "pList_ng" p (pFoldr_ng     list_alg   p)

pList1    ::  IsParser p =>  p a -> p [a]
pList1         p =  must_be_non_empty "pList"    p (pFoldr1       list_alg   p)
pList1_ng ::   IsParser p => p a -> p [a]
pList1_ng      p =  must_be_non_empty "pList_ng" p (pFoldr1_ng    list_alg   p)

-- * Recognising list structures with separators

pFoldrSep    ::  IsParser p => (v -> b -> b, b) -> p a -> p v -> p b
pFoldrSep      alg@(op,e) sep p =  must_be_non_empties "pFoldrSep" sep   p
                                   (op <$> p <*> pFoldr    alg sepp `opt` e)
                                   where sepp = sep *> p
pFoldrSep_ng ::  IsParser p => (v -> b -> b, b) -> p a -> p v -> p b
pFoldrSep_ng   alg@(op,e) sep p =  must_be_non_empties "pFoldrSep" sep   p
                                   (op <$> p <*> pFoldr_ng alg sepp <|>  pure e)
                                   where sepp = sep *> p

pFoldr1Sep    ::   IsParser p => (a -> b -> b, b) -> p a1 ->p a -> p b
pFoldr1Sep     alg@(op,e) sep p =  must_be_non_empties "pFoldr1Sep"    sep   p pfm
                                   where pfm = op <$> p <*> pFoldr    alg (sep *> p)
pFoldr1Sep_ng ::   IsParser p => (a -> b -> b, b) -> p a1 ->p a -> p b
pFoldr1Sep_ng  alg@(op,e) sep p =  must_be_non_empties "pFoldr1Sep_ng" sep   p pfm 
                                   where pfm = op <$> p <*> pFoldr_ng alg (sep *> p)

pListSep    :: IsParser p => p a1 -> p a -> p [a]
pListSep      sep p = must_be_non_empties "pListSep"    sep   p (pFoldrSep     list_alg sep p)
pListSep_ng :: IsParser p => p a1 -> p a -> p [a]
pListSep_ng   sep p = must_be_non_empties "pListSep_ng" sep   p pFoldrSep_ng  list_alg sep p

pList1Sep    :: IsParser p => p a1 -> p a -> p [a]
pList1Sep     s p =  must_be_non_empties "pListSep"    s   p (pFoldr1Sep    list_alg s p)
pList1Sep_ng :: IsParser p => p a1 -> p a -> p [a]
pList1Sep_ng  s p =  must_be_non_empties "pListSep_ng" s   p (pFoldr1Sep_ng list_alg s p)

-- * Combinators for chained structures
-- ** Treating the operator as right associative
pChainr    :: IsParser p => p (c -> c -> c) -> p c -> p c
pChainr    op x    =   must_be_non_empties "pChainr"    op   x r where r = x <??> (flip <$> op <*> r)
pChainr_ng :: IsParser p => p (c -> c -> c) -> p c -> p c
pChainr_ng op x    =   must_be_non_empties "pChainr_ng" op   x r where r = x <**> ((flip <$> op <*> r)  <|> pure id)

-- ** Treating the operator as left associative
pChainl    :: IsParser p => p (c -> c -> c) -> p c -> p c
pChainl   op x    =  must_be_non_empties "pChainl"    op   x (f <$> x <*> pList (flip <$> op <*> x)) 
                    where  f x [] = x
                           f x (func:rest) = f (func x) rest
pChainl_ng :: IsParser p => p (c -> c -> c) -> p c -> p c
pChainl_ng op x    = must_be_non_empties "pChainl_ng" op   x (f <$> x <*> pList_ng (flip <$> op <*> x))
                     where f x [] = x
                           f x (func:rest) = f (func x) rest

-- * Repeating parsers

-- | `pExact` recognises a specified number of elements
pExact :: (IsParser f) => Int -> f a -> f [a]
pExact n p | n == 0 = pure []
           | n >  0 = (:) <$> p <*> pExact (n-1) p

pBetween :: (IsParser f) => Int -> Int -> f a -> f [a]
pBetween m n p |  n < 0 || m <0 =  error "negative arguments to pBwteeen"
               |  m > n         =  empty
               |  otherwise     =  (++) <$> pExact m p <*> pAtMost (n-m) p

pAtLeast ::  (IsParser f) => Int -> f a -> f [a]
pAtLeast n p  = (++) <$> pExact n p <*> pList p

pAtMost ::  (IsParser f) => Int -> f a -> f [a]
pAtMost n p | n > 0  = (:) <$> p <*> pAtMost (n-1) p `opt`  []
            | n == 0 = pure []

-- * Counting Parser
-- | Count the number of times @p@ has succeeded
pCount :: (IsParser p, Num b) => p a -> p b
pCount p = (\_ b -> b+1) <$> p <*> pCount p  `opt` 0

-- * Miscelleneous 
-- | Build a parser for each element in the argument list and try them all.
pAny :: IsParser p => (a -> p a1) -> [a] -> p a1
pAny  f l =  foldr (<|>) pFail (map f l)

-- | pSym was removed because the class Provides was eliminated
-- pAnySym :: Provides st s s => [s] -> P st s
-- pAnySym = pAny pSym 

