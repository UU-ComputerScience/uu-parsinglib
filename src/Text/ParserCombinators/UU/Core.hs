{-# LANGUAGE  RankNTypes, 
              GADTs,
              MultiParamTypeClasses,
              FunctionalDependencies,
              FlexibleInstances,  
              KindSignatures,
              CPP #-}
-- | The module `Core` contains the basic functionality of the parser library.
--   It defines the types and implementations of the elementary  parsers and  recognisers involved.  

module Text.ParserCombinators.UU.Core 
  ( -- * Classes
    IsParser,
    ExtAlternative (..),
--    Provides (..),
    Eof (..),
    IsLocationUpdatedBy (..),
    StoresErrors (..),
    HasPosition (..),
    -- * Types
    -- ** The parser descriptor
    P (..),
    -- ** The progress information
    Steps (..),
    Cost,
    Progress,
    -- ** Auxiliary types
    Nat (..),
    Strings,
    -- * Functions
    -- ** Basic Parsers
    micro,
    amb,
    pErrors,
    pPos,
    pState,
    pEnd,
    pSwitch,
    pSymExt,
--     pSym,
    -- ** Calling Parsers
    parse, parse_h,
    -- ** Acessing and updating various components    
    getZeroP,
    getOneP,
    addLength,
    -- ** Evaluating the online result
    eval,
    -- ** Re-exported modules
    module Control.Applicative,
    module Control.Monad
  ) where

import Control.Applicative
import Control.Monad 
import Data.Char
-- import Debug.Trace
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 710
import Prelude hiding ((.), traverse)
#else
import Prelude hiding ((.))
#endif
import Data.Maybe

f . g = \x ->  f ( g x)

{-# INLINE (.) #-}

-- | In the class `IsParser` we assemble the basic properties we expect parsers to have. The class itself does not have any methods. 
--   Most properties  come directly from the standard 
--   "Control.Applicative" module. The class `ExtAlternative` contains some extra methods we expect our parsers to have.

class (Alternative p, Applicative p, ExtAlternative p) => IsParser p

instance  MonadPlus (P st) where
  mzero = empty
  mplus = (<|>) 

class (Alternative p) => ExtAlternative p where
   -- | `<<|>` is the greedy version of `<|>`. If its left hand side parser can
   --   make any progress then it commits to that alternative. Can be used to make
   --   parsers faster, and even get a complete Parsec equivalent behaviour, with
   --   all its (dis)advantages. Intended use @p \<\<\|> q \<\<\|> r \<\|> x \<\|> y \<?> \"string\"@. Use with care!   
   (<<|>)  :: p a -> p a -> p a
   -- | The parsers build a list of symbols which are expected at a specific point. 
   --   This list is used to report errors.
   --   Quite often it is more informative to get e.g. the name of the non-terminal . 
   --   The `<?>` combinator replaces this list of symbols by the string argument.   
   (<?>)   :: p a -> String -> p a
   -- |  `must_be_non_empty` checks whether its second argument
   --    is a parser which can recognise the empty input. If so, an error message is
   --    given using the  String parameter. If not, then the third argument is
   --    returned. This is useful in testing for illogical combinations. For its use see
   --    the module "Text.ParserCombinators.UU.Derived".
   must_be_non_empty   :: String -> p a ->        c -> c
   --
   -- |  `must_be_non_empties` is similar to `must_be_non_empty`, but can be 
   --    used in situations where we recognise a sequence of elements separated by 
   --    other elements. This does not make sense if both parsers can recognise the 
   --    empty string. Your grammar is then highly ambiguous.
   must_be_non_empties :: String -> p a -> p b -> c -> c 
   -- | If 'p' can be recognized, the return value of 'p' is used. Otherwise,
   --   the value 'v' is used. Note that `opt` by default is greedy. If you do not want
   --   this use @...\<\|> pure v@  instead. Furthermore, 'p' should not
   --   recognise the empty string, since this would make the parser ambiguous!!
   opt     :: p a ->   a -> p a
   opt p v = must_be_non_empty "opt" p (p <<|> pure v)   

infix   2  <?>    
infixl  3  <<|>     
infixl  2 `opt`

-- | The class `Eof` contains a function `eof` which is used to check whether we have reached the end of the input and `deletAtEnd` 
--   should discard any unconsumed input at the end of a successful parse.
class Eof state where
       eof          ::  state   -> Bool
       deleteAtEnd  ::  state   -> Maybe (Cost, state)

-- | The input state may maintain a location which can be used in generating error messages. 
--   Since we do not want to fix our input to be just a @String@ we provide an interface
--   which can be used to advance this location by passing  information about the part recognised. This function is typically
--   called in the `splitState` functions.

class Show loc => loc `IsLocationUpdatedBy` str where
    advance :: loc -- ^ The current position
            -> str -- ^ The part which has been removed from the input
            -> loc

-- | The class `StoresErrors` is used by the function `pErrors` which retrieves the generated 
--  correction steps since the last time it was called.
--

class state `StoresErrors`  error | state -> error where
  -- | `getErrors` retrieves the correcting steps made since the last time the function was called. The result can, 
  --    by using it in a monad, be used to control how to proceed with the parsing process.
  getErrors :: state -> ([error], state)

class state `HasPosition`  pos | state -> pos where
  -- | `getPos` retrieves the correcting steps made since the last time the function was called. The result can, 
  --   by using it as the left hand side of a monadic bind, be used to control how to proceed with the parsing process.
  getPos  ::  state -> pos

-- | The data type `T` contains three components, all being some form of primitive parser. 
--   These components are used in various combinations,
--   depending on whether you are in the right and side operand of a monad, 
--   whether you are interested in a result (if not, we use recognisers), 
--   and whether you want to have the results in an online way (future parsers), or just prefer to be a bit faster (history parsers)

data T st a  = T  (forall r . (a  -> st -> Steps r)  -> st -> Steps       r  )  --   history parser
                  (forall r . (      st -> Steps r)  -> st -> Steps   (a, r) )  --   future parser
                  (forall r . (      st -> Steps r)  -> st -> Steps       r  )  --   recogniser 

instance Functor (T st) where
  fmap f (T ph pf pr) = T  ( \  k -> ph ( k .f ))
                           ( \  k ->  apply2fst f . pf k) -- pure f <*> pf
                           pr
  f <$ (T _ _ pr)     = T  ( pr . ($f)) 
                           ( \ k st -> push f ( pr k st)) 
                           pr

instance   Applicative (T  state) where
  T ph pf pr  <*> ~(T qh qf qr)  =  T ( \  k -> ph (\ pr -> qh (\ qr -> k (pr qr))))
                                      ((apply .) . (pf .qf))
                                      ( pr . qr)
  T ph pf pr  <*  ~(T _  _  qr)   = T ( ph. (qr.))  (pf. qr)   (pr . qr)
  T _  _  pr  *>  ~(T qh qf qr )  = T ( pr . qh  )  (pr. qf)    (pr . qr)            
  pure a                          = T ($a) ((push a).) id 

instance   Alternative (T  state) where 
  T ph pf pr  <|> T qh qf qr  =   T (\  k inp  -> ph k inp `best` qh k inp)
                                    (\  k inp  -> pf k inp `best` qf k inp)
                                    (\  k inp  -> pr k inp `best` qr k inp)
  empty                =  T  ( \  k inp  ->  noAlts) ( \  k inp  ->  noAlts) ( \  k inp  ->  noAlts)


data  P   st  a =  P  (T  st a)         --   actual parsers
                      (Maybe (T st a))  --   non-empty parsers; Nothing if  they are absent
                      (Maybe a)         --   the possibly  empty alternative with its value 
                      Nat               --   minimal number of symbols accepted by  the non-empty part


instance Show (P st a) where
  show (P _ nt e n) = "P _ " ++ maybe "Nothing" (const "(Just _)") nt  ++ maybe "Nothing" (const "(Just _)") e ++ " (" ++ show n ++ ") "

-- | `getOneP` retrieves the non-zero part from a descriptor.
getOneP :: P a b -> Maybe (P a b)
-- getOneP (P _ (Just _)  (Zero Unspecified) _  )  =  error "The element is a special parser which cannot be combined"
getOneP (P _ Nothing  _  l)  =  Nothing
getOneP (P _ onep     ep l)  =  Just( mkParser onep Nothing  l)

-- | `getZeroP` retrieves the possibly empty part from a descriptor.
getZeroP :: P t a -> Maybe a
getZeroP (P _ _ z _)  =  z

-- | `mkParser` combines the non-empty descriptor part and the empty descriptor part into a descriptor tupled with the parser triple
mkParser :: Maybe (T st a) -> Maybe a -> Nat -> P st a
mkParser np ne  l  =  P (mkParser'  np ne)  np  ne l
  where  mkParser' np@(Just nt)  ne@Nothing    =  nt               
         mkParser' np@Nothing    ne@(Just a)   =  pure a       
         mkParser' np@(Just nt)  ne@(Just a)   =  pure a <|> nt
         mkParser' np@(Nothing)  ne@(Nothing)  =  empty

-- ! `combine` creates the non-empty parser 
combine :: (Alternative f) => Maybe t1 -> Maybe t2 -> t -> Maybe t3
        -> (t1 -> t -> f a) -> (t2 -> t3 -> f a) -> Maybe (f a)
combine Nothing   Nothing  _  _     _   _   = Nothing      -- this Parser always fails
combine (Just p)  Nothing  aq _     op1 op2 = Just (p `op1` aq) 
combine (Just p)  (Just v) aq nq    op1 op2 = Just (case nq of
                                                   Just nnq -> p `op1` aq <|> v `op2` nnq
                                                   Nothing  -> p `op1` aq  -- rhs contribution is just from empty alt
                                                   )
combine Nothing   (Just v) _  nq    _   op2 = case nq of
                                              Just nnq -> Just (v `op2` nnq)  -- right hand side has non-empty part
                                              Nothing  -> Nothing             -- neither side has non-empty part

instance   Functor (P  state) where 
  fmap f   (P  ap np me l)   =  P (fmap f ap) (fmap (fmap f)  np)  (f <$> me)  l 
  f <$     (P  ap np me l)   =  P (f <$ ap)   (fmap (f <$)    np)  (f <$  me)  l 

instance   Applicative (P  state) where
  P ap np pe pl  <*> ~(P aq nq  qe ql)  = trace'' "<*>"  (mkParser (combine np pe aq nq (<*>) (<$>))       (pe <*> qe)  (nat_add pl ql))
  P ap np pe pl  <*  ~(P aq nq  qe ql)  = trace'' "<* "  (mkParser (combine np pe aq nq (<*)  (<$))        (pe <* qe )  (nat_add pl ql))
  P ap np pe pl  *>  ~(P aq nq  qe ql)  = trace'' " *>"  (mkParser (combine np pe aq nq (*>) (flip const)) (pe *> qe )  (nat_add pl ql)) 
  pure a                                = trace'' "pure" (mkParser Nothing (Just a)  Zero)

instance Alternative (P   state) where 
  (P ap np  pe pl) <|> (P aq nq qe ql) 
    =  let pl' = maybe pl (const Zero) pe
           ql' = maybe ql (const Zero) qe
           (rl', b) = trace' "calling natMin from <|>" (nat_min pl' ql' 0)
           (rl, _)  = nat_min pl ql 0
           Nothing `alt` q  = q
           p       `alt` Nothing = p
           Just p  `alt` Just q  = Just (p <|> q)
       in  mkParser ((if b then  id  else flip) alt np nq) (pe <|> qe) rl
  empty  = mkParser empty empty  Infinite 

instance ExtAlternative (P st) where
  ~(P ap np pe pl) <<|> ~(P aq nq qe ql) 
    = let pl' = maybe pl (const Zero) pe
          ql' = maybe ql (const Zero) qe
          (rl', b) = nat_min pl' ql' 0
          (rl, _)  = nat_min  pl  ql  0
          bestx :: Steps a -> Steps a -> Steps a
          bestx = (if b then id else flip) best
          choose:: T st a -> T st a -> T st a
          choose  (T ph pf pr)  (T qh qf qr) 
             = T  (\ k st -> let left  = norm (ph k st)
                             in if has_success left then left else left `bestx` qh k st)
                  (\ k st -> let left  = norm (pf k st)
                             in if has_success left then left else left `bestx` qf k st) 
                  (\ k st -> let left  = norm (pr k st)
                             in if has_success left then left else left  `bestx` qr k st)
      in   P (choose  ap aq )
             (maybe np (\nqq -> maybe nq (\npp -> return( choose  npp nqq)) np) nq)
             (pe <|> qe) -- due to the way Maybe is instance of Alternative  the left hand operator gets priority
             rl
  P  _  np  pe pl <?> label = let replaceExpected :: Steps a -> Steps a
                                  replaceExpected (Fail _ c) = (Fail [label] c)
                                  replaceExpected others     = others
                                  nnp = case np of Nothing -> Nothing
                                                   Just ((T ph pf  pr)) -> Just(T ( \ k inp -> replaceExpected (norm  ( ph k inp)))
                                                                                  ( \ k inp -> replaceExpected (norm  ( pf k inp)))
                                                                                  ( \ k inp -> replaceExpected (norm  ( pr k inp))))
                                in mkParser nnp pe pl
  must_be_non_empty msg p@(P _ _ (Just _)  _) _ 
            = error ("The combinator " ++ msg ++  " requires that its argument cannot recognise the empty string\n")
  must_be_non_empty _ _      q  = q
  must_be_non_empties  msg (P _ _ (Just _) _) (P _ _ (Just _) _) _ 
            = error ("The combinator " ++ msg ++  " requires that not both arguments can recognise the empty string\n")
  must_be_non_empties  _ _ _ q  = q

instance IsParser (P st) 

-- !! do not move the P constructor behind choices/patern matches
instance  Monad (P st) where
       p@(P  ap np pe pl ) >>=  a2q = 
          (P newap newnp  newep (nat_add pl Hole))
          where (newep, newnp, newap) = case pe of
                                 Nothing -> (Nothing, t, maybe empty id t) 
                                 Just a  -> let  P aq nq  eq lq = a2q a 
                                            in  (eq, combine t nq , t `alt` aq)
                Nothing  `alt` q    = q
                Just p   `alt` q    = p <|> q
                t = fmap (\  (T h _ _  ) ->      (T  (  \k -> h (\ a -> unParser_h (a2q a) k))
                                                     (  \k -> h (\ a -> unParser_f (a2q a) k))
                                                     (  \k -> h (\ a -> unParser_r (a2q a) k))) ) np
                combine Nothing     Nothing     = Nothing
                combine l@(Just _ ) Nothing     =  l
                combine Nothing     r@(Just _ ) =  r
                combine (Just l)    (Just r)    = Just (l <|> r)
                -- | `unParser_h` retreives the history parser from the descriptor
                unParser_h :: P b a -> (a -> b -> Steps r) -> b -> Steps r
                unParser_h (P (T  h   _  _ ) _ _ _ )  =  h
                -- | `unParser_f` retreives the future parser from the descriptor
                unParser_f :: P b a -> (b -> Steps r) -> b -> Steps (a, r)
                unParser_f (P (T  _   f  _ ) _ _ _ )  =  f
                -- | `unParser_r` retreives therecogniser from the descriptor
                unParser_r :: P b a -> (b -> Steps r) -> b -> Steps r
                unParser_r (P (T  _   _  r ) _ _ _ )  =  r
       return  = pure 

-- |  The basic recognisers are written elsewhere (e.g. in our module "Text.ParserCombinataors.UU.BasicInstances"; 
--    they (i.e. the parameter `splitState`) are lifted to our`P`  descriptors by the function `pSymExt` which also takes
--    the minimal number of tokens recognised by the parameter `splitState`  and an  @Maybe@ value describing the possibly empty value.
pSymExt ::  (forall a. (token -> state  -> Steps a) -> state -> Steps a) -> Nat -> Maybe token -> P state token
pSymExt splitState l e   = mkParser (Just t)  e l
                 where t = T (        splitState                       )
                             ( \ k -> splitState  (\ t -> push t . k)  )
                             ( \ k -> splitState  (\ _ -> k )          )

-- | `micro` inserts a `Cost` step into the sequence representing the progress the parser is making; 
--   for its use see `"Text.ParserCombinators.UU.Demos.Examples"`
micro :: P state a -> Int -> P state a
P _  np  pe pl `micro` i  
  = let nnp = fmap (\ (T ph pf  pr) -> (T ( \ k st -> ph (\ a st -> Micro i (k a st)) st)
                                          ( \ k st -> pf (Micro i .k) st)
                                          ( \ k st -> pr (Micro i .k) st))) np
    in mkParser nnp pe pl

-- |  For the precise functioning of the `amb` combinators see the paper cited in the "Text.ParserCombinators.UU.README";
--    it converts an ambiguous parser into a parser which returns a list of all possible recognitions,
amb :: P st a -> P st [a]
amb (P _  np  pe pl) 
 = let  combinevalues  :: Steps [(a,r)] -> Steps ([a],r)
        combinevalues lar  =   Apply (\ lar -> (map fst lar, snd (head lar))) lar
        nnp = case np of
              Nothing -> Nothing
              Just ((T ph pf  pr)) -> Just(T ( \k     ->  removeEnd_h . ph (\ a st' -> End_h ([a], \ as -> k as st') noAlts))
                                             ( \k inp ->  combinevalues . removeEnd_f $ pf (\st -> End_f [k st] noAlts) inp)
                                             ( \k     ->  removeEnd_h . pr (\ st' -> End_h ([undefined], \ _ -> k  st') noAlts)))
        nep = (fmap pure pe)
    in  mkParser nnp nep pl

-- | `pErrors` returns the error messages that were generated since its last call.
pErrors :: StoresErrors st error => P st [error]
pErrors = let nnp = Just (T ( \ k inp -> let (errs, inp') = getErrors inp in k    errs    inp' )
                            ( \ k inp -> let (errs, inp') = getErrors inp in push errs (k inp'))
                            ( \ k inp -> let (errs, inp') = getErrors inp in            k inp' ))
          in mkParser nnp  Nothing Zero

-- | `pPos` returns the current input position.
pPos :: HasPosition st pos => P st pos
pPos =  let nnp = Just ( T ( \ k inp -> let pos = getPos inp in k    pos    inp )
                           ( \ k inp -> let pos = getPos inp in push pos (k inp))
                           ( \ k inp ->                                   k inp ))
        in mkParser nnp Nothing Zero

-- | `pState` returns the current input state
pState :: P st st
pState =   let nnp = Just ( T ( \ k inp -> k inp inp)
                          ( \ k inp -> push inp (k inp))
                          ($))
           in mkParser nnp Nothing Zero 

-- | The function `pEnd` should be called at the end of the parsing process. It deletes any unconsumed input, turning it into error messages.

pEnd    :: (StoresErrors st error, Eof st) => P st [error]
pEnd    = let nnp = Just ( T ( \ k inp ->   let deleterest inp =  case deleteAtEnd inp of
                                                  Nothing -> let (finalerrors, finalstate) = getErrors inp
                                                             in k  finalerrors finalstate
                                                  Just (i, inp') -> Fail []  [const (i,  deleterest inp')]
                                            in deleterest inp)
                             ( \ k   inp -> let deleterest inp =  case deleteAtEnd inp of
                                                  Nothing -> let (finalerrors, finalstate) = getErrors inp
                                                             in push finalerrors (k finalstate)
                                                  Just (i, inp') -> Fail [] [const ((i, deleterest inp'))]
                                            in deleterest inp)
                             ( \ k   inp -> let deleterest inp =  case deleteAtEnd inp of
                                                  Nothing -> let (finalerrors, finalstate) = getErrors inp
                                                             in  (k finalstate)
                                                  Just (i, inp') -> Fail [] [const (i, deleterest inp')]
                                            in deleterest inp))
         in mkParser nnp  Nothing Zero
           
-- | @`pSwitch`@ takes the current state and modifies it to a different type of state to which its argument parser is applied. 
--   The second component of the result is a function which  converts the remaining state of this parser back into a value of the original type.
--   For the second argument to @`pSwitch`@  (say split) we expect the following to hold:
--   
-- >  let (n,f) = split st in f n == st

pSwitch :: (st1 -> (st2, st2 -> st1)) -> P st2 a -> P st1 a -- we require let (n,f) = split st in f n to be equal to st
pSwitch split (P _ np pe pl)    
   = let nnp = fmap (\ (T ph pf pr) ->T (\ k st1 ->  let (st2, back) = split st1
                                                     in ph (\ a st2' -> k a (back st2')) st2)
                                        (\ k st1 ->  let (st2, back) = split st1
                                                     in pf (\st2' -> k (back st2')) st2)
                                        (\ k st1 ->  let (st2, back) = split st1
                                                     in pr (\st2' -> k (back st2')) st2)) np
     in mkParser nnp pe pl


-- | The function @`parse`@ shows the prototypical way of running a parser on
-- some specific input.
-- By default we use the future parser, since this gives us access to partial
-- result; future parsers are expected to run in less space.
parse :: (Eof t) => P t a -> t -> a
parse   (P (T _  pf _) _ _ _)  state = fst . eval $   pf (\ rest   -> if eof rest then  Done ()
                                                                                   else error "pEnd missing?") state
-- | The function @`parse_h`@ behaves like @`parse`@ but using the history
-- parser. This parser does not give online results, but might run faster.
parse_h :: (Eof t) => P t a -> t -> a
parse_h (P (T ph _  _) _ _ _) state  = eval $  ph  (\ a rest -> if eof rest then  Done a
                                                                      else error "pEnd missing?") state

-- | The data type `Steps` is the core data type around which the parsers are constructed.
--   It describes a tree structure of streams containing (in an interleaved way) both the online result of the parsing process,
--   and progress information. Recognising an input token should correspond to a certain amount of @`Progress`@, 
--   which tells how much of the input state was consumed. 
--   The @`Progress`@ is used to implement the breadth-first search process, in which alternatives are
--   examined in a more-or-less synchronised way. The meaning of the various @`Step`@ constructors is as follows:
--
--   [`Step`] A token was succesfully recognised, and as a result the input was 'advanced' by the distance  @`Progress`@
--
--   [`Apply`] The type of value represented by the `Steps` changes by applying the function parameter.
--
--   [`Fail`] A correcting step has to be made to the input; the first parameter contains information about what was expected in the input, 
--   and the second parameter describes the various corrected alternatives, each with an associated `Cost`
--
--   [`Micro`] A small cost is inserted in the sequence, which is used to disambiguate. Use with care!
--
--   The last two alternatives play a role in recognising ambigous non-terminals. For a full description see the technical report referred to from 
--   "Text.ParserCombinators.UU.README".



data  Steps :: * -> *  where
      Step   ::                 Progress       ->  Steps a                             -> Steps   a
      Apply  ::  forall a b.    (b -> a)       ->  Steps   b                           -> Steps   a
      Fail   ::                 Strings        ->  [Strings   ->  (Cost , Steps   a)]  -> Steps   a
      Micro  ::                 Int            ->  Steps a                             -> Steps   a
      Done   ::                 a                                                      -> Steps   a
      End_h  ::                 ([a] , [a]     ->  Steps r)    ->  Steps   (a,r)       -> Steps   (a, r)
      End_f  ::                 [Steps   a]    ->  Steps   a                           -> Steps   a

instance Show (Steps a) where
  show (Step _ _)   = "Step"
  show (Apply _ _)  = "Apply"
  show (Fail _ _)   = "Fail"
  show (Micro _ _)  = "Micro"
  show (Done _)     = "Done"
  show (End_h _ _ ) = "End_h"
  show (End_f _ _ ) = "End_f"

type Cost     = Int
type Progress = Int
type Strings  = [String]

apply       :: Steps (b -> a, (b, r)) -> Steps (a, r)
apply       =  Apply (\(b2a, br) -> let (b, r) = br in (b2a b, r)) 

push        :: v -> Steps   r -> Steps   (v, r)
push v      =  Apply (\ r -> (v, r))

apply2fst   :: (b -> a) -> Steps (b, r) -> Steps (a, r)
apply2fst f = Apply (\ br -> let (b, r) = br in (f b, r)) 

noAlts :: Steps a
noAlts      =  Fail [] []

has_success :: Steps t -> Bool
has_success (Step _ _) = True
has_success (Done _)   = True
has_success _          = False 

-- | @`eval`@ removes the progress information from a sequence of steps, 
--   and constructs the value embedded in it.
--   If you are really desparate to see how your parsers are making progress
--   (e.g. when you have written an ambiguous parser, and you cannot find 
--   the cause of the exponential blow-up of your parsing process), 
--   you may switch on the trace in the function @`eval`@ (you will need to edit the library source code).
-- 
eval :: Steps   a      ->  a
eval (Step  n    l)     =   trace' ("Step " ++ show n ++ "\n") (eval l)
eval (Micro  _    l)    =   eval l
eval (Fail   ss  ls  )  =   trace' ("expecting: " ++ show ss) (eval (getCheapest 5 (map ($ss) ls))) 
eval (Apply  f   l   )  =   f (eval l)
eval (End_f   _  _   )  =   error "dangling End_f constructor"
eval (End_h   _  _   )  =   error "dangling End_h constructor"
eval (Done  a        )  =   a

-- | `norm` makes sure that the head of the sequence contains progress information. 
--   It does so by pushing information about the result (i.e. the `Apply` steps) backwards.
--
norm ::  Steps a ->  Steps   a
norm     (Apply f (Step   p    l  ))   =   Step  p (Apply f l)
norm     (Apply f (Micro  c    l  ))   =   Micro c (Apply f l)
norm     (Apply f (Fail   ss   ls ))   =   Fail ss (applyFail (Apply f) ls)
norm     (Apply f (Apply  g    l  ))   =   norm (Apply (f.g) l)
norm     (Apply f (End_f  ss   l  ))   =   End_f (map (Apply f) ss) (Apply f l)
norm     (Apply f (End_h  _    _  ))   =   error "Apply before End_h"
norm     (Apply f (Done  a        ))   =   Done (f a)
norm     steps                         =   steps

applyFail :: (c -> d) -> [a -> (b, c)] -> [a -> (b, d)]
applyFail f  = map (\ g -> \ ex -> let (c, l) =  g ex in  (c, f l))

-- | The function @best@ compares two streams
best :: Steps a -> Steps a -> Steps a
x `best` y =   norm x `best'` norm y

best' :: Steps   b -> Steps   b -> Steps   b
(Done  _)               `best'`   (Done  _)           =   error "ambiguous parsers"
l@(Done _)              `best'`   r                   =   l
l                       `best'`   r@(Done _)          =   r
End_f  as  l            `best'`  End_f  bs r          =   End_f (as++bs)  (l `best` r)
End_f  as  l            `best'`  r                    =   End_f as        (l `best` r)
l                       `best'`  End_f  bs r          =   End_f bs        (l `best` r)
End_h  (as, k_h_st)  l  `best'`  End_h  (bs, _) r     =   End_h (as++bs, k_h_st)  (l `best` r)
End_h  as  l            `best'`  r                    =   End_h as (l `best` r)
l                       `best'`  End_h  bs r          =   End_h bs (l `best` r)
Fail  sl  ll     `best'`  Fail  sr rr     =   Fail (sl ++ sr) (ll++rr)
Fail  _   _      `best'`  r               =   r   -- <----------------------------- to be refined
l                `best'`  Fail  _  _      =   l
Step  n   l      `best'`  Step  m  r
    | n == m                              =   Step n (l  `best` r)     
    | n < m                               =   Step n (l  `best`  Step (m - n)  r)
    | n > m                               =   Step m (Step (n - m)  l  `best` r)
ls@(Step _  _)    `best'`  Micro _ _        =  ls
Micro _    _      `best'`  rs@(Step  _ _)   =  rs
ls@(Micro i l)    `best'`  rs@(Micro j r)  
    | i == j                               =   Micro i (l `best` r)
    | i < j                                =   ls
    | i > j                                =   rs
l                       `best'`  r         =   error ("missing alternative in best': " ++ show l ++ " " ++ show r) 

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% getCheapest  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getCheapest :: Int -> [(Int, Steps a)] -> Steps a 
getCheapest _ [] = error "no correcting alternative found"
getCheapest n l  =  snd $  foldr (\(w,ll) btf@(c, l)
                               ->    if w < c   -- c is the best cost estimate thus far, and w total costs on this path
                                     then let new = (traverse n ll w c) 
                                          in if new < c then (new, ll) else btf
                                     else btf 
                               )   (maxBound, error "getCheapest") l


traverse :: Int -> Steps a -> Int -> Int  -> Int 
traverse 0  _            v c  =  trace' ("traverse " ++ show' 0 v c ++ " choosing" ++ show v ++ "\n") v
traverse n (Step _   l)  v c  =  trace' ("traverse Step   " ++ show' n v c ++ "\n") (traverse (n -  1 ) l (v - n) c)
traverse n (Micro x  l)  v c  =  trace' ("traverse Micro  " ++ show' n v c ++ "\n") (traverse n         l v c)
traverse n (Apply _  l)  v c  =  trace' ("traverse Apply  " ++ show n ++ "\n")      (traverse n         l  v      c)
traverse n (Fail m m2ls) v c  =  trace' ("traverse Fail   " ++ show m ++ show' n v c ++ "\n") 
                                 (foldr (\ (w,l) c' -> if v + w < c' then traverse (n -  1 ) l (v+w) c'
                                                       else c') c (map ($m) m2ls)
                                 )
traverse n (End_h ((a, lf))    r)  v c =  traverse n (lf a `best` removeEnd_h r) v c
traverse n (End_f (l      :_)  r)  v c =  traverse n (l `best` r) v c
traverse n (Done _               )  v c =  trace' ("traverse at Done" ++ show' 0 v c ++ " choosing" ++ show v ++ "\n") 0

show' :: (Show a, Show b, Show c) => a -> b -> c -> String
show' n v c = "n: " ++ show n ++ " v: " ++ show v ++ " c: " ++ show c


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%% Handling ambiguous paths             %%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

removeEnd_h     :: Steps (a, r) -> Steps r
removeEnd_h (Fail  m ls             )  =   Fail m (applyFail removeEnd_h ls)
removeEnd_h (Step  ps l             )  =   Step  ps (removeEnd_h l)
removeEnd_h (Apply f l              )  =   error "not in history parsers"
removeEnd_h (Micro c l              )  =   Micro c (removeEnd_h l)
removeEnd_h (End_h  (as, k_st  ) r  )  =   k_st as `best` removeEnd_h r 
removeEnd_h (Done  _)                  =   error "spurious End_h at Done"

removeEnd_f      :: Steps r -> Steps [r]
removeEnd_f (Fail m ls)        =   Fail m (applyFail removeEnd_f ls)
removeEnd_f (Step ps l)        =   Step ps (removeEnd_f l)
removeEnd_f (Apply f l)        =   Apply (map' f) (removeEnd_f l) 
                                   where map' f ~(x:xs)  =  f x : map f xs
removeEnd_f (Micro c l      )  =   Micro c (removeEnd_f l)
removeEnd_f (End_f(s:ss) r)    =   Apply  (:(map  eval ss)) s 
                                                 `best`
                                          removeEnd_f r
removeEnd_f (Done  _)          =    error "spurious End_f at Done"  

-- ** The type @`Nat`@ for describing the minimal number of tokens consumed
-- | The data type @`Nat`@ is used to represent the minimal length of a parser.
--   Care should be taken in order to not evaluate the right hand side of the binary function @`nat-add`@ more than necesssary.

data Nat = Zero 
         | Succ Nat
         | Infinite
         | Unspecified
         | Hole
         deriving  Show

{-
-- | `getlength` retrieves the length of the non-empty part of a parser
getLength :: Nat -> Nat
getLength (Zero  l)    = l
getLength l            = l
-}

addLength n  (P t nep e l) = P t nep e (addLength' n l)  
addLength' :: Int -> Nat -> Nat
addLength' n Zero            = fromInt n
addLength' n (Succ m)        = Succ (addLength' n m)
addLength' n Infinite        = Infinite
addLength' n Unspecified     = Unspecified
addLength' n Hole            = fromInt n

fromInt n = if n>= 0 then (n `times` Succ) Zero else error "error: negative argument passed to addlength"
            where times :: Int -> (Nat -> Nat) -> Nat -> Nat
                  times 0 _ v = v
                  times n f v = times (n-1) f (f v)

-- | `nat_min` compares two minmal length and returns the shorter length. The second component indicates whether the left
--   operand is the smaller one; we cannot use @Either@ since the first component may already be inspected 
--   before we know which operand is finally chosen
nat_min :: Nat -> Nat -> Int -> ( Nat  --  the actual minimum length
                                , Bool --  whether alternatives should be swapped
                                ) 
nat_min Zero       Zero          n  = trace' "Both Zero in nat_min\n" (Zero , False) 
nat_min l          rr@Zero       n  = trace' "Right Zero in nat_min\n"  (Zero , True)
nat_min ll@Zero    r             n  = trace' "Left Zero in nat_min\n"   (Zero, False)
nat_min (Succ ll)  (Succ rr)     n  = if n > 1000 then error "problem with comparing lengths" 
                                      else trace' ("Succ in nat_min " ++ show n ++ "\n")         
                                                  (let (v, b) = nat_min ll  rr (n+1) in (Succ v, b))
nat_min Infinite      r           _  = trace' "Left Infinite in nat_min\n"  (r, True) 
nat_min l             Infinite    _  = trace' "Right Infinite in nat_min\n" (l, False) 
nat_min  Hole         r           _  = error "canot compute minmal length of a parser due to occurrence of a moadic bind, use addLength to override"
nat_min  l            Hole        _  = error "canot compute minmal length of a parser due to occurrence of a moadic bind, use addLength to override"
nat_min  l            Unspecified _  = (l          , False)
nat_min  Unspecified  r           _  = (r          , False)


nat_add :: Nat -> Nat -> Nat
nat_add Zero            r           = trace' "Zero in add\n"        r
nat_add (Succ l)        r           = trace' "Succ in add\n"        (Succ (nat_add l r))
nat_add Infinite        _           = trace' "Infinite in add\n"    Infinite
nat_add Hole            _           = Hole
nat_add Unspecified     r           = trace' "Unspecified in add\n" Unspecified
 


trace' :: String -> b -> b
trace' m v =   v
-- trace' m v = trace m  v

trace'' :: String -> b -> b
trace'' m v =   v
--trace'' m v = trace m  v  

