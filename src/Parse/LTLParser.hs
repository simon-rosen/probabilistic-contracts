{-# OPTIONS_GHC -w #-}
module Parse.LTLParser
  ( ltlParser
  , parseLTL
  , ltlProbContractParser
  , parseLTLProbContract
  ) where

import qualified Parse.Lexer as L
import Specs.LTL (Formula (..))
import Contracts.Probabilistic (ProbContract (..))
import qualified Math
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t5 t6 t7
	= HappyTerminal (L.Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,73) ([50816,1,4096,2048,0,29088,0,0,512,0,9088,0,0,50816,1,1818,26624,28,29088,0,0,0,0,0,0,0,50816,1,1818,26624,28,29088,32768,454,58368,8,0,0,654,0,0,2272,0,32,32768,32768,454,58368,8,0,15,16384,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_ltlParser","%start_ltlProbContractParser","Phi","PC","Cmp","'('","')'","id","'!'","'&'","'|'","'->'","'G'","'F'","'X'","'U'","'P'","','","'<'","'<='","'>'","'>='","double","%eof"]
        bit_start = st Prelude.* 26
        bit_end = (st Prelude.+ 1) Prelude.* 26
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..25]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (8) = happyShift action_3
action_0 (10) = happyShift action_7
action_0 (11) = happyShift action_8
action_0 (15) = happyShift action_9
action_0 (16) = happyShift action_10
action_0 (17) = happyShift action_11
action_0 (5) = happyGoto action_6
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (19) = happyShift action_5
action_1 (6) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (8) = happyShift action_3
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (8) = happyShift action_3
action_3 (10) = happyShift action_7
action_3 (11) = happyShift action_8
action_3 (15) = happyShift action_9
action_3 (16) = happyShift action_10
action_3 (17) = happyShift action_11
action_3 (5) = happyGoto action_21
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (26) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (8) = happyShift action_20
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (12) = happyShift action_16
action_6 (13) = happyShift action_17
action_6 (14) = happyShift action_18
action_6 (18) = happyShift action_19
action_6 (26) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_3

action_8 (8) = happyShift action_3
action_8 (10) = happyShift action_7
action_8 (11) = happyShift action_8
action_8 (15) = happyShift action_9
action_8 (16) = happyShift action_10
action_8 (17) = happyShift action_11
action_8 (5) = happyGoto action_15
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (8) = happyShift action_3
action_9 (10) = happyShift action_7
action_9 (11) = happyShift action_8
action_9 (15) = happyShift action_9
action_9 (16) = happyShift action_10
action_9 (17) = happyShift action_11
action_9 (5) = happyGoto action_14
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (8) = happyShift action_3
action_10 (10) = happyShift action_7
action_10 (11) = happyShift action_8
action_10 (15) = happyShift action_9
action_10 (16) = happyShift action_10
action_10 (17) = happyShift action_11
action_10 (5) = happyGoto action_13
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (8) = happyShift action_3
action_11 (10) = happyShift action_7
action_11 (11) = happyShift action_8
action_11 (15) = happyShift action_9
action_11 (16) = happyShift action_10
action_11 (17) = happyShift action_11
action_11 (5) = happyGoto action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_10

action_13 _ = happyReduce_9

action_14 _ = happyReduce_8

action_15 _ = happyReduce_4

action_16 (8) = happyShift action_3
action_16 (10) = happyShift action_7
action_16 (11) = happyShift action_8
action_16 (15) = happyShift action_9
action_16 (16) = happyShift action_10
action_16 (17) = happyShift action_11
action_16 (5) = happyGoto action_27
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (8) = happyShift action_3
action_17 (10) = happyShift action_7
action_17 (11) = happyShift action_8
action_17 (15) = happyShift action_9
action_17 (16) = happyShift action_10
action_17 (17) = happyShift action_11
action_17 (5) = happyGoto action_26
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (8) = happyShift action_3
action_18 (10) = happyShift action_7
action_18 (11) = happyShift action_8
action_18 (15) = happyShift action_9
action_18 (16) = happyShift action_10
action_18 (17) = happyShift action_11
action_18 (5) = happyGoto action_25
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (8) = happyShift action_3
action_19 (10) = happyShift action_7
action_19 (11) = happyShift action_8
action_19 (15) = happyShift action_9
action_19 (16) = happyShift action_10
action_19 (17) = happyShift action_11
action_19 (5) = happyGoto action_24
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (8) = happyShift action_3
action_20 (10) = happyShift action_7
action_20 (11) = happyShift action_8
action_20 (15) = happyShift action_9
action_20 (16) = happyShift action_10
action_20 (17) = happyShift action_11
action_20 (5) = happyGoto action_23
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (9) = happyShift action_22
action_21 (12) = happyShift action_16
action_21 (13) = happyShift action_17
action_21 (14) = happyShift action_18
action_21 (18) = happyShift action_19
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_2

action_23 (12) = happyShift action_16
action_23 (13) = happyShift action_17
action_23 (14) = happyShift action_18
action_23 (18) = happyShift action_19
action_23 (20) = happyShift action_28
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_11

action_25 (12) = happyShift action_16
action_25 (13) = happyShift action_17
action_25 (14) = happyShift action_18
action_25 (18) = happyShift action_19
action_25 _ = happyReduce_7

action_26 (18) = happyShift action_19
action_26 _ = happyReduce_6

action_27 (18) = happyShift action_19
action_27 _ = happyReduce_5

action_28 (8) = happyShift action_3
action_28 (10) = happyShift action_7
action_28 (11) = happyShift action_8
action_28 (15) = happyShift action_9
action_28 (16) = happyShift action_10
action_28 (17) = happyShift action_11
action_28 (5) = happyGoto action_29
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (9) = happyShift action_30
action_29 (12) = happyShift action_16
action_29 (13) = happyShift action_17
action_29 (14) = happyShift action_18
action_29 (18) = happyShift action_19
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (21) = happyShift action_32
action_30 (22) = happyShift action_33
action_30 (23) = happyShift action_34
action_30 (24) = happyShift action_35
action_30 (7) = happyGoto action_31
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (25) = happyShift action_36
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_13

action_33 _ = happyReduce_14

action_34 _ = happyReduce_15

action_35 _ = happyReduce_16

action_36 _ = happyReduce_12

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyTerminal (L.Ident happy_var_1))
	 =  HappyAbsSyn5
		 (Atom happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Not happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (And happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Or happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Implies happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Globally happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Future happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Next happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Until happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 8 6 happyReduction_12
happyReduction_12 ((HappyTerminal (L.DoubleLit happy_var_8)) `HappyStk`
	(HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (ProbContract (Math.Probability (happy_var_3, happy_var_5) happy_var_7 happy_var_8)
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn7
		 (Math.Less
	)

happyReduce_14 = happySpecReduce_1  7 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn7
		 (Math.Leq
	)

happyReduce_15 = happySpecReduce_1  7 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn7
		 (Math.Greater
	)

happyReduce_16 = happySpecReduce_1  7 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn7
		 (Math.Geq
	)

happyNewToken action sts stk [] =
	action 26 26 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.LPar -> cont 8;
	L.RPar -> cont 9;
	L.Ident happy_dollar_dollar -> cont 10;
	L.Not -> cont 11;
	L.And -> cont 12;
	L.Or -> cont 13;
	L.Implies -> cont 14;
	L.Globally -> cont 15;
	L.Future -> cont 16;
	L.Next -> cont 17;
	L.Until -> cont 18;
	L.Prob -> cont 19;
	L.Comma -> cont 20;
	L.Less -> cont 21;
	L.Leq -> cont 22;
	L.Greater -> cont 23;
	L.Geq -> cont 24;
	L.DoubleLit happy_dollar_dollar -> cont 25;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 26 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(L.Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
ltlParser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

ltlProbContractParser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [L.Token] -> a
parseError ts = error $ "Parse error:" <> show ts

-- | utility function to parse an LTL formula from a string
parseLTL :: String -> Formula
parseLTL = ltlParser . L.alexScanTokens

-- | utility function to parse a probabilistic contract specified with ltl from a string
parseLTLProbContract :: String -> ProbContract Formula
parseLTLProbContract = ltlProbContractParser . L.alexScanTokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
