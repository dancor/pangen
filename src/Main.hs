module Main where

import Control.Arrow
import Control.Monad.Error
import Control.Monad.Random
import Data.List
import Data.Maybe
import FUtil
import Test.HUnit
import qualified Data.Map as M

infixr 9 :->
infixr 8 :::
data Type = Var Int | Type :-> Type | Datum String [Type] | BaseType String
  deriving (Eq, Ord, Show)
data Func = String ::: Type
type Subst = [(Int, Type)]

-- basetypes _could_ be considered Datum .. [] if that simplifies things..
myInt = BaseType "Int"
myList x = Datum "List" [x]
myPair x y = Datum "Pair" [x, y]

-- todo: parse normal id :: a -> a from .hs input file to this
-- note: we are assuming all List's will matchingly have one var etc..
--    not sure if we'll add consistency checking or where
-- note: no typeclasses (just Int, no Integral)
-- note: by convention we always number left low
--    but we could relax this for algorithmic simplicity or efficiency..
funcs :: [Func]
funcs = [
  "id" ::: a :-> a,
  "const" ::: a :-> b :-> a,
  "flip" ::: (a :-> b :-> c) :-> b :-> a :-> c,
  "(.)" ::: (a :-> b) :-> (c :-> a) :-> c :-> b,
  "repeat" ::: a :-> myList a,
  "map" ::: (a :-> b) :-> myList a :-> myList b,
  "(+)" ::: myInt :-> myInt :-> myInt,
  "($)" ::: (a :-> b) :-> a :-> b
  ] where
  a = Var 1
  b = Var 2
  c = Var 3

funcToType :: M.Map String Type
funcToType = M.fromList $ map (\ (name ::: t) -> (name, t)) funcs

funcMap :: M.Map Type [(Type, String)]
funcMap = M.fromListWith (++) $ map (second (:[]) . f) funcs where
  f (name ::: (t1 :-> t2)) = (t1, (t2, name))

genExpr :: Type -> Rand g [String]
genExpr (BaseType t) = case t of
  "Int" -> return ["1"] -- todo: some variety..
  _ -> error $ "Don't know how to generate vals for base type: " ++ t
genExpr (Datum t vars) = case t of
  "List" -> return ["Empty"]  -- todo: variety
  _ -> error $ "Don't know how to generate vals for data type: " ++ t
genExpr (Var 1) = genExpr (BaseType "Int") -- todo: variety
genExpr (Var _) = error "Renumbering failure.."
genExpr (t1 :-> t2) = error "lol"
  -- find something that starts with t1

class Types t where
  apply :: Subst -> t -> t
  tv :: t -> [Int]

instance Types Type where
  apply s v@(Var i) = case lookup i s of
    Just t -> t
    Nothing -> v
  apply s (t1 :-> t2) = apply s t1 :-> apply s t2
  apply _s t = t

  tv (Var i) = [i]
  tv (t1 :-> t2) = tv t1 `union` tv t2
  tv _t = []

instance Types t => Types [t] where
  apply = map . apply
  tv = nub . concat . map tv

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(i, apply s1 t) | (i, t) <- s2] ++ s1

unify :: Type -> Type -> Either String Subst
unify (x1 :-> x2) (y1 :-> y2) = do
  s1 <- unify x1 y1
  s2 <- unify (apply s1 x2) (apply s1 y2)
  return $ s1 @@ s2
unify (Var i) t = varBind i t
unify t (Var i) = varBind i t
unify t1 t2 = if t1 == t2 then return []
  else fail $ "Unification failed; base types unequal: " ++ show (t1, t2)

varBind :: Int -> Type -> Either String Subst
varBind i t
  | t == Var i = return []
  | i `elem` tv t = fail $
    "Occurs check failed for var " ++ show i ++ " in " ++ show t
  -- fixme: do we need kind check in any form?
  | otherwise = return [(i, t)]

incVars :: Int -> Type -> Type
incVars n (t1 :-> t2) = incVars n t1 :-> incVars n t2
incVars n (Var i) = Var $ i + n
incVars n t = t

-- not sure how this should be correctly reconciled with apply..
-- can there ever be loops?  probably not right bc of occurs check?
recApply :: [(Int, Type)] -> Type -> Type
recApply s v@(Var i) = case lookup i s of
  Just t -> recApply s t
  Nothing -> v
recApply s (t1 :-> t2) = recApply s t1 :-> recApply s t2
recApply _s t = t

renumberVars :: Type -> Type
renumberVars = snd . renumberVarsAccum [] where
  renumberVarsAccum accum (Var i) = case lookup i accum of
    Just i' -> (accum, Var i')
    Nothing -> (accum ++ [(i, i')], Var i') where i' = length accum + 1
  renumberVarsAccum accum (t1 :-> t2) = (accum'', t1' :-> t2') where
    (accum', t1') = renumberVarsAccum accum t1
    (accum'', t2') = renumberVarsAccum accum' t2
  renumberVarsAccum accum (Datum s ts) = (accum', Datum s ts') where
    (accum', ts') = mapAccumL renumberVarsAccum accum ts
  renumberVarsAccum accum b@(BaseType s) = (accum, b)

-- Find most general type of h for f = g h
hWithFEqGH :: Type -> Type -> Either String Type
hWithFEqGH f g@(t1 :-> t2) = do
  let
    gNewVar = max0 (tv g) + 1
    f' = Var gNewVar :-> incVars gNewVar f
  s <- unify f' g
  let
    h = renumberVars . recApply s $ Var gNewVar
  -- The constraint derived for h thus far sometimes leads to an impossible
  -- situation.  (ex. f :: a -> b -> b -> c, g = (.))
  -- So we do a post-check of f = g h.  I don't know if we need to do more
  -- checking, I'm keeping things practical for now, not rigorous.
  sGh <- unify t1 $ (incVars . max0 $ tv t1) h
  let
    gh = renumberVars $ recApply sGh t2
    ghMax = max0 $ tv f
  sGhAndF <- unify f $ (incVars ghMax) gh
  when (not . null $ filter
    (\ (v, t) -> v > ghMax && any (> ghMax) (tv t)) sGhAndF) $
    fail $ "f vs g h type check fail: " ++ show (f, gh)
  return h

runTests :: IO ()
runTests = do
  let
    a = Var 1
    b = Var 2
    c = Var 3
    d = Var 4
    e = Var 5
    f1 = a :-> a :-> b
    f2 = a :-> b :-> b :-> c
    -- check that f = g h' implies that h' has type h
    assFGH fStr f gStr h = assertEqual (fStr ++ " " ++ gStr) (Right h) .
      hWithFEqGH f . fromJust $ M.lookup gStr funcToType
    assFGHFail fStr f gStr = assertBool (fStr ++ " " ++ gStr) . isLeft .
      hWithFEqGH f . fromJust $ M.lookup gStr funcToType
  c <- runTestTT $ TestList $ map TestCase [
    assFGH "f1" f1 "id" f1,
    assFGH "f1" f1 "const" $ a :-> b,
    assFGH "f1" f1 "flip" $ a :-> a :-> b,
    assFGHFail "f1" f1 "(.)",
    assFGHFail "f1" f1 "repeat",
    assFGHFail "f1" f1 "map",
    assFGHFail "f1" f1 "(+)",
    assFGH "f2" f2 "id" f2,
    assFGH "f2" f2 "const" $ a :-> a :-> b,
    assFGH "f2" f2 "flip" $ a :-> b :-> a :-> c,
    assFGHFail "f2" f2 "(.)",
    assFGHFail "f2" f2 "repeat",
    assFGHFail "f2" f2 "map",
    assFGHFail "f2" f2 "(+)"
    ]
  when (failures c /= 0) $ error "Test Fail!!!"

-- we should try deriving last = head . reverse

main :: IO ()
main = do
  runTests
  {-
  eval <- evalRandIO . genExpr $ myInt :-> myInt
  print eval
  -}
  let
    a = Var 1
    b = Var 2
    c = Var 3
    funcNames = map (\ (n ::: _) -> n) funcs
    tryFG f gStr = hWithFEqGH f . fromJust $ M.lookup gStr funcToType
    tryF f = (f, map (\ gStr -> (gStr, tryFG f gStr)) funcNames)
    myF1 = a :-> a :-> b
    myF2 = a :-> b :-> b :-> c
    pp (f, ress) = putStr . unlines $
      ["", show f] ++ spaceTable
      (map (\ (s, res) -> [s ++ ":", show res]) ress)
  pp $ tryF myF1
  pp $ tryF myF2
