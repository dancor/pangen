module Main where

import Control.Arrow
import Control.Monad.Random
import qualified Data.Map as M

data Type = Var Int | FType Type Type | Datum String [Type] | BaseType String
  deriving (Eq, Ord)
data Func = Func String Type

-- todo: parse human readable id :: a -> a to this
-- note: we are assuming all List's will matchingly have one var etc..
--    not sure if we'll add consistency checking or where
-- note: no typeclasses (just Int, no Integral)
-- note: by convention we always number left low
--    but we could relax this for algorithmic simplicity or efficiency..
funcs :: [Func]
funcs = [
  Func "id" (a --> a),
  Func "const" (FType a (FType b a)),
  Func "flip" (FType (FType (Var 1) (FType (Var 2) (Var 3))) 
    (FType (Var 2) (FType (Var 1) (Var 3)))),
  Func "(.)" (FType (FType (Var 1) (Var 2)) 
    (FType (FType (Var 2) (Var 3)) (FType (Var 1) (Var 3)))),
  Func "repeat" (FType (Var 1) (Datum "List" [Var 1])),
  Func "map" (FType (FType (Var 1) (Var 2)) 
    (FType (Datum "List" [Var 1]) (Datum "List" [Var 2]))),
  Func "(+)" (FType (BaseType "Int") (FType (BaseType "Int") (BaseType "Int")))
  ] where
    (-->) = FType
    a = Var 1
    b = Var 2

funcMap = M.fromListWith (++) $ map (second (:[]) . f) funcs where
  f (Func name (FType t1 t2)) = (t1, (t2, name))

genExpr :: Type -> Rand g [String]
genExpr (BaseType t) = case t of
  "Int" -> return ["1"] -- todo: some variety..
  _ -> error $ "don't know how to generate vals for base type: " ++ t
genExpr (Datum t vars) = case t of
  "List" -> return ["Empty"]  -- todo: variety
  _ -> error $ "don't know how to generate vals for data type: " ++ t
genExpr (Var 1) = genExpr (BaseType "Int") -- todo: variety
genExpr (Var _) = error "renumbering failure.."
genExpr (FType t1 t2) = error "todo"

main :: IO ()
main = do
  e <- evalRandIO . genExpr $ BaseType "Int"
  print e
