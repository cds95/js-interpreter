module JS.Runtime where 
import JS.Core
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)

initialEnv :: Env
initialEnv = H.fromList []

runtimeOperations :: RuntimeOperationsEnv
runtimeOperations = H.fromList [
                        ("+", liftPrimitiveAdd), 
                        ( "-", liftPrimitiveSubtract), 
                        ("/", liftPrimitiveDivide), 
                        ("*", liftPrimitiveMultiply), 
                        ("&&", liftPrimitiveAndBoolean), 
                        ("||", liftPrimitiveOrBoolean),
                        (">", liftPrimitiveGreaterThan),
                        ("<", liftPrimitiveLessThan),
                        (">=", liftPrimitiveGreaterThanOrEqual),
                        ("<=", liftPrimitiveLessThanOrEqual),
                        ("!=", liftPrimitiveNotEqual),
                        ("==", liftPrimitiveEqual)
                    ]

liftPrimitiveAdd (Num pOne) (Num pTwo) = Num (pOne + pTwo) 
liftPrimitiveSubtract (Num pOne) (Num pTwo) = Num (pOne - pTwo)
liftPrimitiveDivide (Num pOne) (Num pTwo) = Num (pOne `div` pTwo)
liftPrimitiveMultiply (Num pOne) (Num pTwo) = Num (pOne * pTwo)
liftPrimitiveGreaterThan (Num pOne) (Num pTwo) = Boolean (pOne > pTwo)
liftPrimitiveLessThan (Num pOne) (Num pTwo) = Boolean (pOne < pTwo)
liftPrimitiveGreaterThanOrEqual (Num pOne) (Num pTwo) = Boolean (pOne >= pTwo)
liftPrimitiveLessThanOrEqual (Num pOne) (Num pTwo) = Boolean (pOne <= pTwo)
liftPrimitiveNotEqual (Num pOne) (Num pTwo) = Boolean (pOne /= pTwo)
liftPrimitiveNotEqual (Boolean a) (Boolean b) = Boolean (a /= b)
liftPrimitiveEqual (Num pOne) (Num pTwo) = Boolean (pOne == pTwo)
liftPrimitiveEqual (Boolean a) (Boolean b) = Boolean (a == b)

liftPrimitiveAndBoolean (Boolean a) (Boolean b) = Boolean (a && b)
liftPrimitiveOrBoolean (Boolean a) (Boolean b) = Boolean (a || b)

liftOp :: PrimitiveOp -> Val -> Val -> Val 
liftOp f (ConstVal cOne) (ConstVal cTwo) = ConstVal (f cOne cTwo) 
liftOp f (ConstVal cOne) (LetVal cTwo) = ConstVal (f cOne cTwo)
liftOp f (LetVal cOne) (ConstVal cTwo) = ConstVal (f cOne cTwo)
liftOp f (LetVal cOne) (LetVal cTwo) = ConstVal (f cOne cTwo)
liftOp f (IntVal a) (IntVal b) = liftOp f (ConstVal (Num a)) (ConstVal (Num b))
liftOp f c@(ConstVal cOne) (IntVal b) = liftOp f c (ConstVal (Num b))
liftOp f (IntVal b) c@(ConstVal cOne) = liftOp f c (ConstVal (Num b))
liftOp f c@(LetVal cOne) (IntVal b) = liftOp f c (ConstVal (Num b))
liftOp f (IntVal b) c@(LetVal cOne) = liftOp f c (ConstVal (Num b))