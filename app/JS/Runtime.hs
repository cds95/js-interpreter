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
liftOp f eOne eTwo = 
    let (ConstVal cOne) = eOne 
        (ConstVal cTwo) = eTwo 
    in ConstVal (f cOne cTwo)