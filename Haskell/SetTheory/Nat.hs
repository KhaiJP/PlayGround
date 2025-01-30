import Prelude hiding (succ)


class ArithmeticNat a where
    zero :: a
    succ :: a -> a
    add  :: a -> a -> a


data Nat = Zero | Succ Nat deriving (Eq, Show)
instance ArithmeticNat Nat where
    zero = Zero
    succ = Succ
    add x Zero = x
    add x (Succ y) = Succ (add x y)


main :: IO ()
main = do
    let one  :: Nat = succ zero
    let two  :: Nat = succ one
    let two' :: Nat = add one one
    print $ two == two'
    print two'
