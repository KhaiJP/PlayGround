import Prelude hiding (succ, (+))


class ArithmeticNat a where
    zero :: a
    succ :: a -> a
    (+)  :: a -> a -> a


data Nat = Zero | Succ Nat deriving (Eq, Show)
instance ArithmeticNat Nat where
    zero = Zero
    succ = Succ
    x + Zero = x
    x + (Succ y) = Succ (x + y)


main :: IO ()
main = do
    let one  :: Nat = succ zero
    let two  :: Nat = succ one
    let two' :: Nat = one + one
    print $ two == two'
    print two'
