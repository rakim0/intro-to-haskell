data Nat = Zero | Succ Nat deriving (Eq)

natToInt  :: Nat -> Integer
natToInt Zero = 0
natToInt (Succ n) = 1+natToInt n

instance Show Nat where
    show = show.natToInt