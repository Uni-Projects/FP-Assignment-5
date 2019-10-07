module Hardware
where

data Bit  =  O | I
  deriving (Eq, Ord, Show)

infixr 3 ∧∧
(∧∧) :: Bit -> Bit -> Bit
O ∧∧ _b  =  O
I ∧∧ b   =  b

infixr 2 ||
(||) :: Bit -> Bit -> Bit
O || b   =  b
I || _b  =  I

infixr 4 ><
(><) :: Bit -> Bit -> Bit
O >< O  =  O
O >< I  =  I
I >< O  =  I
I >< I  =  O

--mapr :: ((a, state) -> (b, state)) -> (([a], state) -> ([b], state))

type Carry  =  Bit

halfAdder :: (Bit, Bit) -> (Bit, Carry)
halfAdder (x,y) = ((Hardware.><) x y, (Hardware.∧∧) x y)

fullAdder :: ((Bit, Bit), Carry) -> (Bit, Carry)
fullAdder ((b1,b2), c) = (res,cout)
   where
      res = fst(halfAdder (fst (halfAdder (b1,b2)),c))
      cout = (Hardware.||) (snd (halfAdder (b1,b2))) (snd (halfAdder (fst (halfAdder (b1,b2)),c)))
