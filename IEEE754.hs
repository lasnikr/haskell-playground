import Data.Char (digitToInt)

type Bits = String
type Number = String

toNum :: Bits -> Number
toNum b = show ((-1) ^ s) ++ "*" ++ show m ++ "*" ++ show (2 ^^ e)
  where
    s = valueAt b 0 1 bitToNum
    e = valueAt b 1 8 bitToNum - 127
    m = 1 / fromIntegral (valueAt b 8 32 bitToNum') + 1

valueAt :: Bits -> Int -> Int -> (Bits -> Int -> Int) -> Int
valueAt bits start len b = b (take len $ drop start bits) 0

bitToNum :: Bits -> Int -> Int
bitToNum [] _ = 0
bitToNum bits i = digitToInt l * 2 ^ i + bitToNum t (i+1)
  where
    l = last bits
    t = init bits

bitToNum' :: Bits -> Int -> Int
bitToNum' [] _ = 0
bitToNum' bits i = digitToInt l * 2 ^ i + bitToNum' t (i+1)
  where
    l = head bits
    t = tail bits

isValid :: Bits -> Bool
isValid b = length bits == 32 && foldr (\x acc -> acc && (x == 0 || x == 1)) True bits
  where bits = map digitToInt b

toBits :: Number -> Bits
toBits = undefined
