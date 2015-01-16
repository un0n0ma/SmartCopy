{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

import SmartCopy
import JSON
import Control.Applicative
import Control.Exception
import Data.Monoid
import Data.Aeson
import Data.Data
import GHC.Generics

---------------------------- Record datatype -----------------------------
data SomeData
    = SomeData
    { sd_1 :: String
    , sd_2 :: Int
    , sd_3 :: Int
    , sd_4 :: String
    } deriving (Generic, Show, Eq)

sd :: SomeData
sd = SomeData "Field1" 2 3 "Field 4"

instance Data SomeData
instance Typeable SomeData

instance Format m => SmartCopy SomeData m where
    serialize (SomeData a b c d)
        = enterCons (Cons 0 "SomeData" False [])
            (enterField (Field 0 "sd_1") (writeValue (PrimString a)) <>
            (enterField (Field 1 "sd_2") (writeValue (PrimInt $ toInteger b))) <>
            (enterField (Field 2 "sd_3") (writeValue (PrimInt $ toInteger c))) <>
            (enterField (Field 3 "sd_4") (writeValue (PrimString d))))
    parse js = runParser (SomeData <.$> a <.*> b <.*> c <.*> d) fail return
               where
                 a = primString <*> (unPack $ parseField (Field 0 "sd_1") [] js)
                 b = primInt <*> (unPack $ parseField (Field 1 "sd_2") [] js)
                 c = primInt <*> (unPack $ parseField (Field 2 "sd_3") [] js)
                 d = primString <*> (unPack $ parseField (Field 3 "sd_4") [] js)

----------------------- Trivial datatype --------------------------------
data PrimTest = PrimTest Int deriving (Show, Eq, Generic)
instance Data PrimTest
instance Typeable PrimTest

pt = PrimTest 45

instance Format m => SmartCopy PrimTest m where
    serialize (PrimTest a)
        = enterCons (Cons 0 "PrimTest" False []) (enterField (Field 0 "")
                                              (writeValue (PrimInt $ toInteger a)))
    parse js = let pb = unPack $ parseField (Field 0 "") [] js
               in runParser (PrimTest <.$>
                            (primInt <*> pb)) fail return


------------------- More complicated datatype (sumtype) ---------------------
data SumTest a = Sum1 Int a | Sum2 [SumTest a] | Sum3 | Sum4 Int | Sum5 [String] 
    deriving (Generic, Show, Eq)

st2 :: SumTest Int
st2 = Sum2 [Sum1 1 2 , Sum1 3 1, Sum2 [], Sum3]
st4 :: SumTest Int
st4 = Sum4 100
st1 :: SumTest String
st1 = Sum1 42 "somestring"
st3 :: SumTest Int
st3 = Sum3
st5 :: SumTest String
st5 = Sum5 ["eins", "zwei"]


instance (Format m, SmartCopy a m) => SmartCopy (SumTest a) m where
    serializeWithIndex (Sum1 i a) _ _ 
            = enterCons (Cons 0 "Sum1" True [])
                        ((enterField (Field 0 "") (writeValue (PrimInt $ toInteger i))) <>
                        (enterField (Field 1 "") (serializeWithIndex a 0 True)))
    serializeWithIndex (Sum2 xs) i b
            = let l = length xs in
              enterCons (Cons 1 "Sum2" True [])
                        (enterField (Field 0 "")
                        (openRepetition l (map (\x -> serializeWithIndex x i b) xs)))
    serializeWithIndex Sum3 _ _ = enterCons (Cons 2 "Sum3" True []) mempty
    serializeWithIndex (Sum4 i) _ _ = enterCons (Cons 3 "Sum4" True [])
                                      (enterField (Field 0 "")
                                      (writeValue (PrimInt $ toInteger i)))
    serializeWithIndex (Sum5 xs) i b
            = let l = length xs in
              enterCons (Cons 4 "Sum5" True [])
              (enterField (Field 0 "")
              (openRepetition l (map (\x -> serializeWithIndex x i b) xs)))
    parse js = let fields4 = [Field 0 ""]
                   fields3 = []
                   fields1 = [Field 0 "", Field 1 ""]
                   fields5 = [Field 0 ""]
                   a1 = primInt <*> pb1 ----- This needs fixing.
                   pb1 = unPack $ getFromCons (Cons 1 "Sum1" True fields1) js
                   pb3 =  unPack $ getFromCons (Cons 2 "Sum3" True fields3) js
                   pb4 = unPack $ getFromCons (Cons 3 "Sum4" True fields4) js
                   pb5 = unPack $ getFromCons (Cons 4 "Sum5" True fields5) js
                   in runParser (((Sum4 <.$> (primInt <*> pb4)) <|>
                                 (Sum3 <$ pb3))) -- <|>
                                fail return

--- Aeson instances for comparison:

instance ToJSON a => ToJSON (SumTest a)
instance ToJSON SomeData
instance ToJSON PrimTest

main = do putStrLn "~~ TESTING WITH MANUALLY WRITTEN INSTANCES ~~:\n"
          putStrLn "Encoding SomeData with SmartCopy:"
          runJSONEncode sd
          putStrLn "Encoding SomeData with Aeson:"
          print $ toJSON sd
          putStrLn "Encoding SumTest with SmartCopy:"
          runJSONEncode st2
          putStrLn "Encoding SumTest with Aeson:"
          print $ toJSON st2
          putStrLn "Parseing PrimTest: "
          print ((parse (serialize pt :: JSON Value)) :: JSON PrimTest)
          putStrLn "Parseing SomeData: "
          print ((parse (serialize sd :: JSON Value)) :: JSON SomeData)
          putStrLn "Encoding SumTest (easy type) with SmartCopy: "
          runJSONEncode st4
          putStrLn "Parseing SumTest (simple cons): "
          print ((parse (serialize st4 :: JSON Value)) :: JSON (SumTest Int))
          putStrLn "Encoding SumTest (empty cons): "
          runJSONEncode st3
          putStrLn "Parseing SumTest (empty cons): "
          print ((parse (serialize st3 :: JSON Value)) :: JSON (SumTest Int))
          putStrLn "Encoding SumTest (array): "
          runJSONEncode st5
          putStrLn "Parseing SumTest (array): "
          


