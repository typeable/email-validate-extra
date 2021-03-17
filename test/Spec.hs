{-# LANGUAGE OverloadedStrings #-}
import Data.Either
import Test.Hspec
import Text.Email.Extra

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "validate emails" $ do
    it "correct emails" $
      all (isRight . validateEmail)
        [ "nick@typeable.io"
        , "user@wtf.pl"
        , "moot@4chan.org"
        , "capt@spaceship.gnaa.eu"
        ]
    it "incorrect emails" $
      all (isLeft . validateEmail)
        [ "user@.com"
        , "@.typeable.io"
        , "x@.com"
        , "abcd@eff.c"
        ]