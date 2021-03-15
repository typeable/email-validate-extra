{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Text.Email.Extra
  ( validateEmail
  , emailToText
  ) where

import           Control.Monad ((<=<))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.Text (Text)
import           Data.Text as T (unpack)
import           Data.Text.Encoding as T (decodeUtf8)
import           Test.QuickCheck
import           Text.Email.Validate (EmailAddress)
import           Text.Email.Validate as Email
  (domainPart, toByteString, unsafeEmailAddress, validate)

#ifndef ghcjs_HOST_OS
import           Text.Regex.PCRE.Heavy (re, (=~))

validateEmailDomain :: EmailAddress -> Either String EmailAddress
validateEmailDomain email =
  if emailDomain =~ domainRegex
    then Right email
    else Left $ errorMessage <> (T.unpack $ T.decodeUtf8 emailDomain)
  where
    emailDomain = Email.domainPart email
    domainRegex = [re|^[^.]+(?:\.[^.]+)*\.[^.]{2,}$|]
    errorMessage = "email domain is not valid: " :: String
#endif

validateEmail :: ByteString -> Either String EmailAddress
#ifndef ghcjs_HOST_OS
validateEmail = validateEmailDomain <=< Email.validate
#else
validateEmail = error "validateEmail: unimplemented in GHCJS"
#endif

emailToText :: EmailAddress -> Text
#ifndef ghcjs_HOST_OS
emailToText = T.decodeUtf8 . toByteString
#else
emailToText = error "emailToText: unimplemented in GHCJS"
#endif

instance Arbitrary EmailAddress where
  arbitrary = do
    s <- listOf1 (elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'])
    return $ unsafeEmailAddress (BSC.pack s) "example.com"
  -- FIXME: write the correct instance
  -- arbitrary = do
  --   m <- elements [1..32]
  --   n <- elements [1..32]
  --   let
  --     localGen = do
  --       let
  --         alphabets = T.pack $ ['a'..'Z']
  --         digits    = T.pack $ ['0'..'9']
  --         alphaNum  = alphabets <> digits
  --         symbols   = "-!#$%&'*+-/=?^_`{|}~."
  --         localSym  = alphaNum <> symbols
  --       encodeUtf8 <$> (replicateM m (elements localSym)
  --         `suchThat` (isRight . parseOnly localPart))
  --   local <- localGen
  --   -- no subdomains
  --   domain <- replicateM n (elements ['a'..'Z'])
  --   domainLiteral <- elements ["com", "travel", "net", "ru", "io"]
  --   unsafeEmailAddress <$> local <*> domain <> "." domainLiteral
