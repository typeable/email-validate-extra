{-# OPTIONS_GHC -Wno-orphans #-}
module Text.Email.Extra
  ( validateEmail
  , emailToText
  ) where

import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Test.QuickCheck
import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as Email

-- | Validate that the domain part in this email address is
--   in fact a domain name.
--   The old version of this function used the following regular expression
--   to validate email domains:
--   > domainRegex = [re|^[^.]+(?:\.[^.]+)*\.[^.]{2,}$|]
--   It was rewritten without regex to make this library more portable and
--   allow using it with GHCJS.
validateEmailDomain :: EmailAddress -> Either String EmailAddress
validateEmailDomain email
  | validDomain domain = Right email
  | otherwise = Left ("email domain invalid: " <> ByteString.unpack domain)
  where
    domain = Email.domainPart email
    validDomain dom
      -- Split domain into chunks,
      -- i.e. awesome.typeable.io -> [awesome typeable io]
      | chunks <- ByteString.split '.' dom
      -- Check that there are at least two chunks,
      -- do not allow one word domains.
      , length chunks >= 2
      -- Check that there are no empty chunks. This would happen if the domain
      -- starts or ends with '.' or has two or more dots next to one another.
      , none (ByteString.null) chunks
      -- Finally, check that the last chunk (TLD) is two or more
      -- characters long, since there are no single-character top domains.
      , ByteString.length (last chunks) >= 2 = True
      | otherwise = False

validateEmail :: ByteString -> Either String EmailAddress
validateEmail = validateEmailDomain <=< Email.validate

emailToText :: EmailAddress -> Text
emailToText = Text.decodeUtf8 . Email.toByteString

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f = not . any f

instance Arbitrary EmailAddress where
  arbitrary = do
    s <- take 64 <$> listOf1 (elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'])
    return $ Email.unsafeEmailAddress (ByteString.pack s) "example.com"
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
