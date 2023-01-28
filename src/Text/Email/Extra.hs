{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
module Text.Email.Extra
  ( validateEmail
  , emailToText
  ) where

import           Control.Monad                        ((<=<))
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as ByteString
import           Data.Text                            (Text)
import qualified Data.Text.Encoding                   as Text
import qualified Text.Email.Validate                  as Email
import           Text.Email.Validate                  (EmailAddress)
#ifdef ARBITRARY
import           Test.QuickCheck
#endif
#ifdef POSTGRES
import           Data.CaseInsensitive                 as CI hiding (map)
import           Database.PostgreSQL.Simple.FromField as PG
import           Database.PostgreSQL.Simple.ToField   as PG
#endif
#ifdef AESON
import           Control.Monad
import           Data.Aeson
#endif

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
      , not $ any ByteString.null chunks
      -- Finally, check that the last chunk (TLD) is two or more
      -- characters long, since there are no single-character top domains.
      , ByteString.length (last chunks) >= 2 = True
      | otherwise = False

validateEmail :: ByteString -> Either String EmailAddress
validateEmail = validateEmailDomain <=< Email.validate

emailToText :: EmailAddress -> Text
emailToText = Text.decodeUtf8 . Email.toByteString

#ifdef ARBITRARY
instance Arbitrary EmailAddress where
  arbitrary = do
    s <- take 64 <$> listOf1 (elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'])
    return $ Email.unsafeEmailAddress (ByteString.pack s) "antorica.com"
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
#endif

#ifdef POSTGRES
instance ToField EmailAddress where
  toField = toField . CI.mk . emailToText

instance FromField EmailAddress where
  fromField f mbs = do
    ci <- fromField f mbs
    case validateEmail $ Text.encodeUtf8 $ CI.original ci of
      Left _  -> returnError ConversionFailed f "EmailAddress is not parseable"
      Right a -> return a
#endif

#ifdef AESON
instance ToJSON EmailAddress where
  toEncoding = toEncoding . emailToText
  toJSON = toJSON . emailToText

instance FromJSON EmailAddress where
  parseJSON = parseJSON >=> either fail pure . validateEmail . Text.encodeUtf8
#endif
