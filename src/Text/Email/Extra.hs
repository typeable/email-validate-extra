{-# LANGUAGE CPP #-}
module Text.Email.Extra
  ( validateEmail
  , emailToText
  ) where

import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text as T (unpack)
import Data.Text.Encoding as T (decodeUtf8)
import Text.Email.Validate (EmailAddress)
import Text.Email.Validate as Email (validate, domainPart, toByteString)

#ifndef ghcjs_HOST_OS
import Text.Regex.PCRE.Heavy (re, (=~))

validateEmailDomain :: EmailAddress -> Either String EmailAddress
validateEmailDomain email =
  if emailDomain =~ domainRegex
    then Right email
    else Left $ errorMessage <> (T.unpack $ T.decodeUtf8 emailDomain)
  where
    emailDomain  = Email.domainPart email
    domainRegex  = [re|^[^.]+(?:\.[^.]+)*\.[^.]{2,}$|]
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
