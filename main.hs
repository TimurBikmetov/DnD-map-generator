{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.Time (Day)
import Text.Read (readEither)

newtype FullName = FullName String
  deriving (Show)

newtype Phone = Phone Integer
  deriving (Show)

newtype Email = Email String
  deriving (Show)

data Role = Regular | Admin
  deriving (Show, Read)

data User = User
  { userName      :: FullName
  , userPhone     :: Phone
  , userBirthday  :: Maybe Day
  , userEmail     :: Maybe Email
  , userRole      :: Role
  } deriving (Show)

-- | Prompt user input.
--
-- >>> prompt "Full name"
-- Full name: Jack
-- "Jack"
prompt :: String -> IO String
prompt str = do
    putStr (str ++ ": ")
    getLine
    -- case str of
    --     "Full name" -> putStrLn userName
    --     "Phone" -> putStrLn userPhone
    --     "Day" -> putStrLn userBirthday
    --     "Email" -> putStrLn userEmail
    --     "userRole" -> putStrLn userRole
-- | An input form for a value of type a.
--
-- >>> runForm (textField "Full Name")
-- Full Name: Jack
-- Right "Jack"

-- >>> runForm (numField "Age" :: Form Int)
-- Age: 23
-- Right 23

-- >>> runForm (numField "Age" :: Form Int)
-- Age: twenty
-- Left "Prelude.read: no parse"

-- >>> runForm (readField "Date of Birth" :: Form Day)
-- Date of Birth: 2001-02-03
-- Right 2001-02-03
newtype Form a = Form { runForm :: IO (Either String a) }
  deriving (Functor)

-- | Build a form with a single field.
field
  :: (String -> Either String a)  -- ^ How to parse this field.
  -> String                       -- ^ Field name.
  -> Form a
field f str = Form $ do
    input <- prompt str
    return (f input)

textField :: String -> Form String
textField str = field Right str  

numField :: Num a => Read a => String -> Form a
numField = field readEither


readField :: Read a => String -> Form a
readField x = field readEither x

optional :: Form a -> Form (Maybe a)
optional (Form io) = Form $ do
  result <- io
  case result of
    Left _    -> return (Right Nothing)
    Right val -> return (Right (Just val)) 
-- | Try to parse a form.
-- Upon failure return default value.
--
-- >>> runForm (withDefault (readField "Role" :: Form Role) Regular)
-- Role: something
-- Right Regular
withDefault :: Form a -> a -> Form a
withDefault (Form io) def = Form $ do
  result <- io
  case result of
    Left _    -> return (Right def)
    Right val -> return (Right val)
-- | Try to parse a form.
-- Upon failure return 'Nothing'.
--
-- >>> runForm (optional (numField "Age" :: Form Int))
-- Age: 12
-- Right Nothing
optionalField :: String -> (String -> Either String a) -> Form (Maybe a)
optionalField label parser = Form $ do
  putStr (label ++ ": ")
  input <- getLine
  if null input
    then return (Right Nothing)
    else case parser input of
      Left _err -> return (Right Nothing) 
      Right val -> return (Right (Just val))

instance Applicative Form where
  pure x = Form $ return (Right x)
  
  (Form ff) <*> (Form fa) = Form $ do
    ef <- ff
    ea <- fa
    return $ case (ef, ea) of
      (Right f, Right a) -> Right (f a)
      (Left err, _)      -> Left err
      (_, Left err)      -> Left err

instance Monad Form where
  return = pure

  (Form fa) >>= f = Form $ do
    ea <- fa
    case ea of
      Left err -> return (Left err)
      Right a  -> runForm (f a)

-- >>> runForm user
-- Full Name: Jack
-- Phone: 123456
-- Birthday: 2001-02-03
-- Email: jack@example.com
-- Role: Regular
-- Right (User {userName = FullName "Jack", ...})
user :: Form User
user = do
  userName     <- FullName <$> textField "Full Name"
  userPhone    <- Phone <$> numField "Phone"
  userBirthday <- optional (readField "Birthday")
  userEmail    <- fmap Email <$> optional (textField "Email")
  userRole     <- readField "Role" `withDefault` Regular
  return (User
    { userName     = userName
    , userPhone    = userPhone
    , userBirthday = userBirthday
    , userEmail    = userEmail
    , userRole     = userRole
    })
main :: IO ()
main = do
  result <- runForm user
  print result