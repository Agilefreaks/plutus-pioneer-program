module Week04.MaybeNew where

import           Text.Read       (readMaybe)
import           Week04.MonadNew (threeInts)

readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
    Nothing -> Left $ "can't parse: " ++ s
    Just x  -> Right x

foo :: String -> String -> String -> Maybe Int
foo x y z = case readMaybe x of
    Nothing -> Nothing
    Just k -> case readMaybe y of
        Nothing -> Nothing
        Just l -> case readMaybe z of
            Nothing -> Nothing
            Just m  -> Just (k + l + m)

foo'' :: String -> String -> String -> Either String Int
foo'' x y z = case readEither x of
    Left err -> Left err
    Right k -> case readEither y of
        Left err -> Left err
        Right l -> case readEither z of
            Left err -> Left err
            Right m  -> Right (k + l + m)

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _  = Nothing
bindMaybe (Just x) f = f x

bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x) f  = f x

foo' :: String -> String -> String -> Maybe Int
foo' x y z = readMaybe x `bindMaybe` \k ->
             readMaybe y `bindMaybe` \l ->
             readMaybe z `bindMaybe` \m ->
             Just (k + l + m)

fooEither :: String -> String -> String -> Either String Int
fooEither x y z =
    readEither x `bindEither` \k ->
    readEither y `bindEither` \l ->
    readEither z `bindEither` \m ->
    Right (k + l + m)

fooMonad :: String -> String -> String -> Maybe Int
fooMonad x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)

eitherMonad :: String -> String -> String ->Either String Int
eitherMonad x y z = threeInts (readEither x) (readEither y) (readEither z)
