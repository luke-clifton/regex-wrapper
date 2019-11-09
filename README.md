This library allows you to create types that are guaranteed to contain a
string that matches a given regular expression which is expressed at the
type level.

```haskell
newtype User = User (Matched String "^[a-zA-Z0-9]{4,15}$")

parseUser :: String -> Either (RegexError String) User
parseUser = fmap User . parseMatchedEither

prettyUser :: User -> String
prettyUser (User m) = asString m

main :: IO ()
main = do
    l <- getLine
    case parseUser l of
        Right user -> putStrLn $ "Hello, " ++ prettyUser user ++ "!"
        Left error -> putStrLn $ "Could not parse username: " ++ prettyRegexError  error
```

```
./prog
bad
Could not parse username: The input "bad" did not match the pattern ^[a-zA-Z0-9]{4,15}$
```

```
./prog
good
Hello, good!
```
