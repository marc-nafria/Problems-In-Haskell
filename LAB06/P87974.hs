main :: IO ()
main = do
  nom <- getLine
  let s = saludaAdeu nom
  putStrLn s

saludaAdeu :: String -> String
saludaAdeu [] = ""
saludaAdeu (x : _)
  | x == 'A' = "Hello!"
  | x == 'a' = "Hello!"
  | otherwise = "Bye!"