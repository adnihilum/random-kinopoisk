import Web.Scotty

main :: IO ()
main = do
  scotty 3000 $ get "/" $ html "Hello, world!"
