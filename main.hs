import qualified Lexer as Lx

main :: IO ()
main = do
  file_as_str <- getLine
  let tokens = Lx.parseTokens file_as_str
  putStrLn $ show tokens

