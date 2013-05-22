import System.Environment(getArgs)
import Text.Printf(printf)
import GAuth(gauth_totp)

main :: IO ()
main = do
  (secret:[]) <- getArgs
  (token, countdown) <- gauth_totp secret
  putStrLn $ printf "valid: %is" countdown
  putStrLn token
