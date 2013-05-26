import Control.Applicative((<$>))
import Text.Printf(printf)
import GAuth(gauth_totp)

main :: IO ()
main = mapM_ printToken =<< map words . filter (not.null) . lines <$> getContents
  where printToken (secret:name:[]) = do (token, countdown) <- gauth_totp secret
                                         putStrLn $ printf "%s\t(valid: %is)\t%s" token countdown name
        printToken (secret:[]) = printToken [secret, "<unnamed>"]
        printToken _ = error "syntax error"
