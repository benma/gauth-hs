module GAuth(gauth_hotp, gauth_totp) where

import Data.HMAC (hmac_sha1)
import qualified Data.Binary.Put as BP
import qualified Data.Binary.Get as BG
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.Base32 as B32
import Data.Time.Clock.POSIX(getPOSIXTime)
import Data.Bits
import Text.Printf(printf)

gauth_hotp :: String -> Integer -> String
gauth_hotp secret intervalNo = printf "%06i" token
  where
    msg = BP.runPut $ BP.putWord64be $ fromIntegral intervalNo
    -- pad secret such that its length is divisible by 8 ('A' = 0b00000 in base32).
    Just key = B32.decode $ secret ++ (replicate (8 - length secret `rem` 8) 'A')
    hash = hmac_sha1 key $ BS.unpack msg
    last_nibble = fromIntegral $ last hash .&. 0x0f
    hash_part = take 4 $ drop last_nibble hash
    token_base = (BG.runGet BG.getWord32be $ BS.pack $ hash_part) .&. 0x7fffffff
    token = token_base `rem` 1000000

gauth_totp :: String -> IO (String, Int)
gauth_totp secret = do
  posixTime <- getPOSIXTime
  let (intervalNo, remaining) = floor posixTime `quotRem` 30
  return (gauth_hotp secret intervalNo, 30 - fromIntegral remaining)
