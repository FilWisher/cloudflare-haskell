# Cloudflare

Haskell bindings to the Cloudflare V4 API

```
account :: Account
account = Account "someone@example.com" "<apikey>"

main :: IO ()
main = do
  records <- runCloudflareAPI $ do
    zid <- getZoneIDByName "example.com"
    listDNSRecords zid
  print records 
```
