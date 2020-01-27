# Cloudflare

Haskell bindings to the [Cloudflare V4 API](https://api.cloudflare.com/)

```haskell
auth :: TokenAuth
auth = TokenAuth "<api-token>"

main :: IO ()
main = do
  records <- runCloudflareAPI auth $ do
    zid <- getZoneIDByName "example.com"
    listDNSRecords zid
  print records
```
