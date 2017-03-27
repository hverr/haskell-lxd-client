import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Servant.Client

import Network.LXD.Client

main :: IO ()
main = do
    manager <- newManager managerSettings
    res <- runClientM apiConfig (ClientEnv manager baseUrl)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right sup -> print sup
  where
    managerSettings = mkManagerSettings tlsSettings Nothing
    tlsSettings = TLSSettingsSimple True False False
    baseUrl = BaseUrl Https "127.0.0.1" 8443 ""
