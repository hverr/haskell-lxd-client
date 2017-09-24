-- | A collection of default remotes.
--
-- These remotes are installed on an LXD instance by default.
--
-- Run @lxc image list@ for more information.
module Network.LXD.Client.Remotes where

import Network.LXD.Prelude

imagesRemote :: String
imagesRemote = "https://images.linuxcontainers.org"

ubuntuRemote :: String
ubuntuRemote = "https://cloud-images.ubuntu.com/releases"

ubuntuDailyRemote :: String
ubuntuDailyRemote = "https://cloud-images.ubuntu.com/daily"
