-- | A collection of default remotes.
--
-- These remotes are installed on an LXD instance by default.
--
-- Run @lxc image list@ for more information.
module Network.LXD.Client.Remotes where

import Network.LXD.Prelude

imagesRemote :: String
imagesRemote = "images"

ubuntuRemote :: String
ubuntuRemote = "ubuntu"

ubuntuDailyRemote :: String
ubuntuDailyRemote = "ubuntu-daily"
