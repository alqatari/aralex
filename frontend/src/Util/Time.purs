module Util.Time where

import Prelude
import Data.DateTime (DateTime, time)
import Data.Time (hour, minute, second)
import Data.Enum (fromEnum)

-- | Format a DateTime as HH:MM:SS
formatTime :: DateTime -> String
formatTime dt =
  let t = time dt
      h = fromEnum $ hour t
      m = fromEnum $ minute t
      s = fromEnum $ second t
      pad n = if n < 10 then "0" <> show n else show n
  in pad h <> ":" <> pad m <> ":" <> pad s
