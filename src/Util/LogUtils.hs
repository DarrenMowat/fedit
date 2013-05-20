module Util.LogUtils where 

import Debug.Trace 

-- So log statments can easily be turned off when not in development ;)
shout :: String -> a -> a
shout _ a = a
-- shout s a = trace s a