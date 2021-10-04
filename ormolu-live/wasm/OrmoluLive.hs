module OrmoluLive where

import Asterius.Text
import Asterius.Types

-- TODO use actual implementation
webOrmolu :: JSString -> IO JSString
webOrmolu = pure . toJSString . ("\n" <>) . fromJSString

foreign export javascript "webOrmolu" webOrmolu :: JSString -> IO JSString
