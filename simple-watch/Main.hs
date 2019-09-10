import           System.FSNotify
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )

main = withManager $ \mgr -> do
    watchDir mgr "." (const True) print

    forever $ threadDelay 1000000
