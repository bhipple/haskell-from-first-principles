module Main where

import Control.Monad (forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
    (soc, _) <- accept sock
    printAndKickback soc
    sClose soc
    where printAndKickback conn = do
            msg <- recv conn 1024
            print msg
            sendAll conn msg

main :: IO ()
main = withSocketsDo $ do
        return () 
