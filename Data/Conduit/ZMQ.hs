{-# LANGUAGE RankNTypes #-} 
module Data.Conduit.ZMQ
       (
         SocketEnd(..)
       , SocketOpts(..)
       ) where

import Data.Conduit
import System.ZMQ

data SocketEnd = Bind String | Connect String
     deriving Show

data SocketOpts st = SocketOpts
     { end :: SocketEnd
     , sType :: st
     }

attach :: Socket a -> SocketEnd -> IO ()
attach sock (Bind s) = bind sock s
attach sock (Connect s) = connect sock s

-- data SocketOpts = SocketOpts
--      { end :: SocketEnd
--      , sType :: SType st => st
--      }

zmqSource :: (ResourceIO m, SType st) => Context -> SocketOpts st -> Source m a
zmqSource ctx so = sourceIO
          mkSocket
          recvSock
          (\x -> undefined)
          where
              recvSock = undefined
              mkSocket = do
                       sock <- socket ctx $ sType so
                       attach sock $ end so
                       return sock
                       