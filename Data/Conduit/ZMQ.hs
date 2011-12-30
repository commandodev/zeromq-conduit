{-# LANGUAGE RankNTypes, GADTs #-} 
module Data.Conduit.ZMQ
       (
         SocketEnd(..)
       , SocketOpts(..)
       , zmqSource
       , zmqSink
       ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Internal as BS
import Data.Conduit
import Prelude hiding (init)
import System.ZMQ

data SocketEnd = Bind String | Connect String
     deriving Show

data SocketOpts st where
     SockOpts :: (SType st) => SocketEnd -> st -> SocketOpts st
     SubOpts :: (SubsType st) => SocketEnd -> st -> String -> SocketOpts st

type SocketMaker = (SType st) => Context -> SocketOpts st -> IO (Socket st)


attach :: Socket a -> SocketEnd -> IO ()
attach sock (Bind s) = bind sock s
attach sock (Connect s) = connect sock s

mkSocket :: SocketMaker
mkSocket ctx so =
  case so of
        (SockOpts e st) -> do 
               sock <- socket ctx st
               attach sock e
               return sock
        (SubOpts e st sub) -> do
               sock <- socket ctx st
               attach sock e
               subscribe sock sub
               return sock


zmqSource :: (ResourceIO m, SType st) => Context
                         -> SocketOpts st
                         -> Source m BS.ByteString
zmqSource ctx so = sourceIO
          (mkSocket ctx so)
          close
          (\sock -> liftIO $ do
              fmap Open $ receive sock [])


zmqSink :: (ResourceIO m, SType st) => Context
                       -> SocketOpts st
                       -> Sink BS.ByteString m ()
zmqSink ctx so = sinkIO
         (mkSocket ctx so)
         close
         (\sock msg -> do
           liftIO $ send sock msg [] 
           return Processing)
         (\_ -> return ())

