{-# LANGUAGE RankNTypes #-} 
module Data.Conduit.ZMQ
       (
         SocketEnd(..)
       , SocketOpts(..)
       , zmqSource
       ) where
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Serialize (encode)
import Data.ByteString (pack)
import qualified Data.ByteString.Internal as BS
import Data.Conduit
import Data.Conduit.List as CL
import Prelude hiding (init)
import System.ZMQ

data SocketEnd = Bind String | Connect String
     deriving Show

data SocketOpts st = SocketOpts
     { end :: SocketEnd
     , sType :: st
     }
     
type SocketMaker = (SType st) => Context -> SocketOpts st -> IO (Socket st)


attach :: Socket a -> SocketEnd -> IO ()
attach sock (Bind s) = bind sock s
attach sock (Connect s) = connect sock s

mkSocket :: SocketMaker -- (SType st) => Context -> SocketOpts st  -> IO (Socket st)
mkSocket ctx so = do
  sock <- socket ctx $ sType so
  attach sock $ end so
  return sock
-- data SocketOpts = SocketOpts
--      { end :: SocketEnd
--      , sType :: SType st => st
--      }

zmqSource :: (ResourceIO m, SType st) => Context -> SocketOpts st -> Source m BS.ByteString
zmqSource ctx so = sourceIO
          (mkSocket ctx so)
          close
          (\sock -> liftIO $ do
              fmap Open $ receive sock [])
              
zmqSubSource :: (ResourceIO m, SType st, SubsType st) => Context 
                -> SocketOpts st 
                -> String 
                -> Source m BS.ByteString              
zmqSubSource ctx so subString = sourceIO
         (mkSocket ctx so >>= (\sock -> do
                               subscribe sock subString
                               return sock))
         close
         (\sock -> liftIO $ do
              fmap Open $ receive sock [])
         
         

zmqSink :: (ResourceIO m, SType st) => Context -> SocketOpts st -> Sink BS.ByteString m ()
zmqSink ctx so = sinkIO
         (mkSocket ctx so)
         close
         (\sock msg -> do
           liftIO $ send sock msg [] 
           return Processing)
         (\_ -> return ())

