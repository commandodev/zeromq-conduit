{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit

import qualified Data.ByteString as S
import Data.Either
import Data.Serialize (encode, decode)
import Control.Concurrent
import Control.Applicative
import Data.Conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Conduit.ZMQ
import Prelude hiding (init)
import System.ZMQ

main :: IO ()
main = withContext 1 $ \ctx -> do 
  
  hspecX $ do
  
    describe "Closing Source" $ do
      it "drops messages" $ do
          feed ctx -- Sends the first 10 ints
          first5 <- get5 ctx
          feed ctx
          second5 <- get5 ctx -- if messages weren't dropped this would be [6..11]
          second5 @?= first5
        
    describe "Buffered Source" $ do
      it "Keeps messages" $ do
        feed ctx
        (first5, second5) <- runResourceT $ do
                bs <- C.bufferSource $ src ctx
                x <- bs $$ CL.map decode =$ CL.take 5
                y <- bs $$ CL.map decode =$ CL.take 5
                return (x, y)
        [1..5 :: Integer] @?= rights first5
        [6..10 :: Integer] @?= rights second5
         
    where
      feed :: Context ->  IO ThreadId
      feed ctx = do
        threadDelay 2000
        forkIO $ runResourceT $ listSrc $$ (snk ctx)
        where listSrc = CL.sourceList $ encode <$> [1..10 :: Integer]
      
      get5 :: Context -> IO [Integer]
      get5 ctx = runResourceT $ src ctx  $$ do
                  ints <- CL.map decode =$ CL.take 5
                  -- liftIO $ print strings
                  return $ rights ints
      src :: Context -> Source IO S.ByteString
      src ctx = zmqSource ctx (SockOpts (Bind "tcp://127.0.0.1:9999") Pull)

      snk :: Context -> Sink S.ByteString IO ()
      snk ctx = zmqSink ctx (SockOpts (Connect "tcp://127.0.0.1:9999") Push)

