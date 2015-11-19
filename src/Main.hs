{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (release)
import           Data.ByteString.Char8        hiding (take)
import           Data.Default
import           Data.Monoid
import           Database.LevelDB
import qualified Database.LevelDB.Streaming   as S
import           Prelude                      hiding (putStrLn)

main :: IO ()
main = runResourceT $ do
    printVersion

    db <- open "/tmp/leveltest"
               defaultOptions{ createIfMissing = True
                             , cacheSize= 2048
                             }

    putStrLn' "Put value"
    put db def "foo" "bar"
    get db def "foo" >>= liftIO . print

  where
    printVersion = do
        v <- versionBS
        putStrLn' $ "LevelDB Version: " <> v

    printDbSize db = do
        s <- approximateSize db ("a", "z")
        putStrLn' $ "Approximate DB size: " <> pack (show s)

    versionBS = do
        (major, minor) <- version
        return $ intToBs major <> "." <> intToBs minor

    intToBs :: Int -> ByteString
    intToBs = pack . show

    putStrLn' = liftIO . putStrLn