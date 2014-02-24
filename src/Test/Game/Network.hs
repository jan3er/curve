{-# OPTIONS -Wall #-}

module Test.Game.Network where

import System.IO
import System.Directory(removeFile,getTemporaryDirectory)
import Control.Applicative

import Test.HUnit

import Curve.Game.Network

--------------------------------------

messages :: [Msg]
messages = [ CMsgHello "asdfal asdkj asdlfkjh"
           , SMsgWorld [(4,Nothing)] 42 False ] 

test_putAndGetMsg :: Msg -> Test 
test_putAndGetMsg msg = TestCase $ do 
    -- create temporary file
    tempDir <- getTemporaryDirectory
    (filePath, handle) <- openTempFile tempDir "test_putAndGetMsg"
    -- write message to file
    putMsg handle msg
    -- go to start of file and 
    -- receive message from there
    hSeek handle AbsoluteSeek 0
    recv <- getMsg handle
    -- both messages should be equal 
    assertEqual "" (Just msg) recv
    removeFile filePath

-- TODO this might better be replaced by QuickCheck
test_putGet :: Test
test_putGet = TestList $ (TestLabel "put/get message" . test_putAndGetMsg) <$> messages
    
--------------------------------------

tests :: Test
tests = TestList 
    [ TestLabel "put/get message" test_putGet]
