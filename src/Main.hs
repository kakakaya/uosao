{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 標準出力にMozcのキーマップを出力します
module Main ( main ) where

import ClassyPrelude
import Uosao (makeMozc)

main :: IO ()
main = mapM_ putStrLn makeMozc
