{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import ClassyPrelude
import qualified Data.Text as T
import Test.Hspec
import Uosao (makeMozc)

spec :: Spec
spec = do
  describe "makeMozc" $ do
    -- 基本的な変換
    describe "基本的な変換" $ do
      forM_ [("a", "あ"), ("ca", "か")] $ \(key, kana) ->
        it ("'" <> unpack key <> "' -> '" <> unpack kana <> "'") $ do
          makeMozc `shouldSatisfy` elem (key <> "\t" <> kana)

    -- 左辺が重複しないこと
    it "出力の左辺に重複がない" $ do
      let keys = map (T.takeWhile (/= '\t')) makeMozc
      length keys `shouldBe` length (ordNub keys)

    -- 「ん」の変換
    it "出力に 'nn\tん' が含まれている" $ do
      makeMozc `shouldSatisfy` elem "nn\tん"

    -- 矢印キー変換
    describe "矢印キー変換" $ do
      forM_ [
("zd", "←"), ("zh", "↓"), ("zt", "↑"), ("zn", "→"), 
        ("zf", "↖"), ("zg", "↙"), ("zc", "↘"), ("zr", "↗")] $ \(key, arrow) ->
        it ("'" <> unpack key <> "' -> '" <> unpack arrow <> "'") $ do
          makeMozc `shouldSatisfy` elem (key <> "\t" <> arrow)

    -- 小文字変換
    describe "小文字は l で変換" $ do
      forM_ [("la", "ぁ"), ("lca", "ゕ"), ("lwa", "ゎ"), ("ltu", "っ")] $ \(key, kana) ->
        it ("'" <> unpack key <> "' -> '" <> unpack kana <> "'") $ do
          makeMozc `shouldSatisfy` elem (key <> "\t" <> kana)

describe "小文字を x で変換しない" $ do
      let keys = map (T.takeWhile (/= '\t')) makeMozc
      it "出力の左辺に x で始まる2文字以上のキーが存在しない" $ do
        keys `shouldSatisfy` all (\k -> not ("x" `T.isPrefixOf` k) || T.length k == 1)

    -- 特殊文字変換
    it "出力に '/a\t∧' が含まれている" $ do
      makeMozc `shouldSatisfy` elem "/a\t∧"

    -- 撥音
    describe "撥音変換" $ do
      forM_ [("cga", "かっ")] $ \(key, kana) ->
        it ("'" <> unpack key <> "' -> '" <> unpack kana <> "'") $ do
          makeMozc `shouldSatisfy` elem (key <> "\t" <> kana)

    -- 拗音変換
    describe "拗音変換" $ do
      forM_ [("sta", "しゃ"), ("hta", "ひゃ")] $ \(key, kana) ->
        it ("'" <> unpack key <> "' -> '" <> unpack kana <> "'") $ do
          makeMozc `shouldSatisfy` elem (key <> "\t" <> kana)

    -- 促音変換
    it "出力に 'sha\tさっ' が含まれている" $ do
      makeMozc `shouldSatisfy` elem "sha\tさっ"

    -- 長音
    it "出力に '-\tー' が含まれている" $ do
      makeMozc `shouldSatisfy` elem "-\tー"

    -- 長い変換がないこと
    it "出力の左辺はすべて3文字以下である" $ do
      let keys = map (T.takeWhile (/= '\t')) makeMozc
      keys `shouldSatisfy` all (\k -> T.length k <= 3)
    
    it "出力の右辺はすべて3文字以下である" $ do
      let values = map (T.tail . T.dropWhile (/= '\t')) makeMozc
      values `shouldSatisfy` all (\v -> T.length v <= 3)

main :: IO ()
main = hspec spec
