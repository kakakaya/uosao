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
    -- 基本的な変換（parametrize風に書く）
    describe "基本的な変換" $ do
      forM_ ["sa\tさ", "ta\tた", "na\tな"] $ \expected ->
        it ("出力に '" <> unpack expected <> "' が含まれている") $ do
          makeMozc `shouldSatisfy` elem expected

    -- 存在しないものが含まれていないこと
    describe "存在しないもの" $ do
      forM_ ["hoge", "fuga", "piyo"] $ \notExpected ->
        it ("出力に '" <> unpack notExpected <> "' が含まれていない") $ do
          makeMozc `shouldSatisfy` notElem notExpected

    -- 「ん」の変換
    it "出力に 'nn\tん' が含まれている" $ do
      makeMozc `shouldSatisfy` elem "nn\tん"

    -- 矢印キー変換
    describe "矢印キー変換" $ do
      forM_ [("zd", "←"), ("zn", "→"), ("zh", "↓"), ("zt", "↑")] $ \(key, arrow) ->
        it ("'" <> unpack key <> "' -> '" <> unpack arrow <> "'") $ do
          makeMozc `shouldSatisfy` elem (key <> "\t" <> arrow)

    -- 小文字変換
    describe "小文字変換" $ do
      forM_ [("la", "ぁ"), ("ltu", "っ"), ("xa", "ぁ")] $ \(key, kana) ->
        it ("'" <> unpack key <> "' -> '" <> unpack kana <> "'") $ do
          makeMozc `shouldSatisfy` elem (key <> "\t" <> kana)

    -- 特殊文字変換
    it "出力に '/a\t∧' が含まれている" $ do
      makeMozc `shouldSatisfy` elem "/a\t∧"

    -- 拗音変換
    describe "拗音変換" $ do
      forM_ [("sta", "しゃ"), ("hna", "ひゃ")] $ \(key, kana) ->
        it ("'" <> unpack key <> "' -> '" <> unpack kana <> "'") $ do
          makeMozc `shouldSatisfy` elem (key <> "\t" <> kana)

    -- 促音変換
    it "出力に 'sha\tさっ' が含まれている" $ do
      makeMozc `shouldSatisfy` elem "sha\tさっ"

    -- 長音
    it "出力に '-\tー' が含まれている" $ do
      makeMozc `shouldSatisfy` elem "-\tー"

    -- 長い変換がないこと
    it "出力の左辺に4文字以上のローマ字が含まれていない" $ do
      let keys = map (T.takeWhile (/= '\t')) makeMozc
      keys `shouldSatisfy` all (\k -> T.length k <= 3)


main :: IO ()
main = hspec spec
