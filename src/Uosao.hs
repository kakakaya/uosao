{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | 基礎的な流れ
-- テーブルを元にNLP.Romkan向けのローマ字を出力して変換する
module Uosao ( makeMozc ) where

import           ClassyPrelude hiding (keys)
import           Data.Char
import qualified Data.Text     as T
import           NLP.Romkan

-- | 各項目がMozcのキーマップ一行となるリストを生成します
makeMozc :: [Text]
makeMozc = map (\(s, h) -> s <> "\t" <> h) romaKana

-- | ローマ字、かなのペアのリストを生成します
-- かなに未変換のローマ字がある項目は排除します
romaKana :: [(Text, Text)]
romaKana = ordNubBy fst (==) $ filter validEntry $ map (toHiraganaCompatibleForGoogle <$>) seqRoma
 where validEntry :: (Text, Text) -> Bool
       validEntry (key, kana) =
         not (any (\c -> isLatin1 c && isAlpha c) kana) &&
         T.length key <= 3 && T.length kana <= 3

-- | NLP.Romkanを使ってローマ字を平仮名に変換します
-- `toHiragana`は う゛ と出力してしまうため ゔ に直す処理を含めました
-- see [読みに「う゛」を含む単語を辞書登録できない - Gboard Community](https://support.google.com/gboard/thread/12248624?hl=ja)
-- ゖとかの小小書きは対応してなかったので雑にラップします
toHiraganaCompatibleForGoogle :: Text -> Text
toHiraganaCompatibleForGoogle = completeKogaki . T.replace "う゛" "ゔ" . toHiragana
  where completeKogaki = T.replace "xか" "ゕ" . T.replace "xけ" "ゖ"

-- | ローマ字、かなのペアのリストを基礎的なテーブルデータから生成します
seqRoma :: [(Text, Text)]
seqRoma = (manual <>) $ filter removeConflict $
  concat
  -- 単体
  [ single
  -- 小文字
  , small
  -- 2シーケンスの変換(hs -> ひょうなど)
  , concatMap
    (\x ->
      let shortcuts = de $ asLevelKeys x
      in [ c <> v
         | c@(cf, _) <- start x
         , v <- if T.length cf <= 1
                then basicVowel shortcuts
                else basicVowelBase
         ])
    consonant
  -- 拗音3シーケンスの変換(stn -> しゅくなど)
  , concatMap
    (\x ->
        [ (cf <> yoon x <> vf, cs <> vs)
        | (cf, cs) <- filter ((<= 1) . T.length . fst) $ start x
        , (vf, vs) <- yoonVowel (asLevelKeys x)
        ]
    )
    consonant
  -- 促音3シーケンスの変換
  , concatMap
    (\x ->
       [ (cf <> sokuon x <> vf, cs <> vs)
       | (cf, cs) <- filter ((<= 1) . T.length . fst) $ start x
       , (vf, vs) <- sokuonVowel
       ]
    )
    consonant
  ]
 where de xs = (headEx xs, lastEx xs)
       -- 他のテーブルと競合するものを排除
       removeConflict (s, _) = not
         (("ww" `isPrefixOf` s) || -- 草を生やすため
          ("we" `isPrefixOf` s) ||
          ("wi" `isPrefixOf` s))

  -- 小文字にするキーを付与したバージョンも作る
  -- concatMap (\x -> [x, bimap ("l" <>) ("x" <>) x]) $
-- 小文字は単体でのみ入力する
small :: [(Text, Text)]
small =
  [ ("la", "ぁ")
  , ("li", "ぃ")
  , ("lu", "ぅ")
  , ("le", "ぇ")
  , ("lo", "ぉ")
  , ("lca", "ゕ")
  , ("lce", "ゖ")
  , ("ltu", "っ")
  , ("lwa", "ゎ")
  , ("lva", "ゃ")
  , ("lvu", "ゅ")
  , ("lvo", "ょ")
  ]

-- | 手動で入れるしかない特殊変換
manual :: [(Text, Text)]
manual =
  [ ("nn" , "n'")
  , ("we" , "ゑ")
  , ("wi" , "ゐ")
  , ("/a" , "∧") -- andから連想
  , ("/o" , "∨") -- orから連想
  , ("/e" , "∃") -- existから連想
  , ("/u" , "∀") -- andが埋まっていたのでeの隣に置きました
  , ("/b" , "⇔") -- bothから連想
  , ("/d" , "∈") -- Dvorakだと∋と対になっていて丁度いい
  , ("/f" , "∋") -- Dvorakだと∈と対になっていて丁度いい
  , ("/w" , "ʬ")  -- wの特殊文字なので当然wに配置
  -- 矢印キーは z から派生
  -- dhtn: VIM 相当
  -- gcmw: ht(jk)の列で斜めを表現
  , ("zd" , "←")
  , ("zh" , "↓")
  , ("zt" , "↑")
  , ("zn" , "→")
  -- 斜め矢印: fgcr は dhtn の上段キー
  , ("zf" , "↖")
  , ("zg" , "↙")
  , ("zc" , "↘")
  , ("zr" , "↗")
  ]

-- | 単体で読みを構成するもの
single :: [(Text, Text)]
single =
  [ ("'", "xtu")
  , ("-", "ー")
  , ("p", "…") -- 便利
  , ("a", "a")
  , ("o", "o")
  , ("e", "e")
  , ("u", "u")
  , ("i", "i")
  , (";", "an'")
  , ("；", "an'")
  , ("q", "on'")
  , ("j", "en'")
  , ("k", "un'")
  , ("x", "in'")
  ]

-- | テーブルを構成するための配置データを構成する
data Consonant
  = Consonant
  { start       :: ![(Text, Text)] -- ^ 打ち始めの文字
  , yoon        :: !Text           -- ^ 拗音を開始するための文字
  , sokuon      :: !Text           -- ^ 促音を開始するための文字
  , asLevelKeys :: ![Text]         -- ^ 同じキーボードの段にある文字
  } deriving (Eq, Ord, Show, Read)

-- | 3段マップデータ
consonant :: [Consonant]
-- 中指で拗音（ゃゅょ）
-- 人差し指で促音（っ）
-- の拡張入力
consonant =
  [ Consonant
    { start       = [("f", "p"), ("g", "g"), ("c", "k"), ("r", "r")] <>
      [("fr", "pux"), ("gr", "gux"), ("cr", "kux"), ("rr", "rux")]
    , yoon        = "c"
    , sokuon      = "g"
    , asLevelKeys = ["f", "g", "c", "r", "l"]
    }
  , Consonant
    { start       = [("d", "d"), ("h", "h"), ("t", "t"), ("n", "n"), ("s", "s")] <>
      [("dn", "dex"), ("hn", "hux"), ("tn", "tex"), ("sn", "sux")]
    , yoon        = "t"
    , sokuon      = "h"
    , asLevelKeys = ["d", "h", "t", "n", "s"]
    }
  , Consonant
    { start       = [("b", "b"), ("m", "m"), ("w", "w"), ("v", "y"), ("z", "z")] <>
      [("bv", "bux"), ("mv", "mux"), ("vv", "v"), ("wv", "ux"), ("zv", "zux")]
    , yoon        = "w"
    , sokuon      = "m"
    , asLevelKeys = ["b", "m", "w", "v", "z"]
    }
  ]

-- | 基礎的な変換テーブル（ショートカットなし）
basicVowelBase :: [(Text, Text)]
basicVowelBase =
  [ ("'", "ai")
  , (",", "ou")
  , ("、", "ou")
  , (".", "ei")
  , ("。", "ei")
  , ("p", "uu")
  , ("y", "ui")
  , ("a", "a")
  , ("o", "o")
  , ("e", "e")
  , ("u", "u")
  , ("i", "i")
  , (";", "an'")
  , ("；", "an'")
  , ("q", "on'")
  , ("j", "en'")
  , ("k", "un'")
  , ("x", "in'")
  ]

-- | 基礎的な変換テーブル（ショートカット付き）
basicVowel :: (Text, Text) -> [(Text, Text)]
basicVowel (yuu, you) =
  basicVowelBase
  <> [(yuu, "ixyuu"), (you, "ixyou")] -- 2キーショートカット

-- | 拗音を含む出力をするためのテーブル
yoonVowel :: [Text] -> [(Text, Text)]
yoonVowel keys =
  [ ("'", "ixyai") -- ゃい
  , (",", "ixyou") -- ょう
  , ("、", "ixyou") -- ょう
  , (".", "ixei") -- ぇい
  , ("。", "ixei") -- ぇい
  , ("p", "ixyuu") -- ゅう
  , ("y", "ixyui") -- ゅい
  , ("a", "ixya") -- ゃ
  , ("o", "ixyo") -- ょ
  , ("e", "ixe") -- ぇ
  , ("u", "ixyu") -- ゅ
  , ("i", "ixi") -- ぃ
  , (";", "ixyan'") -- ゃん
  , ("；", "ixyan'") -- ゃん
  , ("q", "ixyon'") -- ょん
  , ("j", "ixen'") -- ぇん
  , ("k", "ixyun'") -- ゅん
  , ("x", "ixin'")-- ぃん
  ]
  <> zip keys ["ixyatu", "ixyaku", "ixyoku", "ixyuku", "ixyutu"] -- 3キーショートカット

-- | 促音を含む出力をするためのテーブル
sokuonVowel :: [(Text, Text)]
sokuonVowel =
  [ ("'", "ixyaxtu")
  , (",", "ixyoxtu")
  , ("、", "ixyoxtu")
  , (".", "ixextu")
  , ("。", "ixextu")
  , ("p", "ixyuxtu")
  , ("y", "ixixtu")
  , ("a", "axtu") -- あっ
  , ("o", "oxtu")
  , ("e", "extu")
  , ("u", "uxtu")
  , ("i", "ixtu")
  , (";", "an'xtu")
  , ("；", "an'xtu")
  , ("q", "on'xtu")
  , ("j", "en'xtu")
  , ("k", "un'xtu")
  , ("x", "in'xtu")
  ]
