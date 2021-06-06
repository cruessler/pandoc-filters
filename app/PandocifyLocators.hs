{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.Regex.Posix

main :: IO ()
main = toJSONFilter pandocifyLocators


{-|
  Replaces locators like "(S. 10)", "(S. 10 f.)", or "(S. 10–12)" by "[@key, S.
  10]", "[@key, S. 10 f.]", or "[@key, S. 10–12]" respectively.

  While the source form is more easily read by humans, the target form can be
  processed by pandoc.

  Assumes that the first paragraph of the document contains a single BibTeX key
  and nothing else (this paragraph is filtered out).
-}
pandocifyLocators :: Pandoc -> Pandoc
pandocifyLocators pandoc@(Pandoc meta blocks) =
  case blocks of
    (Para [Cite [Citation {citationId = id, citationPrefix = [], citationSuffix = []}] inlines]:rest) ->
      Pandoc meta $ walk (convertBlock $ T.unpack id) rest
    _ ->
      pandoc


convertBlock :: String -> Block -> Block
convertBlock id (BlockQuote blocks) =
  BlockQuote $ walk (convertBlockInQuote id) blocks
convertBlock _ block = block


convertBlockInQuote :: String -> Block -> Block
convertBlockInQuote id (Para inlines) =
  Para (pandocifyInlines id . isolateNumbers . convertSoftBreaks $ inlines)
convertBlockInQuote _ block = block


{-|
  Turns `SoftBreak` into `Space`.
-}
convertSoftBreaks :: [Inline] -> [Inline]
convertSoftBreaks inlines = walk f inlines
  where
    f SoftBreak = Space
    f x = x


{-|
 Turns `Str "100)"` into `Str "100", Str ")"`.
-}
isolateNumbers :: [Inline] -> [Inline]
isolateNumbers inlines =
  f inlines []
  where
    f (s@(Str str):xs) acc
      | matches = (Str (T.pack number):Str ")":f xs acc)
      | otherwise = (s:f xs acc)
      where
        unpackedStr = T.unpack str
        matches = unpackedStr =~ regex :: Bool
        match = unpackedStr =~ regex :: AllTextSubmatches [] String

        (_:[number, _]) = getAllTextSubmatches match

    f (x:xs) acc = (x:f xs acc)

    f [] acc = acc

    regex = "([0-9]+(–[0-9]+)?)\\)$" :: String


pandocifyInlines :: String -> [Inline] -> [Inline]
pandocifyInlines id inlines = f id inlines []
  where
    -- Returns "[@lorem, S. 100 f.]".
    g id locator =
      RawInline (Format "markdown") (T.pack $ "[@" ++ id ++ ", S. " ++ locator ++ " f.]")

    -- Returns "[@lorem, S. 100–102]".
    h id locator =
      RawInline (Format "markdown") (T.pack $ "[@" ++ id ++ ", S. " ++ locator ++ "]")

    -- Turns "(S. 100 f.)" into "[@lorem, S. 100 f.]".
    f id (Str "(S.":Space:Str number:Space:Str "f.)":xs) acc =
      (g id (T.unpack number):f id xs acc)

    -- Turns "(S. 100)" or "(S. 100–102)" into "[@lorem, S. 100]" or "[@lorem,
    -- S. 100–102]".
    f id (Str "(S.":Space:Str number:Str ")":xs) acc
      | matches   = (h id (T.unpack number):f id xs acc)
      | otherwise = (Str "(S.":Space:Str number:Str ")":f id xs acc)
      where
        matches = (T.unpack number) =~ regex :: Bool

    f id (x:xs) acc = (x:f id xs acc)

    f id [] acc = acc

    regex = "([0-9]+)$" :: String
