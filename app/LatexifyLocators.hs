{-# LANGUAGE OverloadedStrings #-}
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter latexifyLocators


latexifyLocators :: Maybe Format -> Inline -> Inline
latexifyLocators (Just (Format "latex")) (Cite citations inlines) =
  Cite (map latexifyLocator citations) inlines
latexifyLocators _ x = x


{-|
  Replaces locators like "S. 10", "S. 10 f.", or "S. 10–12" by "S.~10",
  "S.~10\,f.", or "S.~10–12" respectively.

  This makes them typographically correct.
-}
latexifyLocator :: Citation -> Citation
latexifyLocator citation =
  citation { citationSuffix = latexifySuffix $ citationSuffix citation }


latexifySuffix :: [Inline] -> [Inline]
latexifySuffix suffix = f suffix []
  where
    -- Turns "S. 100 f." into "S.~100\,f.".
    f (Str "S.":Space:Str number:Space:Str "f.":xs) acc =
      (RawInline (Format "latex") "S.~":Str number:RawInline (Format "latex") "\\,f.":f xs acc)
    -- Turns "S. 100" into "S.~100" (also matches "S. 100–102").
    f (Str "S.":Space:Str number:xs) acc =
      (RawInline (Format "latex") "S.~":Str number:f xs acc)
    f (x:xs) acc = (x:f xs acc)
    f [] acc     = acc
