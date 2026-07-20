module Utils where

import qualified Data.Char as C (toUpper, isDigit, isSpace)
import qualified Data.List as L -- (map, zip, concat, reverse, init, takeWhile, words, unwords, sortOn, intersperse)
import qualified Data.Set  as S (toList, fromList)
import qualified Data.Map  as M (toList, fromListWith)
import Text.JSON (JSON, JSValue(..), makeObj, toJSString, encode)

setnub :: Ord a => [a] -> [a]
setnub = S.toList . S.fromList

-- replacing INDEXEDTERM with the $ expression given in Env
unindexString :: [String] -> String -> String
unindexString tindex = L.unwords . findterms . L.words
  where
    findterms ws = case ws of
      "\\INDEXEDTERM" : ('{' : cs) : ww -> (tindex !! (Prelude.read (L.init cs))) : findterms ww
      "\\INDEXEDTERM" : "{" : cs : "}" : ww -> (tindex !! (Prelude.read (L.init cs))) : findterms ww --- different unlexing
      w : ww -> w : findterms ww
      _ -> ws

-- for LaTeX, Agda, etc
snake2camel :: String -> String
snake2camel = L.concat . capit . L.words . uncamel where
  uncamel = L.map (\c -> if c == '_' then ' ' else c)
  capit (w:ws) = w : [C.toUpper c : cs | (c:cs) <- ws]

frequencyTable :: Ord a => [a] -> [(a, Int)]
frequencyTable xs = L.sortOn (\ (_, i) -> -i) $ M.toList $ M.fromListWith (+) [(x, 1) | x <- xs]

showFreqs :: [(String, Int)] -> [String]
showFreqs = L.map (\ (c, n) -> c ++ "\t" ++ Prelude.show n)

commaSepInts :: String -> [Int]
commaSepInts s =
  let ws = commaSep s in
  if all (all C.isDigit) ws
  then L.map Prelude.read ws
  else error ("expected digits found " ++ s)

fileSuffix = L.reverse . L.takeWhile (/= '.') . L.reverse

commaSep s = L.words (L.map (\c -> if c==',' then ' ' else c) s)

toLatexDoc :: [String] -> [String] -> [String]
toLatexDoc ms ss = latexPreamble ++ ms ++ ss ++ [latexEndDoc]
  where
    latexPreamble = [
      "\\batchmode",
      "\\documentclass{article}",
      "\\usepackage{amsfonts}",
      "\\usepackage{amssymb}",
      "\\usepackage{amsmath}",
      "\\setlength\\parindent{0pt}",
      "\\setlength\\parskip{8pt}",
      "\\begin{document}",
      "\\newcommand{\\meets}{\\mathrel{\\supset\\!\\!\\!\\subset}}",
      "\\newcommand{\\notmeets}{\\mathrel{\\not\\meets}}"
      ]
    latexEndDoc = "\\end{document}"


mkJSONObject :: [(String, JSValue)] -> JSValue
mkJSONObject fields = makeObj fields

mkJSONField :: String -> JSValue -> (String, JSValue)
mkJSONField key value = (key, value)

mkJSONListField :: String -> [JSValue] -> (String, JSValue)
mkJSONListField key values = mkJSONField key (JSArray values)

stringJSON :: String -> JSValue
stringJSON s = JSString (toJSString s)

encodeJSON :: JSON a => a -> String
encodeJSON = encode

transInEnv :: String -> ([String] -> String) -> [String] -> [String]
transInEnv env trans = chop
 where
  chop ss = case Prelude.break ((== "\\begin{" ++ env ++ "}") . strip) ss of
    (ls, []) -> ls
    (ls, rest) -> ls ++ case Prelude.break ((== "\\end{" ++ env ++ "}") . strip) rest of
      (ds, line : rest) -> trans (ds ++ [line]) : chop rest
      (ds, []) -> ds


-- for generating valid LaTeX
escapeUnderscores :: String -> String
escapeUnderscores = L.concat . L.map (\c -> if c=='_' then "\\_" else [c])

-- for converting back to Dedukti
unescapeUnderscores :: String -> String
unescapeUnderscores s = case s of
  '\\':'_':cs -> '_':unescapeUnderscores cs
  c:cs -> c:unescapeUnderscores cs
  _ -> s


-- like Python strip()
strip :: String -> String
strip = L.unwords . L.words


-- like Python split();  Data.List.Split cannot be found...
split :: Char -> String -> [String]
split c cs = case Prelude.break (==c) cs of
  ([], []) -> []
  (s,  []) -> [strip s]
  (s, _:s2) -> strip s : split c s2

-- split with c outside a given lim env, such as $..$
splitOutside lim c str = L.filter (Prelude.not . Prelude.null) (gather segments)
  where
    s = L.dropWhile C.isSpace str
    startlim = if (L.take 1 s == [lim]) then 1 else 0
    handle (seg, i) = if (Prelude.even i) then split c seg else [lim : seg ++ [lim]]
    segments = L.filter (Prelude.not . Prelude.null) (split lim s)
    gather segs = L.concatMap handle (L.zip segs [startlim ..])

-- Python-like dict values from line by line from e.g. symbol tables
dictValues :: String -> [String]
dictValues = L.map (L.drop 1 . L.dropWhile (/= ':')) . L.lines
