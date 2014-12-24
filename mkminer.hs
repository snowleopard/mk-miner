import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Data.Function (on)
import Control.Monad
import Data.Functor
import Data.List
import Data.Ord (comparing)
import Data.Char

-- Directories
sourceTree    = ".." </> ".."
results       = "results"
code          = results </> "code"
generatedVars = results </> "vars"

-- Files
todo    = "todo.txt"
allmk   = results </> "allmk.txt"
mklist  = results </> "makefiles.txt"
summary = results </> "summary.txt"

-- Files to look for
mkfiles = ["*.mk", "*.mk.in", "Makefile", "GNUmakefile"]

none :: (a -> Bool) -> [a] -> Bool
none = (not .). any

preprocessMk :: FilePath -> [String] -> String
preprocessMk f xs =
    unlines
    $ map (++ " # " ++ f')
    $ filter useful
    $ map comments
    $ continuations xs
  where
    useful s =
        (  any  (`isPrefixOf` s) goodPrefixes
        || any  (`isInfixOf`  s) goodInfixes)
        && none (`isPrefixOf` s) badPrefixes
    badPrefixes  = ["if " , "echo ", "@"]
    goodPrefixes = ["if", "else", "endif"]
    goodInfixes  = ["="]
    f' = dropDirectory1 $ dropDirectory1 f

    -- adapted from Neil Mitchell's parser
    continuations (x:y:xs) | "\\" `isSuffixOf` x =
        continuations $ (init x ++ dropWhile isSpace y):xs
    continuations (x:xs) = (dropWhile isSpace x):continuations xs
    continuations [] = []

    comments = takeWhile (/= '#')

getVar :: String -> [String] -> [String] -> [String]
getVar _ [] [] = []
getVar _ _  [] = error "No matching endif found."
getVar var ifs (x:xs) 
    | "if"    `isPrefixOf` x = getVar var (x:ifs) xs
    | "else"  `isPrefixOf` x = getVar var ((head ifs ++ "\n" ++ x):tail ifs) xs `unlessNull` ifs
    | "endif" `isPrefixOf` x = getVar var (tail ifs) xs `unlessNull` ifs
    | found var              = reverse (x:ifs) ++ replicate (length ifs) "endif" ++ getVar var ifs xs
    | otherwise              = getVar var ifs xs
      where
        unlessNull body list = if null list then error "No matching if found." else body
        found var = isInfixOf var $ takeWhile (/= '=') x

generateHs :: [String] -> [String]
generateHs s =
    map join
    $ groupBy ((==) `on` fst) 
    $ sortBy (comparing fst)
    $ go [] s
  where
    join xs = var ++ " = " ++ intercalate "\n    ++ " list ++ "\n"
      where
        ((var:_), list) = unzip xs

    go :: [String] -> [String] -> [(String, String)]
    go _ [] = []
    go ifs (x:xs)
        | "ifeq"  `isPrefixOf` x = go ((condition 4 x):ifs) xs
        | "ifneq" `isPrefixOf` x = go ((invert $ condition 5 x):ifs) xs
        | "else"  `isPrefixOf` x = go ((invert $ head ifs):tail ifs) xs
        | "endif" `isPrefixOf` x = go (tail ifs) xs
        | otherwise              =
            (varName x, 
            "[ "
            ++ quote (expression x)
            ++ " | "
            ++ intercalate  ", " (map insertEq ifs)
            ++ " ]"
            ++ comment x) : go ifs xs
          where
            condition skip = takeWhile (/= '#') . dropWhile isSpace . drop skip
            invert s
                | "not $ " `isPrefixOf` s = drop 6 s
                | otherwise             = "not $ " ++ s
            varName = takeWhile (not . isSpace) . takeWhile (/= '=')
            expression = dropWhile isSpace . drop 1 . dropWhile (/= '=') . takeWhile (/= '#')
            comment = ("    --" ++) . reverse . takeWhile (/= '#') . reverse
            quote s = if ("\"" `isPrefixOf` s) then s else  "\"" ++ s ++  "\""
            insertEq s = reverse $ a ++ " == " ++ b
              where
                (a, b) = getLast False $ dropWhile isSpace $ reverse s
                x `append` (a, b) = (x:a, b)
                getLast False (x:xs)
                    | x == ' '  = ("", xs)
                    | x == '\"' = x `append` getLast True xs
                    | otherwise = x `append` getLast False xs
                getLast True (x:xs)
                    | x == '\"' = x `append` getLast False xs
                    | otherwise = x `append` getLast True xs

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    want [summary]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    summary %> \out -> do
        vars <- readFileLines todo
        let varFiles = map (\v -> generatedVars </> v <.> "var") vars
        need varFiles
        foundVars <- getDirectoryFiles "" ["//*.var"]
        let hsFiles = map ((code </>) . (<.> "hs") . takeBaseName) foundVars
        need hsFiles
        processedFiles <- readFileLines mklist
        writeFileChanged out $
            "# Found definitions for the following variables:\n\n"
            ++ unlines foundVars
            ++ "\n# Processed the following makefiles:\n\n"
            ++ unlines processedFiles
            ++ "\n# Generated the following Haskell files:\n\n"
            ++ unlines hsFiles

    [mklist, allmk] &%> \_ -> do
        alwaysRerun
        putNormal "Processing *all* makefiles..."
        res <- (lines . fromStdout)
            <$> (cmd Shell "find" [sourceTree] $ intersperse " -o " $ map ("-name " ++) mkfiles)
        let mks = filter ((`notElem` res) . (<.> "in")) res
        writeFileChanged mklist $ unlines mks
        preprocessedMks <- concat <$> mapM (\f -> preprocessMk f <$> readFileLines f) mks
        writeFileChanged allmk preprocessedMks

    generatedVars </> "*.var" %> \out -> do
        putNormal $ "Generating " ++ out
        contents <- readFileLines allmk
        writeFileChanged out $ unlines $ getVar (takeBaseName out) [] contents

    code </> "*.hs" %> \out -> do
        putNormal $ "Generating " ++ out
        let varFile = generatedVars </> takeBaseName out -<.> "var"
        contents <- readFileLines varFile 
        writeFileChanged out $ unlines $ generateHs contents
