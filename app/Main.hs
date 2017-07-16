{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import DeadLift
import qualified DeadLift.Map as Map
import qualified DeadLift.Set as Set
import qualified Data.Text as T
import qualified Data.ByteString as B

qualifieds :: Map Text Text
qualifieds = Map.fromList
  [ ("B", "DeadLift.ByteString")
  , ("HashMap", "DeadLift.HashMap")
  , ("HashSet", "DeadLift.HashSet")
  , ("Map", "DeadLift.Map")
  , ("Set", "DeadLift.Set")
  , ("V", "DeadLift.Vector")
  , ("T", "DeadLift.Text")
  , ("FE", "DeadLift.ForEach")
  , ("Proc", "DeadLift.Process")
  ]

extensions :: Set Text
extensions = Set.fromList
  [ "NoImplicitPrelude"
  , "OverloadedStrings"
  , "GeneralizedNewtypeDeriving"
  , "DeriveFunctor"
  , "DeriveTraversable"
  , "DeriveFoldable"
  , "DeriveGeneric"
  , "ViewPatterns"
  , "GADTs"
  , "TypeFamilies"
  , "ScopedTypeVariables"
  , "BangPatterns"
  , "PatternGuards"
  , "RankNTypes"
  , "ExistentialQuantification"
  , "KindSignatures"
  , "EmptyDataDecls"
  , "StandaloneDeriving"
  , "DeriveDataTypeable"
  , "FlexibleContexts"
  , "FlexibleInstances"
  , "ConstraintKinds"
  , "MultiParamTypeClasses"
  , "FunctionalDependencies"
  , "PackageImports"
  , "ConstraintKinds"
  , "DataKinds"
  , "DefaultSignatures"
  , "InstanceSigs"
  , "LambdaCase"
  , "MultiWayIf"
  , "PartialTypeSignatures"
  , "TupleSections"
  ]

main :: IO ()
main = do
  [orig, input, output] <- getArgs
  B.readFile input >>=
      B.writeFile output
    . encodeUtf8
    . T.unlines
    . tweak orig
    . zip (map Just [1..])
    . T.lines
    . T.filter (/= '\r')
    . decodeUtf8

tweak :: FilePath -- ^ orig
      -> [(Maybe Int, Text)] -> [Text]
tweak orig input =
  addLines orig $ addLangs $ addImports used input
  where
    used = Map.unions $ map (findUsed . snd) input

addLangs :: [(Maybe Int, Text)] -> [(Maybe Int, Text)]
addLangs input
  | null after = langs : input
  | otherwise = before ++ langs : after
  where
    (before, after) = break (isLang . snd) input

    langs = (Nothing, T.concat
                [ "{-# LANGUAGE "
                , T.intercalate ", " (Set.toList extensions)
                , " #-}"
                ])

    isLang :: Text -> Bool
    isLang t0 = fromMaybe False $ do
      t1 <- T.stripPrefix "{-#" t0
      let t2 = T.strip t1
      _ <- T.stripPrefix "LANGUAGE" t2
      return True

addImports :: Map Text Text
           -> [(Maybe Int, Text)] -> [(Maybe Int, Text)]
addImports used input =
  case break (isImport . snd) input of
    (before, after) | not (null after) ->
      before ++ imports ++ after
    _ ->
      case breakModule input of
        (before, after) -> before ++ imports ++ after
  where
    isImport = ("import " `T.isPrefixOf`)
    imports = (Nothing, "import DeadLift") : map
      (\(k, v) -> (Nothing, T.concat
        [ "import qualified "
        , v
        , " as "
        , k
        ]))
      (Map.toList used)

findUsed :: Text -> Map Text Text
findUsed l =
  Map.filterWithKey p qualifieds
  where
    p k _ = (k <> ".") `T.isInfixOf` l

addLines :: FilePath -> [(Maybe Int, Text)] -> [Text]
addLines _ [] = []
addLines orig ((Just i, x):xs) =
  T.pack pragma : x : addLines orig xs
  where
    pragma = concat
      [ "{-# LINE "
      , show i
      , " "
      , show orig
      , " #-}"
      ]
addLines orig ((Nothing, x):xs) =
  x : addLines orig xs

breakModule :: [(a, Text)] -> ([(a, Text)], [(a, Text)])
breakModule input = fromMaybe ([], input) $ do
  let (beforeModule, afterModule) = break (isModule . snd) input
  guard $ not $ null afterModule
  case break (hasWhere . snd) afterModule of
    (_, []) -> Nothing
    (beforeWhere, withWhere:afterWhere) ->
      Just (beforeWhere ++ [withWhere], afterWhere)
  where
    isModule = ("module " `T.isPrefixOf`)
    hasWhere = ("where" `T.isInfixOf`)
