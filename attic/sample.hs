{-# OPTIONS_GHC -F -pgmF rio #-}
module Main where

main :: IO ()
main = do
  bs :: ByteString <- B.readFile "attic/sample.hs"
  let text = decodeUtf8 bs
      m = Map.fromListWith (+) $ map (, 1 :: Int) (T.unpack text)
  mapM_ sayShow $ Map.toList m
  final <- forEach (0 :: Int)
    (\total x -> do
        if x <= 10
          then do
            say $ T.pack $ "New number is " <> show x
            return $ Continue $! total + x
          else return $ Break total)
    (FE.enumFromTo 1 1000000)
  sayShow final
