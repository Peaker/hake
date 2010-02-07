{-# OPTIONS -O2 -Wall #-}

import System.Posix(getFileStatus, modificationTime, EpochTime)
import Data.List(intercalate)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Data.Binary(Binary(..), encodeFile, decodeFile)
import Data.Binary.Get(getByteString, remaining)
import Data.Binary.Put(putByteString)
import qualified System.Process as Process
import System.Exit(ExitCode(..))

newtype FileData = FileData { unFileData :: ByteString }
inFileData :: (ByteString -> ByteString)
              -> FileData -> FileData
inFileData f = FileData . f . unFileData

instance Binary FileData where
  get = FileData `fmap` (getByteString . fromIntegral =<< remaining)
  put = putByteString . unFileData

data Target = Target { targetFilePath :: FilePath,
                       targetBuild :: IO () }

data BuildResult = BuildResult { buildResultMTime :: EpochTime
                               , buildResultPath :: FilePath }

sourceFile :: FilePath -> Target
sourceFile path = Target path (getFileStatus path >> return ())

whenBuildNecessary :: EpochTime -> FilePath -> IO () -> IO ()
whenBuildNecessary inputMTime outputPath action = do
  outputMTime <- tryMaybe (modificationTime `fmap` getFileStatus outputPath)
  case outputMTime of
    Nothing -> rebuild "missing output"
    Just t ->
      if t < inputMTime
      then rebuild "older output"
      else putStrLn "Not rebuilding (output is newer than input)"
  where
    rebuild reason = do
      putStrLn $ "Rebuilding (" ++ reason ++ ")"
      action

apply :: Binary a => (a -> a) -> FilePath -> BuildResult -> Target
apply func outputPath (BuildResult inputMTime inputPath) = Target outputPath $ do
  whenBuildNecessary inputMTime outputPath $ do
    encodeFile outputPath =<< func `fmap` decodeFile inputPath

build :: Target -> IO BuildResult
build (Target path action) = do
  action
  mtime <- modificationTime `fmap` getFileStatus path
  return (BuildResult mtime path)

gccLine :: FilePath -> [FilePath] -> String
gccLine output inputs = "gcc -c -g -Wall -o " ++ output ++ " " ++ intercalate " " inputs

buildCommand :: String -> IO ()
buildCommand cmd = do
  putStrLn $ "Executing " ++ cmd ++ "..."
  exitCode <- Process.system cmd
  case exitCode of
    ExitSuccess -> putStrLn "Success!"
    ExitFailure rc -> do
      putStrLn $ "Failed with " ++ show rc ++ "!"
      undefined

compiler :: FilePath -> [BuildResult] -> IO Target
compiler outputPath inputs = return . Target outputPath $ do
  whenBuildNecessary (maximum (map buildResultMTime inputs)) outputPath $ do
    buildCommand . gccLine outputPath . map buildResultPath $ inputs

-- executable = build a_o >>= linker "exe"
a_o :: IO Target
a_o = compiler "a.o" . return =<< build a_c
a_c :: Target
a_c = sourceFile "a.c"
reversed_a_h :: Target
reversed_a_h = sourceFile "a.h.reversed"
a_h :: IO Target
a_h = apply (inFileData BS.reverse) "a.h" `fmap` (build reversed_a_h)

tryMaybe :: IO a -> IO (Maybe a)
tryMaybe act = (Just `fmap` act) `catch` (const . return) Nothing

main :: IO ()
main = do
  build =<< a_h
  build =<< a_o
  return ()
