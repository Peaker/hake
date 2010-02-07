{-# OPTIONS -O2 -Wall #-}

import System.Posix(getFileStatus, modificationTime, EpochTime, FileStatus)
import Data.List(intercalate)
import qualified Data.ByteString as BS
import Data.Binary(Binary(..), encodeFile, decodeFile)
import qualified System.Process as Process
import System.Exit(ExitCode(..))
import Control.Monad.Writer(WriterT(..), tell)
import Max(Max(..), AddBounds(..))
import FileData(inFileData)
import Control.Monad.Trans(MonadIO, liftIO)

type IOM a = IO (Maybe a)

tryMaybe :: IO a -> IOM a
tryMaybe act = (Just `fmap` act) `catch`
               const (return Nothing)

io :: (MonadIO m) => IO a -> m a
io = liftIO
  
type Build a = WriterT (Max (AddBounds EpochTime)) IO a

runBuild :: Build a -> IO a
runBuild = fmap fst . runWriterT

mFileStatus :: FilePath -> IOM FileStatus
mFileStatus = tryMaybe . getFileStatus

mmTime :: FilePath -> IOM EpochTime
mmTime = (fmap . fmap) modificationTime . mFileStatus

whenBuildNecessary :: AddBounds EpochTime -> FilePath -> IO () -> IO ()
whenBuildNecessary inputMTime outputPath action = do
  outputMTime <- mmTime outputPath
  case outputMTime of
    Nothing -> rebuild "missing output"
    Just t ->
      if Unbound t < inputMTime
      then rebuild "older output"
      else keep "output is newer"
  where
    keep reason = do
      putStrLn $ "Not rebuilding (" ++ reason ++ ")"
    rebuild reason = do
      putStrLn $ "Rebuilding (" ++ reason ++ ")"
      action

build :: FilePath -> Build a -> (a -> IO ()) -> Build FilePath
build outputPath inputsBuild outputAction = do
  modTime <- io $ do
    (r, newestInputMTime) <- runWriterT inputsBuild
    whenBuildNecessary (unMax newestInputMTime) outputPath (outputAction r)
    modificationTime `fmap` getFileStatus outputPath
  tell . Max . Unbound $ modTime
  return outputPath

apply :: Binary a => FilePath -> (a -> a) -> Build FilePath -> Build FilePath
apply outputPath func inputBuild = build outputPath inputBuild action
  where
    action inputFile =
      encodeFile outputPath =<< func `fmap` decodeFile inputFile

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

compiler :: FilePath -> [Build FilePath] -> Build FilePath
compiler outputPath inputs = build outputPath (sequence inputs) action
  where
    action inputFiles = buildCommand . gccLine outputPath $ inputFiles

a_c :: Build FilePath
a_c = return "a.c"
reversed_a_h :: Build FilePath
reversed_a_h = return "a.h.reversed"
a_h :: Build FilePath
a_h = apply "a.h" (inFileData BS.reverse) reversed_a_h
a_o :: Build FilePath
a_o = compiler "a.o" [a_c]
-- executable = build a_o >>= linker "exe"

main :: IO ()
main = do
  runBuild a_h
  runBuild a_o
  return ()
