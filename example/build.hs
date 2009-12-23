{-# OPTIONS -O2 -Wall
    -fno-warn-missing-signatures
    -fno-warn-type-defaults
    -fno-warn-unused-binds #-}
{-# LANGUAGE GADTs #-}

import System.Posix(getFileStatus, modificationTime)
import Data.Maybe(fromMaybe)
import Data.List(intercalate)
import Data.Binary(Binary(..))

-- Target associates a Builder with a filename to store/cache its
-- output. The output type can be anything we can serialize.
--
-- Typically, most build intermediates would be serialized strings
-- (object files, generated sources, etc).
--
-- However, Targets may also be things like dependency lists, or other
-- non-strings.
--
-- Arrow approach:
--
-- - Can't be a general arrow, because I want every node's output to
--   be cached in a file. Similar trick to RMonad might work...
--
-- - Arrows are clumsy!
--
-- - ArrowApply is needed, to support dependency list generation
--   - So why not a Monad?
--
-- Monad approach:
--
-- - Need RMonad, how does that work?
--
-- - Should work?
data Target a = Binary a =>
                Target { targetFilePath :: FilePath
                       , targetBuilder :: Builder a }

data Builder :: * -> * where
  Pure :: a -> Builder a
  Action :: IO a

gccLine output inputs = "gcc -c -g -Wall -o " ++ output ++ " " ++ intercalate " " inputs

linker = error "not implemented"
compiler path sources = error "not implemented"
                        -- Target path $
                        -- CommandBuilder (gccLine path . map targetFilePath $ sources)
                        --                buildDepList
pureprocessor :: FilePath -> (a -> b) -> Target a -> Target b
pureprocessor path f src = do
  result <- src
  Target path . PureProcessor f $ src
sourcefile path = Target path SourceFile

executable = linker "exe" [a_o]
a_o = compiler "a.o" [a_c]
a_c = sourcefile "a.c"
a_h = pureprocessor "a.h" reverse reversed_a_h
reversed_a_h = sourcefile "a.h.reversed"

autoTargets = [("a.h", a_h)]
autoTarget = (`lookup` autoTargets)

tryMaybe :: IO a -> IO (Maybe a)
tryMaybe act = (Just `fmap` act) `catch` (const . return) Nothing

build (Target path builder) = do
  mMTime <- tryMaybe (modificationTime `fmap` getFileStatus path)
  case builder of
    SourceFile -> maybe (fail $ "Cannot find " ++ path) return mMTime
    PureProcessor processor input -> do
      putStrLn $ "Recomputing " ++ path
      inputMTime <- build input
      let mtime = fromMaybe 0 mMTime
      if mtime >= inputMTime
        then do
          putStrLn "  Already built."
          return mtime
        else do
          dat <- readFile (targetFilePath input)
          writeFile path (processor dat)
          modificationTime `fmap` getFileStatus path
    CommandBuilder cbline -> do
      

main = do
  build a_o
  return ()
