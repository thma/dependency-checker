module FileUtils where

import           Control.Monad.Extra    (concatMapM)
import           Graphmod.Utils         (Import, ModName, parseFile)
import           System.Directory.Extra (doesDirectoryExist, listDirectory)

-- | this type represents the section of import declaration at the beginning of a Haskell module
type ModuleImportDeclarations = (ModName, [Import])

-- | scan all files under filepath 'dir' and return a list of all their import declarations.
allImportDeclarations :: FilePath -> IO [ModuleImportDeclarations]
allImportDeclarations dir = do
  files <- allFiles dir
  mapM parseFile files

-- | list all files in the given directory and recursively include all sub directories
allFiles :: FilePath -> IO [FilePath]
allFiles dir = do
  files <- listDirectory dir
  let qualifiedFiles = map (\f -> dir ++ "/" ++ f) files
  concatMapM
    ( \f -> do
        isFile <- doesDirectoryExist f
        if isFile
          then allFiles f
          else return [f]
    )
    qualifiedFiles
