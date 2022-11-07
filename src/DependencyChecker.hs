module DependencyChecker
  ( ModName,
    fromHierarchy,
    Import (..),
    ImpType (..),
    Package,
    allImportDeclarations,
    verifyCleanArchitectureDependencies,
    verifyAllDependencies,
    verifyImportDecl,
    ppModule,
    allFiles,
    cleanArchitectureCompliantDeps,
    --cleanArchitecturePackages,
    formatLeftAsErrMsg,
    moduleNamed,
    importNamed,
  )
where

import           Data.Either    (partitionEithers)
import           Data.List      (intercalate)
import           FileUtils      (ModuleImportDeclarations, allFiles,
                                 allImportDeclarations)
import           Graphmod.Utils

-- | this type represents the package structure of a module e.g. Data.Time.Calendar resides in package Date.Time
type Package = String

generalBlacklist :: [Package]
generalBlacklist = ["System", "System.IO"]

data PackageDeclaration = PackageDeclaration
  { package    :: Package,
    importRule :: ExplicitImportRule
  }

data ExplicitImportRule = DenyAll | AllowAll | DenyExactly [Package] | AllowExactly [Package]

data OnionArchitecture = OnionArchitecture
  { domain    :: PackageDeclaration,
    usecase   :: PackageDeclaration,
    interface :: PackageDeclaration,
    external  :: PackageDeclaration
  }

defaultCleanArchitecture :: OnionArchitecture
defaultCleanArchitecture =
  OnionArchitecture
    { domain = PackageDeclaration "Domain" (DenyExactly generalBlacklist),
      usecase = PackageDeclaration "UseCases" (DenyExactly generalBlacklist),
      interface = PackageDeclaration "InterfaceAdapters" AllowAll,
      external = PackageDeclaration "ExternalInterfaces" AllowAll
    }

-- | the list of source packages in descending order from outermost to innermost package for an onion architecture.
onionPackages :: OnionArchitecture -> [Package]
onionPackages OnionArchitecture {domain = dom, usecase = uc, interface = intf, external = ext} =
  [package ext, package intf, package uc, package dom]

-- | verify a list of ModuleImportDeclarations to comply to the clean architecture dependency rules.
verifyCleanArchitectureDependencies :: [ModuleImportDeclarations] -> Either [ModuleImportDeclarations] ()
verifyCleanArchitectureDependencies =
  let cleanArchitecturePackages = onionPackages defaultCleanArchitecture
   in verifyAllDependencies
        cleanArchitecturePackages
        (cleanArchitectureCompliantDeps cleanArchitecturePackages)

-- | Right () is returned unchanged,
--   Left imports will be rendered as a human readable error message.
--   This function may be handy in test cases.
formatLeftAsErrMsg :: Either [ModuleImportDeclarations] () -> Either [String] ()
formatLeftAsErrMsg (Right ()) = Right ()
formatLeftAsErrMsg (Left imports) = Left (map toString imports)
  where
    toString :: ModuleImportDeclarations -> String
    toString (modName, imports) = ppModule modName ++ " imports " ++ intercalate ", " (map (ppModule . impMod) imports)

-- | for a given list of packages this function produces the set of all allowed dependency pairs between packages.
--   Allowed dependencies according to CleanArchitecture:
--   1. imports within the same package
--   2. imports from outer layers to inner layers
cleanArchitectureCompliantDeps :: [Package] -> [(Package, Package)]
cleanArchitectureCompliantDeps [] = []
cleanArchitectureCompliantDeps lst@(p : ps) = zip (repeat p) lst ++ cleanArchitectureCompliantDeps ps

-- | Verify the dependencies of a list of module import declarations.
--   The results are collected into an 'Either [(ModName, [Import])] ()'.
verifyAllDependencies :: [Package] -> [(Package, Package)] -> [ModuleImportDeclarations] -> Either [ModuleImportDeclarations] ()
verifyAllDependencies allPackages compliantDependencies imports = do
  let results = map (verifyImportDecl allPackages compliantDependencies) imports
  let (errs, _compliant) = partitionEithers results
  if null errs
    then Right ()
    else Left errs

-- | this function verifies all import declarations of a Haskell module.
--   If offending imports are found, a diagnostic error message is produced.
verifyImportDecl :: [Package] -> [(Package, Package)] -> ModuleImportDeclarations -> Either ModuleImportDeclarations ()
verifyImportDecl allPackages compliantDependencies (packageFrom, imports) =
  let offending = filter (not . verify packageFrom) imports
   in if null offending
        then Right ()
        else Left (packageFrom, offending)
  where
    verify :: ModName -> Import -> Bool
    verify pFrom imp =
      importPackage imp `notElem` allPackages
        || (modulePackage pFrom, importPackage imp) `elem` compliantDependencies --allowedDependencies packages

-- | this function returns the Package information from an Import definition
importPackage :: Import -> Package
importPackage imp = modulePackage (impMod imp)

-- | this function returns the Package information from a ModName definition
modulePackage :: ModName -> Package
modulePackage (q, _m) = intercalate "." (qualifierNodes q)

-- | this function pretty-prints a ModName as a fully qualified module name, e.g. 'Data.Time.Calendar'
ppModule :: ModName -> String
ppModule (q, m) = intercalate "." (qualifierNodes q ++ [m])

-- | creates a ModName instance from a String. '.' is interpreted as hierarchy separator.
--   E.G. moduleNamed "Control.Monad.Extra" returns (Hierarchy ["Control","Monad"],"Extra")
moduleNamed :: String -> ModName
moduleNamed name =
  let parts = split name
      hierarchy = init parts
      modName = last parts
   in (fromHierarchy hierarchy, modName)
  where
    split :: String -> [String]
    split str =
      case break (== '.') str of
        (a, _ : b) -> a : split b
        (a, _)     -> [a]

-- | creates an Import instance from a String. The String is expected to represent a qualified module name.
importNamed :: String -> Import
importNamed name = Import {impMod = moduleNamed name, impType = NormalImp}
