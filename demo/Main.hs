module Main where

import           DependencyChecker

main :: IO ()
main = do
  print $ formatLeftAsErrMsg $ verifyCleanArchitectureDependencies [bogusDependency]

  print $ formatLeftAsErrMsg $ verifyCleanArchitectureDependencies [goodDependency]

  allImports <- allImportDeclarations "src"
  -- print allImports
  print $ formatLeftAsErrMsg $ Left allImports
  print $ verifyAllDependencies dcPacks dcAllowed allImports

-- print $
--   onionArchitecture
--     domainModels ["Domain"]
--     useCase ["UseCases"]
--     interfaceAdapters ["InterfaceAdapters"]
--     externalInterfaces ["ExternalInterfaces"]

dcPacks = ["", "Control.Monad", "Data", "System", "Graphmod"]

dcAllowed = [("", "Control.Monad"), ("", "Data"), ("", "System"), ("", "Graphmod")]

generalBlacklist :: [Package]
generalBlacklist = ["System", "System.IO"]

data PackageDeclaration = PackageDeclaration Package ExplicitImportRule

data ExplicitImportRule = DenyAll | AllowAll | DenyExactly [Package] | AllowExactly [Package]

data OnionArchitecture = OnionArchitecture
  { domain    :: PackageDeclaration,
    usecase   :: PackageDeclaration,
    interface :: PackageDeclaration,
    external  :: PackageDeclaration
  }

polysemyCleanArchitecture :: OnionArchitecture
polysemyCleanArchitecture =
  OnionArchitecture
    { domain = PackageDeclaration "Domain" (DenyExactly generalBlacklist),
      usecase = PackageDeclaration "UseCases" (DenyExactly generalBlacklist),
      interface = PackageDeclaration "InterfaceAdapters" AllowAll,
      external = PackageDeclaration "ExternalInterfaces" AllowAll
    }

dcDeps :: (ModName, [Import])
dcDeps = (mod, [cm, de, dl, sd, gu])
  where
    mod = moduleNamed "DependencyChecker"
    cm = importNamed "Control.Monad.Extra"
    de = importNamed "Data.Either"
    dl = importNamed "Data.List"
    sd = importNamed "System.Directory"
    gu = importNamed "Graphmod.Utils"

goodDependency :: (ModName, [Import])
goodDependency = (mod, [imp])
  where
    mod = moduleNamed "ExternalInterfaces.FileConfigProvider"
    imp = importNamed "Domain.ReservationDomain"

bogusDependency :: (ModName, [Import])
bogusDependency = (mod, [imp])
  where
    mod = moduleNamed "Domain.ReservationDomain"
    imp = importNamed "ExternalInterfaces.FileConfigProvider"
