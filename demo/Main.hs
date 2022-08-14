module Main where

import DependencyChecker

main :: IO ()
main = do

  print $ formatLeftAsErrMsg $ verifyCleanArchitectureDependencies [bogusDependency] 

  print $ formatLeftAsErrMsg $ verifyCleanArchitectureDependencies [goodDependency]

  allImports <- allImportDeclarations "src"
 -- print allImports
  print $ formatLeftAsErrMsg $ Left allImports
  print $ verifyAllDependencies dcPacks dcAllowed allImports

dcPacks = ["", "Control.Monad", "Data", "System", "Graphmod"]

dcAllowed = [("", "Control.Monad"), ("", "Data"), ("", "System"), ("","Graphmod")]

dcDeps :: (ModName, [Import])
dcDeps = (mod, [cm,de,dl,sd,gu]) 
  where
    mod = (fromHierarchy [],"DependencyChecker")
    cm  = Import {impMod = (fromHierarchy ["Control", "Monad"],"Extra"), impType = NormalImp }
    de  = Import {impMod = (fromHierarchy ["Data"],"Either"), impType = NormalImp}
    dl  = Import {impMod = (fromHierarchy ["Data"],"List"), impType = NormalImp}
    sd  = Import {impMod = (fromHierarchy ["System"],"Directory"), impType = NormalImp}
    gu  = Import {impMod = (fromHierarchy ["Graphmod"],"Utils"), impType = NormalImp}


goodDependency :: (ModName, [Import])
goodDependency = (mod, [imp])
  where
    mod =  (fromHierarchy ["ExternalInterfaces"],"FileConfigProvider")
    imp = Import { impMod = (fromHierarchy ["Domain"],"ReservationDomain"), impType = NormalImp }

bogusDependency :: (ModName, [Import])
bogusDependency = (mod, [imp])
  where
    mod =  (fromHierarchy ["Domain"],"ReservationDomain")
    imp = Import { impMod = (fromHierarchy ["ExternalInterfaces"],"FileConfigProvider"), impType = NormalImp }  
