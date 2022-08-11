module Main where

import DependencyChecker

main :: IO ()
main = do
  --let allowed = cleanArchitectureCompliantDeps cleanArchitecturePackages

  print $ formatLeftAsErrMsg $ verifyCleanArchitectureDependencies [bogusDependency] 

  print $ formatLeftAsErrMsg $ verifyCleanArchitectureDependencies [goodDependency]


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
