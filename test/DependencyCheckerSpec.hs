module DependencyCheckerSpec where

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )
import DependencyChecker


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "The Dependency Checker" $ do
    it "makes sure all modules comply to the outside-in rule" $ do
      allImports <- allImportDeclarations "src"
      formatLeftAsErrMsg (verifyCleanArchitectureDependencies allImports) `shouldBe` Right ()
    it "finds non-compliant import declarations" $ do
      verifyCleanArchitectureDependencies [bogusDependency] `shouldBe`
        Left [bogusDependency]

-- | this instance represents a non-compliant dependency from the 'Domain' package to the 'ExternalInterfaces' package.
bogusDependency :: (ModName, [Import])
bogusDependency = (mod, [imp])
  where
    mod = moduleNamed "Domain.ReservationDomain"
    imp = importNamed "ExternalInterfaces.FileConfigProvider"

