import           IO.WriteFile
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "WriteFile.changeFileName" $ do
    it "added (1)" $ do
      changeFileName "file.txt" `shouldBe` "file(1).txt"
    it "increase by one file(1).txt" $ do
      changeFileName "file(1).txt" `shouldBe` "file(2).txt"
    it "increase by one file(10).txt" $ do
      changeFileName "file(10).txt" `shouldBe` "file(11).txt"

    it "added (1) withoun extention" $ do
      changeFileName "file" `shouldBe` "file(1)"
    it "increase by one file(1) withoun extention" $ do
      changeFileName "file(1)" `shouldBe` "file(2)"
    it "increase by one file(10) withoun extention" $ do
      changeFileName "file(10)" `shouldBe` "file(11)"

    it "added (1) double extention" $ do
      changeFileName "file.txt.md" `shouldBe` "file(1).txt.md"
    it "increase by one file(1).txt.md double extention" $ do
      changeFileName "file(1).txt.md" `shouldBe` "file(2).txt.md"
    it "increase by one file(10).txt.md double extention" $ do
      changeFileName "file(10).txt.md" `shouldBe` "file(11).txt.md"

    it "added (1) extra bracket" $ do
      changeFileName "f(il)e.txt.md" `shouldBe` "f(il)e(1).txt.md"
    it "increase by one f(il)e(1).txt.md double extention" $ do
      changeFileName "f(il)e(1).txt.md" `shouldBe` "f(il)e(2).txt.md"
    it "increase by one f(il)e(10).txt.md double extention" $ do
      changeFileName "f(il)e(10).txt.md" `shouldBe` "f(il)e(11).txt.md"
