import           File.Write
import           Network.URL
import           Test.Hspec

main :: IO ()
main = hspec $ do
  specWriteFileChangeFileName
  specWriteFileGetFileName
  specGetBaseUrl
  specAddBaseUrl
  specUrlToDirName

specUrlToDirName :: Spec
specUrlToDirName = do
  describe "Data.URL.urlToDirName" $ do
    it "http" $ do
      urlToDirName "http://site.com" `shouldBe` "site_com"
    it "https" $ do
      urlToDirName "https://site.com" `shouldBe` "site_com"
    it "without http(s)" $ do
      urlToDirName "site.com" `shouldBe` "site_com"

specAddBaseUrl :: Spec
specAddBaseUrl = do
  describe "Data.URL.addBaseUrl" $ do
    it "not add" $ do
      addBaseUrl "http://site.com/image.png" "http://site.com"
        `shouldBe` "http://site.com/image.png"
    it "add" $ do
      addBaseUrl "/image.png" "http://site.com"
        `shouldBe` "http://site.com/image.png"
    it "url starts //" $ do
      addBaseUrl "//site.com/file.jpg" "https://site.com"
        `shouldBe` "https://site.com/file.jpg"
      addBaseUrl "//site.com/file.jpg" "http://site.com"
        `shouldBe` "http://site.com/file.jpg"

specGetBaseUrl :: Spec
specGetBaseUrl = do
  describe "Data.URL.getBaseUrl" $ do
    it "http" $ do
      getBaseUrl "http://site.com" `shouldBe` "http://site.com"
      getBaseUrl "http://site.com/" `shouldBe` "http://site.com"
      getBaseUrl "http://site.com/page" `shouldBe` "http://site.com"
      getBaseUrl "http://site.com?a=a" `shouldBe` "http://site.com"

    it "https" $ do
      getBaseUrl "https://site.com" `shouldBe` "https://site.com"
      getBaseUrl "https://site.com/" `shouldBe` "https://site.com"
      getBaseUrl "https://site.com/page" `shouldBe` "https://site.com"
      getBaseUrl "https://site.com?a=a" `shouldBe` "https://site.com"


specWriteFileGetFileName :: Spec
specWriteFileGetFileName = do
  describe "WriteFile.getFileName" $ do
    it "empty file path" $ do
      getFileName "" `shouldBe` ""
    it "file url with http" $ do
      getFileName "http://localhost/dir/dir2/file" `shouldBe` "file"
    it "file url with https" $ do
      getFileName "https://localhost/dir/dir2/file" `shouldBe` "file"
    it "file path starts with /" $ do
      getFileName "/dir/dir2/file" `shouldBe` "file"
    it "file path doesn't starts with /" $ do
      getFileName "dir/dir2/file" `shouldBe` "file"

specWriteFileChangeFileName :: Spec
specWriteFileChangeFileName = do
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
