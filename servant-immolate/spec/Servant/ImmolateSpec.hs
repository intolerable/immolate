module Servant.ImmolateSpec where

import Immolate
import Servant.API
import Test.Hspec
import Test.Hspec.Wai
import Servant

import Servant.Immolate
import Servant.Immolate.Links

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Servant.Immolate.ContentType" do

    with (pure (serve (Proxy :: Proxy ImmolateAPI) testServer)) do

      it "renders an instance of ToHtml" do
        get "to-html" `shouldRespondWith` "5"

      it "renders an actual Html ()" do
        get "/" `shouldRespondWith`
          "<h1>test page</h1><p>this is some content</p>"

  describe "Servant.Immolate.Links" do

    describe "addBaseUrl" do

      it "handles an empty base URL by simply using the path" do
        addBaseUrl "" "test/something" `shouldBe` "test/something"

      it "prepends the base URL if it has a trailing slash" do
        addBaseUrl "http://google.com/" "example/path" 
          `shouldBe` "http://google.com/example/path"

      it "prepends the base URL with a trailing slash if it doesn't have one" do
        addBaseUrl "example.com" "example/path" 
          `shouldBe` "example.com/example/path"

testServer :: Server ImmolateAPI
testServer = pure 5 :<|> pure exampleHtml
  where
    exampleHtml = do
      h1_ "test page"
      p_ "this is some content"

type ImmolateAPI = 
  "to-html" :> Get '[HTML] Integer :<|>
  Get '[HTML] (Html ())