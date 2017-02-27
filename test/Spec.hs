{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

{-
qualities --------------------------------------------------
headers
auth (sessions, jwts, roles, )
security (csrf token, affecting non-permitted data, ) <- best if this can be made into provable properties

properties --------------------------------------------------
idempotency - GET, PUT, DELETE (sorta)
(get . post) x = x
(delete . delete) = delete ?
(put . get . put) = put ?
(put . put) = put

lens laws:
-- Get-Put
set l (view l whole) whole == whole

-- Put-Get
view l (set l part whole) == part

-- Put-Put
set l part2 (set l part1) whole = set l part2 whole


client flow --------------------------------------------------
create user + login
foreign key utilization



-}


spec :: Spec
spec = with (return app) $ do
    describe "GET /users" $ do
        it "responds with 200" $ do
            get "/users" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
            get "/users" `shouldRespondWith` users
