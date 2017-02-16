# Issues

Feb 15, 2017 - I'm taking a break on this `Servant` iteration. 
    - I like Servant a ton
    - I've found a bug that caused me to need to downgrade
    - There's a **lot** that I'd need to implement by hand to get this to work, versus a framework like Yesod
        - auth/roles/permissions, 
        - logging, 
        - metrics, 

# TODO

- user signup/login
- user owning of timeentries + auth
- auth roles
- serving files
- hide password:
  ```
  data FrontendUser = FrontEndUser User 
  instane ToJSON FrontendUser where toJSON (FrontEndUser User{..}) = object ["name" .= userName]
  ```
# Structure

- app/
    - main
- src/
    - API/
        - <name>.hs
        - Model.hs
    - API.hs
    - Config.hs
- test/
    - 

# resources

- servant persistent authentication jwt

    https://github.com/vishnuixm/servant-persistent-authentication-jwt-example/blob/master/src/Api.hs
