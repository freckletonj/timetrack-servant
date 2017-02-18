# Endpoints

- User - CRUD
- Login, get cookies (not JWT)
- Times

# Issues

# TODO

- auth roles
- serving files
- hide password:
  ```
  data FrontendUser = FrontEndUser User 
  instane ToJSON FrontendUser where toJSON (FrontEndUser User{..}) = object ["name" .= userName]
  ```
- logging
- replace JWT sessions with actual sessions

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

- db: multi-tenant user model

    https://www.getdonedone.com/building-the-optimal-user-database-model-for-your-application/

    login -> user <- membership -> account

    - login
        - email PRIMARY
        - pass hash+salt
    - user
        - email PRIMARY
        - name, other details
    - membership
        - ties to account
        - ties to roles
    - account
        - name
        - planlevel

    
- Don't Use JWT for Sessions

    http://cryto.net/~joepie91/blog/2016/06/19/stop-using-jwt-for-sessions-part-2-why-your-solution-doesnt-work/
