# Setup
- allow UUID in postgres

    `CREATE EXTENSION "uuid-ossp";`

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

- enforcing role security?
- enforcing row-level security?
- enforcing all security things in one place instead of sprinkled throughout

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

- Servant, Persistent, and DSLs (auth, access control, db failures)

    https://hbtvl.wordpress.com/2015/06/28/servant-persistent-and-dsls/
    https://www.reddit.com/r/haskell/comments/3a4qrl/libraryframework_suggestions_for_a_rest_api_in/cs9lujd/
    https://www.reddit.com/r/haskell/comments/3a4qrl/libraryframework_suggestions_for_a_rest_api_in/

- Auto incrementing ids on a per-user-basis

    http://stackoverflow.com/questions/42449568/how-do-i-create-autoincrementing-ids-in-a-composite-key-that-sequence-according
    http://stackoverflow.com/questions/41902775/how-create-a-column-that-increase-according-to-the-value-of-another-column/41914370#41914370
