{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Site where

import            Control.Applicative
import            Control.Monad
import            Control.Monad.State
import            Data.Monoid (Monoid, mappend)
import            Data.ByteString.Char8 (unpack)
import            Data.Char
import            Data.Lens.Template
import            Data.Map (Map)
import qualified  Data.Map as DM
import            Data.Maybe
import            Data.Text (Text)
import qualified  Data.Text as DT
import qualified  Data.Text.Encoding as DT
import            Database.HDBC.Sqlite3
import            Snap.Core
import            Snap.Snaplet
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Auth.Backends.Hdbc
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe
import            Text.Blaze
import qualified  Text.Blaze.Html5 as H
import qualified  Text.Blaze.Html5.Attributes as A
import            Text.Blaze.Internal (HtmlM(..))
import            Text.Blaze.Renderer.Utf8 (renderHtml)
import            Text.Digestive
import            Text.Digestive.Blaze.Html5
import            Text.Digestive.Forms.Snap
import qualified  Text.Email.Validate as E

------------------------------------------------------------------------------
-- | Types
data User = User
  {  email     :: Text
  ,  password  :: Text }
  deriving Show

data Profile = Profile
  { dbId :: Maybe Int
  , pName :: Text
  , pDescription :: Text
  , pExperience :: Text 
  , pTwitter :: Text }
  deriving Show

data App = App
  {  _auth     :: Snaplet (AuthManager App)
  ,  _appsess  :: Snaplet SessionManager
  ,  _hdbc     :: Snaplet (HdbcSnaplet Connection IO)
  }

makeLens ''App

instance HasHdbc AppHandler Connection IO where
  getConnSrc = with hdbc $ gets connSrc


type AppHandler = Handler App App


------------------------------------------------------------------------------
-- | Forms
loginForm :: Form AppHandler SnapInput Html BlazeFormHtml User
loginForm = User
  <$>  label  "Email address: "
       ++>  inputText Nothing `validate` isEmail
       <++  errors
  <*>  label  "Password: "
       ++>  inputPassword False `validateMany` [longPwd, strongPwd]
       <++  errors

profileForm :: Form AppHandler SnapInput Html BlazeFormHtml Profile
profileForm = (\n d e t -> Profile Nothing n d e t)
  <$>  label "Name: "
       ++> inputText Nothing `validate` isNonEmpty
       <++ errors
  <*>  label "Age: "
       ++> inputText Nothing
       <++ errors
  <*>  label "Twitter: "
       ++> inputText Nothing `validate` isNonEmpty
       <++ errors
  <*>  label "Description: "
       ++> inputText Nothing 
       <++ errors

------------------------------------------------------------------------------
-- | Validators

isEmail :: Monad m => Validator m Html Text
isEmail = check "Invalid email address" (E.isValid . DT.unpack)

longPwd :: Monad m => Validator m Html Text
longPwd = check "Password is too short" $ \xs -> DT.length xs >= 6

strongPwd :: Monad m => Validator m Html Text
strongPwd = check "Password needs to contain at least one non-alphanumeric character" $ any (not . isAlphaNum) . DT.unpack

isNonEmpty :: Monad m => Validator m Html Text
isNonEmpty = check "Field must not be empty" $ not . DT.null

------------------------------------------------------------------------------
-- | HTML views
htmlTemplate :: Bool -> Html -> Html
htmlTemplate loggedIn bdy = H.docTypeHtml $ do
  H.head $ do
    H.title "TrackMe - never write CV again!"
    H.link ! A.rel "stylesheet" ! A.href "/css/style.css" ! A.type_ "text/css"
    H.script ! A.src "js/modernizr-1.7.min.js" $ ""
    H.script ! A.src "js/respond.min.js" $ ""
    H.script ! A.src "http://ajax.googleapis.com/ajax/libs/jquery/1.6/jquery.min.js" $ ""
    H.script ! A.src "js/script.js" $ ""
  H.body $ do
    H.div ! A.id "wrapper" $ do
      H.header ! A.id "header" ! A.class_ "clearfix" $ do
        H.hgroup $ do
          H.h1 ! A.id "site-title" $ do
            H.a "TrackMe" ! A.href "/" 
          H.h2 "Never write a CV again!" ! A.id "site-description"
      when loggedIn $ H.p $ toHtml ("You are logged in!" :: Text)
      H.div ! A.id "main" ! A.class_ "clearfix" $ do
        H.nav ! A.id "menu" ! A.class_ "clearfix" $ do
          H.ul $ do
            H.li ! A.class_ "current" $ do
              H.a "Home" ! A.href "/"
            H.li $ do
              H.a "Profiles" 
              H.ul $ do
                H.li $ do
                  H.a "Show all" ! A.href "/profiles/"
                H.li $ do
                  H.a "Experienced" ! A.href "/profiles/experienced/"
                H.li $ do
                  H.a "Latest" ! A.href "/profiles/latest/"
            H.li $ do
              H.a "Add profile" ! A.href "/profile/new/"
        H.div ! A.id "content" $ do
          bdy
        H.aside ! A.id "sidebar" $ do
          H.aside ! A.class_ "widget" $ do
            H.h3 "Quick help"
            H.p "If you get stuck, please contact us at support@trackme.com"
            H.p "When clicking on the Profiles menu, wait for the other query options to appear"
      H.footer ! A.id "footer" $ do
        H.p "Copyright 2011 Design by Venelin Valkov"
      H.div ! A.class_ "clear" $ ""

renderLoginForm :: Maybe Text -> FormHtml (HtmlM a) -> Html
renderLoginForm didFail form' = do
  let (formHtml', enctype) = renderFormHtml form'
  htmlTemplate False $ do
    when (isJust didFail) $ H.p $ toHtml ("Invalid credentials!" :: Text)
    H.form ! A.enctype (toValue $ show enctype)
      ! A.method "POST" ! A.action "/login" $ do
        _ <- formHtml'
        H.input ! A.type_ "submit" ! A.value "Submit"

renderIndex :: Bool -> Html
renderIndex ili =
  htmlTemplate ili $ do
    H.article ! A.class_ "post" $ do
      H.h2 "Welcome!" ! A.class_ "entry-title"
      H.div ! A.class_ "entry-content" $ do
        H.p "TrackMe allows you to follow your professional developer career. Notify your friends about events that you are going to attend. Show to the world how skillful developer you are. Present you to the best talent hunters in the software engineering field!"
      H.footer ! A.class_ "post-meta" $ do
        H.span ! A.class_ "author vcard" $ do
          H.p "TrackMe team"

renderNewProfileForm :: FormHtml (HtmlM a) -> Html
renderNewProfileForm form' = do
  let (formHtml', enctype) = renderFormHtml form'
  htmlTemplate False $ do
    H.article ! A.class_ "post" $ do
      H.h1 "New profile" ! A.class_ "entry-title"
      H.form ! A.enctype (toValue $ show enctype)
        ! A.method "POST" ! A.action "/profile/new" $ do
          _ <- formHtml'
          H.p $ do
            H.input ! A.type_ "submit" ! A.value "Submit"

renderProfiles :: [Profile] -> Html
renderProfiles profiles =
  htmlTemplate False $ do
    H.h1 "Profiles"
    forM_ profiles $ \(Profile _ name description _ twitter) ->
      H.article ! A.class_ "post" $ do
        H.h2 ! A.class_ "entry-title" $ do
          H.a $ toHtml name 
        H.div ! A.class_ "entry-content" $ do
          H.p $ toHtml description
        H.footer ! A.class_ "post-meta" $ do
          H.p $ "You can reach me at tweeter at @" `mappend` toHtml twitter 
          H.a "Delete" ! A.href "/profile/delete/1" ! A.class_ "more-link"

render403 :: Html
render403 = htmlTemplate False $ do
  H.h1  "Access Denied"
  H.p   "Perhaps you forgot to login?"

blaze :: MonadSnap m => Html -> m ()
blaze response = do
  modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
  writeLBS $ renderHtml response

------------------------------------------------------------------------------
-- | Profiles db operations

allProfiles :: HasHdbc p c s => p [Profile]
allProfiles = do
  rows <- query "SELECT * FROM profiles" []
  return $ map convertProfile rows

experiencedProfiles :: HasHdbc p c s => p [Profile]
experiencedProfiles = do
  rows <- query "SELECT * FROM profiles ORDER BY experience DESC LIMIT 2" []
  return $ map convertProfile rows

latestProfiles :: HasHdbc p c s => p [Profile]
latestProfiles = do
  rows <- query "SELECT * FROM profiles ORDER BY id DESC" []
  return $ map convertProfile rows

convertProfile :: Map String SqlValue -> Profile
convertProfile mp =
  let readSql k = fromSql $ mp DM.! k
  in   Profile  (Just $ read $ readSql "id")
                (readSql "name")
                (readSql "description")
                (readSql "experience")
                (readSql "twitter")

addProfile :: HasHdbc p c s => Profile -> p Integer
addProfile (Profile _ n d e t) = do
  query'  "INSERT INTO profiles (name, description, experience, twitter) VALUES (?, ?, ?, ?)" [toSql n, toSql d, toSql e, toSql t]


removeProfile :: HasHdbc m c s => Int -> m Integer
removeProfile pId = query' "DELETE FROM profiles WHERE id = ?" [toSql pId]

------------------------------------------------------------------------------
-- | Handlers
indexHandler :: AppHandler ()
indexHandler = do
  loggedIn <- with auth isLoggedIn
  blaze $ renderIndex loggedIn

loginFormHandler :: AppHandler ()
loginFormHandler = withSession appsess $ do
  loggedIn <- with auth isLoggedIn
  when loggedIn $ redirect "/"
  res <- eitherSnapForm loginForm "login-form-name"
  case res of
    Left form' ->
      with appsess $ do
        didFail <- getFromSession "login-failed"
        blaze $ renderLoginForm didFail form'
        deleteFromSession "login-failed"
        commitSession
    Right (User e p) -> do
      loginRes <- with auth $ loginByUsername (DT.encodeUtf8 e) (ClearText $ DT.encodeUtf8 p) False
      case loginRes of
        Left _   ->  do  with appsess $ do
                           setInSession "login-failed" "1"
                           commitSession
                         redirect "/login"
        Right _  ->  redirect "/"

logoutHandler :: AppHandler ()
logoutHandler = do
  with auth logout
  redirect "/"

listProfilesHandler :: AppHandler ()
listProfilesHandler = do
  profiles <- allProfiles
  blaze $ renderProfiles profiles

listLatestProfilesHandler :: AppHandler ()
listLatestProfilesHandler = do
  profiles <- latestProfiles
  blaze $ renderProfiles profiles

listExperiencedProfilesHandler :: AppHandler ()
listExperiencedProfilesHandler = do
  profiles <- experiencedProfiles
  blaze $ renderProfiles profiles
  
newProfileHandler :: AppHandler ()
newProfileHandler = do
  res <- eitherSnapForm profileForm "profile-form-name"
  case res of
    Left form' -> do
      blaze $ renderNewProfileForm form'
    Right prof -> do
      _ <- addProfile prof
      redirect "/"

deleteProfileHandler :: AppHandler ()
deleteProfileHandler = do
  maybe (return ())
        (\i -> do _ <- removeProfile (read $ unpack i)
                  return ()) =<< getParam "id"
  redirect "/"

------------------------------------------------------------------------------
-- | App initializer
trackme :: SnapletInit App App
trackme = makeSnaplet "trackme" "University relatet project" Nothing
  $ do
  addRoutes  [  ("/", ifTop indexHandler)
             ,  ("/login", loginFormHandler)
             ,  ("/logout", logoutHandler)
             ,  ("/profile/new", method $ newProfileHandler)
             ,  ("/profiles/", method GET $ listProfilesHandler)
             ,  ("/profiles/experienced/", method GET $ listExperiencedProfilesHandler)
             ,  ("/profiles/latest/", method GET $ listLatestProfilesHandler)
             ,  ("/profile/delete/:id", method GET $ deleteProfileHandler)
             ,  ("", serveDirectory "resources/static")
             ]
  sess   <- nestSnaplet "session" appsess $
              initCookieSessionManager "config/site_key.txt" "_session" Nothing
  let sqli = connectSqlite3 "resources/trackme.db"
  conn   <- nestSnaplet "hdbc" hdbc $ hdbcInit sqli
  auth'  <- nestSnaplet "auth" auth $
              initHdbcAuthManager defAuthSettings appsess sqli defAuthTable
              defQueries 
  return $ App auth' sess conn

