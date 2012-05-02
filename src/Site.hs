{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
--   site. The 'app' function is the initializer that combines everything
--   together and is exported by this module.
--
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App (AuthManager App) ()
index =
  ifTop $ requireUser auth noLogin loggedIn where
    loggedIn =
      heistLocal (bindString "login" "janne") $ render "index"
    noLogin =
      heistLocal (bindString "login" "guest") $ render "login"


login :: Handler App (AuthManager App) ()
login = do
  -- TODO handle Maybes
  login <- fmap fromJust $ getParam "login"
  passwd <- fmap fromJust $ getParam "password"
  usr <- loginByUsername login (ClearText passwd) True
  redirect "/"

logoff :: Handler App (AuthManager App) ()
logoff = do
  logout
  redirect "/"

newUserForm :: Handler App (AuthManager App) ()
newUserForm = do
  heistLocal (bindSplices []) $ render "new_user"


newUserCreate :: Handler App (AuthManager App) ()
newUserCreate = do
  -- TODO handle Maybes
  acc <- fmap fromJust $ getParam "login"
  passwd <- fmap fromJust $ getParam "password"
  _usr <- createUser (T.decodeUtf8 acc) passwd
  redirect "/"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",              with auth $ index)
         , ("/login",         with auth $ login)
         , ("/logout",         with auth $ logoff)
         , ("/new_user", with auth $ newUserForm)
         , ("/new_user_submit",  with auth $ newUserCreate)
         , ("", with heist heistServe)
         , ("", serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    return $ App h s a
