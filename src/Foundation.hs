{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foundation where

import Control.Lens ((^?), _Right)
import Control.Monad.Logger (LogSource)
import Data.Aeson.Lens
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Model.UserType
import Network.SendGridV3.Api
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Text.Julius (RawJS (rawJS))
import Text.Shakespeare.Text (stext)
import Yesod.Auth.Dummy
import Yesod.Auth.Email
import Yesod.Auth.OAuth2
import Yesod.Auth.OAuth2.Google.Custom
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings :: AppSettings
  , -- | Settings for static file serving.
    appStatic :: Static
  , -- | Database connection pool.
    appConnPool :: ConnectionPool
  , appHttpManager :: Manager
  , appLogger :: Logger
  }

data MenuItem = MenuItem
  { menuItemLabel :: Text
  , menuItemRoute :: Route App
  , menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *). (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just
      <$> defaultClientSessionBackend
        120 -- timeout in minutes
        "config/client_session_key.aes"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage

    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute

    -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
    -- (title, parents) <- breadcrumbs

    -- Define the menu items of the header.
    let menuItems =
          [ NavbarLeft $
              MenuItem
                { menuItemLabel = "Home"
                , menuItemRoute = HomeR
                , menuItemAccessCallback = True
                }
          , NavbarLeft $
              MenuItem
                { menuItemLabel = "Survey"
                , menuItemRoute = SurveyR
                , menuItemAccessCallback = isJust muser
                }
          , NavbarRight $
              MenuItem
                { menuItemLabel = "Login"
                , menuItemRoute = AuthR LoginR
                , menuItemAccessCallback = isNothing muser
                }
          , NavbarRight $
              MenuItem
                { menuItemLabel = "Sign Up"
                , menuItemRoute = AuthR registerR
                , menuItemAccessCallback = isNothing muser
                }
          , NavbarRight $
              MenuItem
                { menuItemLabel = "Logout"
                , menuItemRoute = AuthR LogoutR
                , menuItemAccessCallback = isJust muser
                }
          ]

    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

    let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    layoutId <- newIdent
    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_css
      (addScript . StaticR)
        if (appVueDevel (appSettings master)) then js_vue_js else js_vue_min_js
      addScript (StaticR js_bootstrapvue_min_js)
      -- TODO: uncomment if icons necessary
      -- addScriptRemote "//unpkg.com/bootstrap-vue@latest/dist/bootstrap-vue-icons.min.js"
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute ::
    App ->
    Maybe (Route App)
  authRoute _ = Just $ AuthR LoginR

  isAuthorized ::
    -- | The route the user is visiting.
    Route App ->
    -- | Whether or not this is a "write" request.
    Bool ->
    Handler AuthResult
  -- Routes not requiring authentication.
  isAuthorized route write = case route of
    AuthR _ -> unrestricted
    ComposersR -> unrestricted
    WorksR -> unrestricted
    HomeR -> unrestricted
    FaviconR -> unrestricted
    RobotsR -> unrestricted
    StaticR _ -> unrestricted
    UploadR -> isLoggedIn
    EntryR _ -> unrestricted
    AddWorkR -> unrestricted
    WorkR _ -> unrestricted
    SurveyR -> unrestricted
    SurveyDemographicsR -> isLoggedIn
    SurveyFingeringR _ -> isLoggedIn
    SurveyDoneR -> unrestricted
    IMSLPR _ -> unrestricted
    EntriesR _ -> unrestricted
    MusicXMLR _ -> unrestricted
    -- TODO: this is blocked right now in javascript on the client but
    -- we should make this more robust since allowing anyone without
    -- an account to spam the API is undesirable
    InferR -> unrestricted
    InferWeightsR -> unrestricted
    where
      unrestricted = return Authorized
      isLoggedIn =
        maybeAuthId <&> \case
          Nothing -> AuthenticationRequired
          Just _ -> Authorized

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ::
    -- | The file extension
    Text ->
    -- | The MIME content type
    Text ->
    -- | The contents of the file
    LByteString ->
    Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
      appShouldLogAll (appSettings app)
        || level == LevelWarn
        || level == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = pure . appLogger

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId

  -- Where to send a user after successful login
  loginDest :: App -> Route App
  loginDest _ = HomeR

  -- Where to send a user after logout
  logoutDest :: App -> Route App
  logoutDest _ = HomeR

  -- Override the above two destinations when a Referer: header is present
  redirectToReferer :: App -> Bool
  redirectToReferer _ = True

  authenticate ::
    (MonadHandler m, HandlerSite m ~ App) => Creds App -> m (AuthenticationResult App)
  authenticate creds = liftHandler $ do
    runDB $ do
      print creds
      x <- getBy $ UniqueUser $ credsIdent creds
      case x of
        Just (Entity uid _) -> return $ Authenticated uid
        Nothing -> do
          now <- liftIO getCurrentTime
          let extract traversal =
                getUserResponseJSON @Value creds ^? _Right . traversal . _String
          uid <- insert $ User (credsIdent creds) (if credsPlugin creds == "email-verify" then Email else OAuth) now False
          case credsPlugin creds of
            "email-verify" ->
              Authenticated uid
                <$ insert_
                  (EmailUser Nothing Nothing False uid)
            oauth ->
              Authenticated uid
                <$ insert_
                  OAuthUser
                    { oAuthUserUserId = uid
                    , oAuthUserEmail = extract (key "email")
                    , oAuthUserName = extract (key "name")
                    , oAuthUserPicture = extract $
                        key $ case oauth of
                          "google" -> "picture"
                          "github" -> "avatar_url"
                          _ -> error "Unknown oauth plugin"
                    }

  -- You can add other plugins like Google Email, email or OAuth here
  authPlugins :: App -> [AuthPlugin App]
  authPlugins app =
    [ oauth2GoogleScopedWidget
        googleWidget
        ["openid", "email", "profile"]
        (appGoogleOauthClientId (appSettings app))
        (appGoogleOauthClientSecret (appSettings app))
     , authEmail
    ]
      ++ extraAuthPlugins
    where
      -- Enable authDummy login if enabled.
      extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]
      googleWidget = [whamlet|
<span .btn .btn-primary>
  <img src=@{StaticR img_btn_google_light_normal_svg}>
  Sign in with Google
|]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $ case muid of
    Nothing -> Unauthorized "You must login to access this page"
    Just _ -> Authorized

instance YesodAuthPersist App

instance YesodAuthEmail App where
  type AuthEmailId App = UserId
  afterPasswordRoute _ = HomeR

  addUnverified email verkey =
    liftIO getCurrentTime >>= \now -> liftHandler $ runDB do
      uid <- insert $ User email Email now False
      insert_ $
        EmailUser
          { emailUserUserId = uid
          , emailUserPassword = Nothing
          , emailUserVerkey = Just verkey
          , emailUserVerified = False
          }
      pure uid

  checkPasswordSecurity _ x =
    pure $
      if length x >= 10 then Right () else Left "Password must be at least ten characters"

  sendVerifyEmail email _ verurl = do
    -- Print out to the console the verification email, for easier
    -- debugging.
    liftIO $ putStrLn $ "\nCopy/ Paste this URL in your browser:" ++ verurl ++ "\n"
    settings <- appSettings <$> getYesod

    -- Send email.
    let to = personalization $ (MailAddress email "" :| [])
        from = MailAddress "info@open-strings.com" "Open Strings"
        subject = "Verify your email address"
        content = Just $ mailContentText textPart :| [mailContentHtml htmlPart]
        message = mail @() @() [to] from subject content
    resp <- liftIO $ sendMail (appSendgridApiKey settings) message
    liftIO (print resp)
    where
      textPart =
        toStrict
          [stext|
Thank you for siging up for OpenStrings! Please confirm your email address by clicking on the link below.

#{verurl}

Thank you
|]
      htmlPart =
        toStrict $
          renderHtml
            [shamlet|
<p>Thank you for siging up for OpenStrings! Please confirm your email address <a href=#{verurl}>here</a>

<p>Thank you
                |]
  sendForgotPasswordEmail email _ verurl = do
    -- Print out to the console the verification email, for easier
    -- debugging.
    liftIO $ putStrLn $ "\nCopy/ Paste this URL in your browser:" ++ verurl ++ "\n"
    settings <- appSettings <$> getYesod

    -- Send email.
    let to = personalization $ (MailAddress email "" :| [])
        from = MailAddress "info@open-strings.com" "Open Strings"
        subject = "open-strings password reset"
        content = Just $ mailContentText textPart :| [mailContentHtml htmlPart]
        message = mail @() @() [to] from subject content
    resp <- liftIO $ sendMail (appSendgridApiKey settings) message
    liftIO (print resp)
    where
      textPart =
        toStrict
          [stext|
Click on the link below to reset your password.
#{verurl}
|]
      htmlPart =
        toStrict $
          renderHtml
            [shamlet|
<p>Click <a href=#{verurl}>here</a> to reset your password.
                |]
  getVerifyKey =
    liftHandler . runDB
      . fmap
        (join . fmap (emailUserVerkey . entityVal))
      . getBy
      . UniqueEmailUserId

  setVerifyKey uid k = liftHandler $ runDB do
    emailUser <- getBy404 (UniqueEmailUserId uid)
    update (entityKey emailUser) [EmailUserVerkey =. Just k]

  verifyAccount uid = liftHandler $
    runDB $ do
      mu <- getBy (UniqueEmailUserId uid)
      case mu of
        Nothing -> return Nothing
        Just u -> do
          update (entityKey u) [EmailUserVerified =. True, EmailUserVerkey =. Nothing]
          return $ Just uid

  getPassword =
    liftHandler . runDB
      . fmap
        (join . fmap (emailUserPassword . entityVal))
      . getBy
      . UniqueEmailUserId
  setPassword uid pass = liftHandler . runDB $ do
    emailUser <- getBy404 (UniqueEmailUserId uid)
    update (entityKey emailUser) [EmailUserPassword =. Just pass]

  getEmailCreds email = liftHandler $
    runDB $ do
      mu <- getBy $ UniqueUser email
      case mu of
        Nothing -> return Nothing
        Just (Entity uid _) -> do
          (Entity _ u) <- getBy404 (UniqueEmailUserId uid)
          return $
            Just
              EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ emailUserPassword u
                , emailCredsVerkey = emailUserVerkey u
                , emailCredsEmail = email
                }
  getEmail = liftHandler . runDB . fmap (fmap userIdent) . get

  emailLoginHandler toParent = do
    csrf <- getCSRF
    $(widgetFile "login-email")

  registerHandler = do
    toParent <- getRouteToParent
    csrf <- getCSRF
    authLayout do
      setTitle "Register a new account"
      $(widgetFile "register-account")

  forgotPasswordHandler = do
    toParent <- getRouteToParent
    csrf <- getCSRF
    authLayout do
      setTitle "Password Reset"
      $(widgetFile "password-reset")

  setPasswordHandler needOld = do
    toParent <- getRouteToParent
    csrf <- getCSRF
    selectRep do
      provideRep $ authLayout do
        setTitle "Set Password"
        $(widgetFile "set-password")

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
