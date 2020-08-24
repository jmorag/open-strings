{ mkDerivation, aeson, base, bytestring, case-insensitive
, classy-prelude, classy-prelude-conduit, classy-prelude-yesod
, conduit, containers, data-default, dhall, directory, esqueleto
, fast-logger, file-embed, flow, foreign-store, generic-lens
, hjsmin, hpack, hspec, html-conduit, http-client-tls, http-conduit
, http-types, language-javascript, lens, lens-aeson
, lens-regex-pcre, monad-control, monad-logger, persistent
, persistent-postgresql, persistent-template, raw-strings-qq, safe
, shakespeare, stdenv, string-conversions, template-haskell, text
, time, unordered-containers, validation-selective, vector, wai
, wai-extra, wai-logger, warp, xml-conduit, xml-lens, yaml, yesod
, yesod-auth, yesod-auth-oauth2, yesod-core, yesod-form
, yesod-static, yesod-test
}:
mkDerivation {
  pname = "fingerdb";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default dhall directory esqueleto fast-logger file-embed flow
    foreign-store generic-lens hjsmin html-conduit http-client-tls
    http-conduit http-types language-javascript lens lens-aeson
    lens-regex-pcre monad-control monad-logger persistent
    persistent-postgresql persistent-template raw-strings-qq safe
    shakespeare string-conversions template-haskell text time
    unordered-containers validation-selective vector wai wai-extra
    wai-logger warp xml-conduit xml-lens yaml yesod yesod-auth
    yesod-auth-oauth2 yesod-core yesod-form yesod-static
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default dhall directory esqueleto fast-logger file-embed flow
    foreign-store generic-lens hjsmin html-conduit http-client-tls
    http-conduit http-types language-javascript lens lens-aeson
    lens-regex-pcre monad-control monad-logger persistent
    persistent-postgresql persistent-template raw-strings-qq safe
    shakespeare string-conversions template-haskell text time
    unordered-containers validation-selective vector wai wai-extra
    wai-logger warp xml-conduit xml-lens yaml yesod yesod-auth
    yesod-auth-oauth2 yesod-core yesod-form yesod-static
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default dhall directory esqueleto fast-logger file-embed flow
    foreign-store generic-lens hjsmin hspec html-conduit
    http-client-tls http-conduit http-types language-javascript lens
    lens-aeson lens-regex-pcre monad-control monad-logger persistent
    persistent-postgresql persistent-template raw-strings-qq safe
    shakespeare string-conversions template-haskell text time
    unordered-containers validation-selective vector wai wai-extra
    wai-logger warp xml-conduit xml-lens yaml yesod yesod-auth
    yesod-auth-oauth2 yesod-core yesod-form yesod-static yesod-test
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
