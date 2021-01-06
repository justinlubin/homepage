{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((>>>), (<<<))

import Hakyll

removing :: String -> Routes
removing s =
  gsubRoute s (const "")

withoutHtml :: Routes
withoutHtml =
  composeRoutes
    ( setExtension ""
    )
    ( customRoute $ \identifier ->
        case toFilePath identifier of
          "index" ->
            "index.html"

          name ->
            name ++ "/index.html"
    )

main :: IO ()
main = hakyll $ do
  match "pages/*" $ do
    route (composeRoutes (removing "pages/") withoutHtml)
    compile $
      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "static/**" $ do
    route (removing "static/")
    compile copyFileCompiler

  match "templates/*" $
    compile templateBodyCompiler
