{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Game.Agora.Web (hlr) where

import Lucid
import CMark
import Text.Heredoc
import qualified Game.Agora.Data.Rule as AR
import qualified Game.Agora.Data.Proposal as AP
import qualified Game.Agora.Data.Index as AI
import Data.Char (isLetter)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Maybe
import qualified Data.Text.Lazy as TL

hlr :: AR.RuleMap -> AP.PropMap -> AI.Index -> Text -> TL.Text
hlr rules _props idx header = renderText $ doctypehtml_ $ do
    head_ $ do
        script_ [ src_ "https://code.jquery.com/jquery-3.2.1.min.js", integrity_ "sha256-hwg4gsxgFZhOsEEamdOYGBf13FyQuiTwlAQgxVSNgt4=", crossorigin_ "anonymous" ] (mempty :: Html ())
        --  Latest compiled and minified CSS
        link_ [ rel_ "stylesheet", href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css", integrity_ "sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u", crossorigin_ "anonymous" ]
        --  Latest compiled and minified JavaScript
        script_ [ src_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js", integrity_ "sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa", crossorigin_ "anonymous" ] (mempty :: Html ())
        style_ [here|
          .fixed {
            position: fixed;
          }

          /* sidebar */
          .bs-docs-sidebar {
            padding-left: 20px;
            margin-top: 20px;
            margin-bottom: 20px;
          }

          /* all links */
          .bs-docs-sidebar .nav>li>a {
            color: #999;
            border-left: 2px solid transparent;
            padding: 4px 20px;
            font-size: 12px;
            font-weight: 400;
          }

          /* nested links */
          .bs-docs-sidebar .nav .nav>li>a {
            padding-top: 1px;
            padding-bottom: 1px;
            padding-left: 30px;
            font-size: 11px;
          }

          /* active & hover links */
          .bs-docs-sidebar .nav>.active>a,
          .bs-docs-sidebar .nav>li>a:hover,
          .bs-docs-sidebar .nav>li>a:focus {
            color: #563d7c;
            text-decoration: none;
            background-color: transparent;
            border-left-color: #563d7c;
          }
          /* all active links */
          .bs-docs-sidebar .nav>.active>a,
          .bs-docs-sidebar .nav>.active:hover>a,
          .bs-docs-sidebar .nav>.active:focus>a {
            font-weight: 700;
          }
          /* nested active links */
          .bs-docs-sidebar .nav .nav>.active>a,
          .bs-docs-sidebar .nav .nav>.active:hover>a,
          .bs-docs-sidebar .nav .nav>.active:focus>a {
            font-weight: 500;
          }

          /* hide inactive nested list */
          .bs-docs-sidebar .nav ul.nav {
            display: none;
          }
          /* show active nested list */
          .bs-docs-sidebar .nav>.active>ul.nav {
            display: block;
          }

          /*Header formatting */
          .MainContent {
            margin-top: 50px;
          }
          .group, .rule {
            padding-top: 50px;
            margin-top: -50px;
          }
          #rawlink {
            margin-top: 20px;
          }
        |]
        script_ [here|
          $(function() {
            $('body').scrollspy({
              target: '.bs-docs-sidebar',
              offset: 40
            });
          })
        |]
    body_ $ do
        --  Top Navbar
        toHtmlRaw $ T.replace "__ACTIVE_RULESET__" "active" header
        div_ [ class_ "container" ] $
            --  Main Content
            div_ [ class_ "row MainContent" ] $ do
                -- Side Nav Bar
                nav_ [ class_ "col-xs-3 bs-docs-sidebar" ] $ ul_ [ id_ "sidebar", class_ "nav nav-stacked fixed" ] $
                    mapM_ tocEntry idx
                -- Main Content
                div_ [ class_ "col-xs-9" ] $ do
                    div_ [ id_ "rawlink" ] $ do
                        "View as raw text: "
                        a_ [ href_ "flr.txt" ] "FLR"
                        ", "
                        a_ [ href_ "slr.txt" ] "SLR"
                    mapM_ category idx
  where
    tocEntry :: AI.Category -> Html ()
    tocEntry cat = li_ $ do
      a_ [ href_ ( T.concat ["#", filterId $ AI.name cat] ) ] (toHtml $ AI.name cat)
      ul_ [ class_ "nav nav-stacked" ] $
          mapM_ (\name -> li_ $ a_ [ href_ (T.concat ["#Rule", pack $ show $ AR.id $ ruleNamed name]) ] $ toHtml name) $ AI.rules cat
    category :: AI.Category -> Html ()
    category cat = section_ [id_ (filterId $ AI.name cat), class_ "group"] $ do
        h3_ $ toHtml $ AI.name cat
        -- TODO: Load section annotations (or get rid of them)
        -- div_ [ class_ "alert alert-info", href_ "alert" ] $ "<%= markdown.render(annotations[group.name]) %>"
        mapM_ rule $ AI.rules cat
    rule :: Text -> Html ()
    rule name = div_ [ id_ $ T.concat ["Rule", pack $ show $ AR.id $ ruleNamed name], class_ "rule" ] $ do
        h4_ $ do
            toHtml name
            " "
            span_ [ class_ "label label-default" ] $ toHtml $ T.concat [pack $ show $ AR.id $ ruleNamed name, "/", pack $ show $ AR.revisionCount $ ruleNamed name]
            " "
            span_ [ class_ "label label-default" ] $ toHtml $ show $ AR.power $ ruleNamed name
        toHtmlRaw $ commonmarkToHtml [] $ linkify $ AR.text $ ruleNamed name
    filterId = T.filter isLetter
    ruleNamed n = fromMaybe (error $ "Rule " ++ show n ++ " does not exist") $ M.lookup n rules
    keywords :: [(Text, AR.Rule)]
    keywords = concat $ M.elems $ M.map (\r -> map (flip (,) r) (AR.keywords r)) rules
    linkify :: Text -> Text
    linkify text = foldl linkifyKeyword text keywords
    linkifyKeyword :: Text -> (Text, AR.Rule) -> Text
    linkifyKeyword text (keyword, kwRule) = T.replace
      (T.concat [" ", keyword, " "])
      (T.concat [" [", keyword, "]", "(#Rule", pack $ show $ AR.id kwRule, ") "])
      text
