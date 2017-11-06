module Game.Agora.Ruleset
( slr
) where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text(Text)
import qualified Data.Text as T

import Game.Agora.Data.Common
import Game.Agora.Data.Index
import qualified Game.Agora.Data.Rule as AR
import qualified Game.Agora.Data.Proposal as AP

data Concat a = Concat Text a

execConcat :: Concat a -> Text
execConcat (Concat t _) = t

instance Functor Concat where
  fmap f (Concat t a) = Concat t $ f a

instance Applicative Concat where
  pure a = Concat "" a
  (Concat t f) <*> (Concat u a) = Concat (T.append t u) $ f a

instance Monad Concat where
  (Concat t a) >>= g = case g a of Concat u b -> Concat (T.append t u) b

c :: Text -> Concat ()
c t = Concat t ()

newLn :: Concat ()
newLn = c "\n"

ln :: Text -> Concat ()
ln t = c t >> newLn

blankLn :: Concat ()
blankLn = ln ""

sepLn :: Text -> Concat ()
sepLn c = ln $ T.replicate 72 c

revisions :: AR.Rule -> Int
revisions = sum . map (count . AR.rcChange) . AR.history
  where
    count :: AR.ChangeType -> Int
    count (AR.Amendment {AR.uncounted=u}) = if bool' u then 0 else 1
    count (AR.InfectionAmendment {AR.uncounted=u}) = if bool' u then 0 else 1
    count AR.Reenactment = 1
    count AR.RetitlingAmendment = 1
    count AR.PowerChangeAmendment = 1
    count _ = 0

slr :: [AR.Rule] -> [AP.Proposal] -> Index -> Text
slr rules' props' idx = execConcat $ do
    ln "THE SHORT LOGICAL RULESET"
    blankLn
    sequence_ $ M.mapWithKey section idx 
  where
    rules = M.fromList $ map (\r -> (AR.name r, r)) rules'
    props = M.fromList $ map (\p -> (AP.id p, p)) props'

    section :: Text -> [Text] -> Concat ()
    section t rs = do
      sepLn "="
      ln t
      sepLn "-"
      mapM_ rule rs
      blankLn

    rule :: Text -> Concat ()
    rule n = do
        c "Rule "
        c $ T.pack $ show $ AR.id r
        c "/"
        c $ T.pack $ show $ revisions r
        c " (Power="
        c $ T.pack $ show $ AR.power r
        c ")"
        newLn
        ln $ AR.name r
        blankLn
        mapM_ (\l -> c "      " >> ln l) $ T.lines $ AR.text r
        blankLn
        sepLn "-"
      where
        r = fromJust $ M.lookup n rules
