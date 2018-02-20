module Game.Agora.Ruleset
( flr
, slr
) where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text(Text)
import qualified Data.Text as T
import Data.Time
import Text.Wrap

import Game.Agora.Data.Common
import qualified Game.Agora.Data.Index as AI
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

wrap :: Bool -> Concat a -> Concat a
wrap indent (Concat t a) =
    Concat t' a
  where
    t' = if indent then (T.drop 2 $ wrapped $ T.append "  " t) else wrapped t
    wrapped t = wrapText (defaultWrapSettings{preserveIndentation=True}) 72 t

data Ruleset = SLR | FLR
  deriving (Eq, Show)

slr :: AR.RuleMap -> AP.PropMap -> AI.Index -> Text
slr rules props idx = execConcat $ do
    ln "THE SHORT LOGICAL RULESET"
    ruleset SLR rules props idx
    ln $ "END OF THE SHORT LOGICAL RULESET"

flr :: AR.RuleMap -> AP.PropMap -> AI.Index -> Text
flr rules props idx = execConcat $ do
    ln "THE FULL LOGICAL RULESET"
    ruleset FLR rules props idx
    ln $ "END OF THE FULL LOGICAL RULESET"

ruleset :: Ruleset -> AR.RuleMap -> AP.PropMap -> AI.Index -> Concat ()
ruleset rs rules props idx = do
    header
    blankLn
    sequence_ $ map section idx 
    footer
  where
    header :: Concat ()
    header = return ()

    footer :: Concat()
    footer = do
      sepLn "="
      blankLn

    section :: AI.Category -> Concat ()
    section c = do
      sepLn "="
      ln $ AI.name c
      sepLn "-"
      mapM_ rule $ AI.rules c

    rule :: Text -> Concat ()
    rule n = do
        c "Rule "
        c $ T.pack $ show $ AR.id r
        c "/"
        c $ T.pack $ show $ AR.revisionCount r
        c " (Power="
        c $ T.pack $ show $ AR.power r
        c ")"
        newLn
        ln $ AR.name r
        blankLn
        mapM_ (\l -> c "      " >> ln l) $ T.lines $ AR.text r
        blankLn
        when (rs == FLR) $ do
          ln "History:"
          foldM_ histLine' 0 $ AR.history r
          blankLn
          ln "Annotations:"
          mapM_ ann $ AR.annotations r
          blankLn
        sepLn "-"
      where
        r = fromMaybe (error $ "Rule " ++ show n ++ " does not exist") $ M.lookup n rules

        histLine' :: Int -> AR.RuleChange -> Concat Int
        histLine' cc rc = do
            wrap True $ histLine cc' rc
            newLn
            return cc'
          where
            cc' = cc + AR.countChange (AR.rcChange rc)

    histLine :: Int -> AR.RuleChange -> Concat ()
    histLine _ rc@AR.RuleChange{AR.rcChange = t@(AR.Initial{})} = do
      c "Initial "
      c $ T.pack $ show $ AR.changeMutability t
      c " Rule "
      c $ T.pack $ show $ AR.changeId t
      c " at Agora's birth, "
      c $ date $ AR.rcDate rc
      extra rc
    histLine _ rc@AR.RuleChange{AR.rcChange = t@(AR.Enactment{})} = do
      c "Enacted"
      c $ maybe "" (\i -> T.concat ["with MI=", i, " "]) $ AR.mi t
      agentDate rc
    histLine cc rc@AR.RuleChange{AR.rcChange = t@(AR.Amendment{})} = do
      c "Amended"
      when (AR.countChange t > 0) $ do
        c "("
        c $ T.pack $ show $ cc
        c ")"
      agentDate rc
    histLine cc rc@AR.RuleChange{AR.rcChange = t@(AR.Retitling{})} = do
      c "Retitled"
      agentDate rc
    histLine cc rc@AR.RuleChange{AR.rcChange = t@(AR.Renumbering{})} = do
      c "Renumbered"
      agentDate rc
    histLine cc rc@AR.RuleChange{AR.rcChange = t@(AR.Mutation{})} = do
      c "Mutated from MI="
      c $ AR.oldMi t
      c " to MI="
      c $ AR.newMi t
      agentDate rc
    histLine cc rc@AR.RuleChange{AR.rcChange = t@(AR.PowerChange{})} = do
      c "Power changed from power "
      c $ T.pack $ show $ AR.oldPower t
      c " to "
      c $ T.pack $ show $ AR.newPower t
      agentDate rc
    histLine cc rc@AR.RuleChange{AR.rcChange = t@(AR.Repeal{})} = do
      c "Repealed"
      agentDate rc
    histLine cc rc@AR.RuleChange{AR.rcChange = t@(AR.Reenactment{})} = do
      c "Reenacted"
      when (AR.countChange t > 0) $ do
        c "("
        c $ T.pack $ show $ cc
        c ")"
      agentDate rc
    histLine cc rc@AR.RuleChange{AR.rcChange = t@(AR.Infection{})} = do
      c "Infected"
      agentDate rc
    histLine cc rc@AR.RuleChange{AR.rcChange = t@(AR.CommitteeAssignment{})} = do
      c "Assigned to the "
      c $ AR.committee t
      agentDate rc
    histLine cc rc@AR.RuleChange{AR.rcChange = t@(AR.InfectionAmendment{})} = do
      c "Infected and "
      histLine cc rc{AR.rcChange = AR.Amendment{AR.uncounted=AR.uncounted t}}
    histLine cc rc@AR.RuleChange{AR.rcChange = t@(AR.Unknown{})} = do
      ln "... history unknown ..."

    agent :: AR.Agent -> Concat ()
    agent (AR.ProposalAgent i) = do
        c "Proposal "
        c $ AP.id p
        maybe (return ()) (\t -> do
          c " \""
          c t
          c "\"")
          $ AP.title p
        when (bool' $ AP.disinterested p) $ c " [disinterested]"
        maybe (return ()) (\a -> do
          c " ("
          c a
          maybe (return ()) (\o -> do
            c ", running for "
            c o
            ) $ AP.campaign p
          unless (null coauthors) $ do
            c "; with "
            c $ head coauthors
            mapM_ (\ca -> c ", " >> c ca) $ tail coauthors
          c ")"
          ) $ AP.author p
      where
        p = fromMaybe (error $ "Proposal " ++ show (T.unpack i) ++ " does not exist") $ M.lookup i props
        coauthors = fromMaybe [] $ AP.coauthors p
    agent (AR.RuleAgent i) = do
      c "Rule "
      c $ T.pack $ show $ i
    agent (AR.CleaningAgent {AR.by=by}) = do
      c "cleaning ("
      c by
      c ")"
    agent (AR.RatificationAgent {AR.document=doc, AR.agent=a}) = do
      c doc
      c " ratification (by "
      agent a
      c ")"
    agent (AR.WithoutObjectionAgent p) = do
      c p
      c ", without objection"
    agent (AR.DecreeAgent player) = do
      c player
      c "'s Decree"

    agentDate :: AR.RuleChange -> Concat ()
    agentDate rc = do
      maybe (return ()) (\a -> do
        c " by "
        agent a
        ) $ AR.rcAgent rc
      c ", "
      c $ date $ AR.rcDate rc
      extra rc

    extra :: AR.RuleChange -> Concat ()
    extra rc = do
      when (any isJust $ map ($ rc) $ [AR.rcSubstantial, AR.rcCosmetic, AR.rcUnattributed]) $ do
        c ","
      c $ maybe "" (\_ -> " substantial") $ AR.rcSubstantial rc
      c $ maybe "" (\_ -> " cosmetic") $ AR.rcCosmetic rc
      c $ maybe "" (\_ -> " (unattributed)") $ AR.rcUnattributed rc

    fmtDay :: Day -> Text
    fmtDay day = T.pack $ formatTime defaultTimeLocale "%b %d, %Y" $ day

    date :: AR.ChangeDate -> Text
    date (AR.Date day) = fmtDay day
    date (AR.UnknownHistory{}) = "in days lost to history"
    date (AR.Around {AR.around=day}) = T.append "around " $ fmtDay day
    date (AR.Range {AR.between=start, AR.and=end}) = T.concat ["sometime between ", fmtDay start, " and ", fmtDay end] 

    ann :: AR.Annotation -> Concat ()
    ann a = do
        unless (null $ AR.annCfjs a) $ do
          cfj $ head $ AR.annCfjs a
          mapM_ (\cj -> c ", " >> cfj cj) $ tail $ AR.annCfjs a
        ln ":"
        wrap False $ c $ T.append "  " $ AR.annText a
        newLn
      where
        cfj :: AR.CFJ -> Concat()
        cfj cj = do
          c "CFJ "
          c $ AR.cfjId cj
          maybe (return ()) (\d -> do
            c " (called "
            c $ date d
            c ")"
            ) $ AR.cfjCalled cj

