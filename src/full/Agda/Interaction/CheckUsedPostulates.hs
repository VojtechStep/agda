{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -dno-suppress-type-signatures -ddump-to-file -dno-typeable-binds #-}
module Agda.Interaction.CheckUsedPostulates
    ( reportUnexpectedPostulates) where

import Agda.Interaction.Library (builtinModulesWithSafePostulates)

import Agda.Syntax.Abstract (QName)
import Agda.Syntax.Internal
import Agda.Syntax.Position
import Agda.Syntax.Common
import Agda.Syntax.Common.Pretty as P

import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Debug
import Agda.TypeChecking.Monad.Signature
import Agda.TypeChecking.Monad.State
import Agda.TypeChecking.CompiledClause qualified as CC
import Agda.TypeChecking.Warnings (warning)
import Agda.TypeChecking.Pretty qualified as TC

import Agda.Utils.FileName
import Agda.Utils.Lens
import Agda.Utils.Maybe.Strict qualified as SM
import Agda.Utils.Monad
import Agda.Utils.Null
import Agda.Utils.Impossible
import Data.Foldable (toList)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet

import Agda.Utils.StrictState
import Agda.Utils.StrictReader
import Agda.Utils.StrictWriter
import Agda.Utils.ExpandCase (ExpandCase(expand))

reportUnexpectedPostulates :: QName -> [QName] -> TCM ()
reportUnexpectedPostulates q allowed = do
  reportSDoc "tc.assumptions" 70 $ "Collecting axioms for" TC.<+> TC.prettyTCM q
  (as, cache) <-
    flip runStateT (AxiomCollectorEnv empty empty) .
    flip runReaderT (HSet.fromList allowed) .
    execWriterT $ usedAxioms q
  reportSDoc "tc.assumptions" 10 $
    TC.vcat $ if (HMap.null as)
      then ["Found no unexpected axioms for" TC.<+> TC.prettyTCM q]
      else ("Found unexpected axioms for" TC.<+> TC.prettyTCM q TC.<+> ":") : (HMap.foldMapWithKey formatChain as)
  reportSDoc "tc.assumptions" 20 $
    "Cached" TC.<+> TC.prettyTCM (HSet.size (stVisited cache)) TC.<+> "definitions"
  where
  formatChain :: QName -> SM.Maybe (DefStackStep QName) -> [TCM TC.Doc]
  formatChain axName revPath = pure $ TC.vcat $
      "Dependency not allowed on" TC.<+> (TC.prettyTCM axName <> ", reached via") :
      reverse (TC.prettyTCM axName : (describeRevSteps revPath))
  describeRevSteps :: SM.Maybe (DefStackStep QName) -> [TCM TC.Doc]
  describeRevSteps = \case
    SM.Nothing -> []
    SM.Just step -> (TC.prettyTCM (unStep step) <> ", who's") TC.<+> (describeKind (stepTag step)) TC.<+> "depends on"
      : describeRevSteps (unTrace step)
  describeKind = \case
    ViaBody -> "body"
    ViaType -> "type"


data DefStackSepTag = ViaBody | ViaType
data DefStackStep a = DefStackStep { unStep :: !a, stepTag :: !DefStackSepTag, unTrace :: !(SM.Maybe (DefStackStep a))}

dropStep :: SM.Maybe (DefStackStep a) -> SM.Maybe (DefStackStep a)
dropStep = \case
  !SM.Nothing -> SM.Nothing
  SM.Just s -> unTrace s

type VisitedDefs = HashSet QName
-- ^ Set of visited definitions
type AxiomChain = HashMap QName (SM.Maybe (DefStackStep QName))
-- ^ Mapping from an axiom name to the path from it to the analyzed definition
data AxiomCollectorEnv = AxiomCollectorEnv
  { stVisited :: !VisitedDefs
  -- ^ Set of visited definitions
  , stDefStack :: !(SM.Maybe (DefStackStep QName))
  -- ^ Stack of definitions for reporting the trace to disallowed
  --   postulates.
  }

visited :: Lens' AxiomCollectorEnv VisitedDefs
visited f s = f (stVisited s) <&> \ x -> s { stVisited = x }
defStack :: Lens' AxiomCollectorEnv (SM.Maybe (DefStackStep QName))
defStack f s = f (stDefStack s) <&> \ x -> s { stDefStack = x }

type AllowedAxioms = HashSet QName
type AxiomCollector t =
  t -> WriterT AxiomChain        -- Collecting found axiom chains
       (ReaderT AllowedAxioms    -- Checking if an axiom is allowed
       (StateT AxiomCollectorEnv -- Tracking visited definitions and current stack
       TCM))                     -- Environment interaction
       ()

class UsedAxioms a where
  usedAxioms :: AxiomCollector a

  default usedAxioms :: (a ~ f b, Foldable f, UsedAxioms b) => AxiomCollector a
  usedAxioms = mapM' usedAxioms

instance UsedAxioms Definition where
  usedAxioms def = expand \ret -> ret do
    modify $ over defStack $ SM.Just . DefStackStep (defName def) ViaBody
    go (theDef def)
    modify $ over defStack $ fmap \s -> s { stepTag = ViaType }
    usedAxioms (defType def)
    modify $ over defStack $ dropStep
    where
    go :: AxiomCollector Defn
    go d = expand \ret -> case d of
      Axiom{} -> ret do
        let name = defName def
        allowed <- asks (name `HSet.member`)
        expand \ret -> if (not allowed) then ret $ pure ()
          else ret do
          let fp = rangeFile $ nameBindingSite name
          isSafe <- SM.caseMaybe fp (return False) \ file -> do
            fId <- idFromFile $ rangeFilePath file
            isBuiltinModuleWithSafePostulates fId
          expand \ret -> if (not isSafe ) then ret $ pure () else ret do
            stack <- use defStack
            tell1 (name, dropStep stack)
      DataOrRecSig _                                -> ret empty
      GeneralizableVar _                            -> ret empty
      -- Look through abstract definitions
      AbstractDefn d'                               -> ret $ go d'
      Function _ compiled _ _ _ _ _ _ _ _ _ _ _ _ -> ret $ expand \ret -> case compiled of
        Nothing                        -> ret do
          -- I'm not sure what Range this should get
          formatted <- TC.prettyTCM $ defName def
          warning $ UselessPragma noRange $
            "The function" P.<+>
            formatted P.<+>
            " has not been completely type-checked, cannot analyse all clauses"
        Just clauses                -> ret $ usedAxioms clauses
      Datatype _ _ _ _ s _ _ _ _ _ _                -> ret $ usedAxioms s
      Record _ _ _ _ fields tele _ _ _ _ _ _ _ _    -> ret $ usedAxioms (fields, tele)
      Constructor _ _ _ _ _ _ _ _ _ _ _             -> ret $ empty
      Primitive _ _ _ _ compiled _                  -> ret $ maybe empty usedAxioms compiled
      PrimitiveSort _ s                             -> ret $ usedAxioms s

instance UsedAxioms QName where
  usedAxioms q = do
    alreadyVisited <- gets (HSet.member q . (^. visited))
    unless alreadyVisited do
      modify $ over visited (HSet.insert q)
      reportSDoc "tc.assumptions" 70 $
        "Getting axioms for" TC.<+> TC.prettyTCM q
      def <- getConstInfo q
      usedAxioms def

instance UsedAxioms Term where
  usedAxioms t = expand \ret -> case t of
    Var _ elims   -> ret $ usedAxioms elims
    Lam _ abs     -> ret $ usedAxioms abs
    Lit _         -> ret $ empty
    Def q elims   -> ret $ usedAxioms (q, elims)
    Con _ _ elims -> ret $ usedAxioms elims
    Pi dom t      -> ret $ usedAxioms (dom, t)
    Sort s        -> ret $ usedAxioms s
    Level l       -> ret $ usedAxioms l
    MetaV x elims -> ret $ usedAxioms elims -- TODO: metas?
    DontCare t    -> ret $ usedAxioms t
    Dummy{}       -> ret $ empty -- internal use only

-- Automatic helper implementations ---------------

instance UsedAxioms a => UsedAxioms (Arg a)
instance UsedAxioms a => UsedAxioms (Abs a)
instance UsedAxioms a => UsedAxioms [a]
instance UsedAxioms a => UsedAxioms (Maybe a)
instance (UsedAxioms a, UsedAxioms b) => UsedAxioms (a, b) where
  usedAxioms (a, b) = (<>) <$> usedAxioms a <*> usedAxioms b
instance UsedAxioms CC.CompiledClauses
instance UsedAxioms Level

-- Manual helper implementations ------------------

instance UsedAxioms a => UsedAxioms (Elim' a) where
  -- Default instance checks endpoints of IApply; should we?
  usedAxioms = usedAxioms . isApplyElim

instance UsedAxioms Clause where
  usedAxioms = usedAxioms . clauseBody

instance UsedAxioms a => UsedAxioms (Dom a) where
  -- Default instance ignores the tactic; should we?
  usedAxioms (Dom _ _ _ tactic _ t) = usedAxioms (tactic, t)

instance UsedAxioms Type where
  -- Default instance ignores the sort
  usedAxioms (El sort el) = usedAxioms (sort, el)

instance UsedAxioms Telescope

instance UsedAxioms Sort where
  usedAxioms s = expand \ret -> case s of
    Univ _ l -> ret $ usedAxioms l
    Inf _ _ ->        ret $ empty
    SizeUniv ->       ret $ empty
    LockUniv ->       ret $ empty
    LevelUniv ->      ret $ empty
    IntervalUniv ->   ret $ empty
    PiSort dom s t -> ret $ usedAxioms (dom, (s, t))
    FunSort s t ->    ret $ usedAxioms (s, t)
    UnivSort s ->     ret $ usedAxioms s
    MetaS _ elims ->  ret $ usedAxioms elims -- metas?
    DefS _ elims ->   ret $ usedAxioms elims
    DummyS _ ->       ret $ empty -- internal only
