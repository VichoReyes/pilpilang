{-# LANGUAGE OverloadedStrings #-}

module Conversion where

import Syntax (Value (..), Predicate(..))
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Control.Monad.Reader (Reader)
import Control.Monad.RWS (asks)
import qualified Data.Text as T
import Types (ValidType (..), ValidVal (ValidVal, vvContents, vvType), tShow, TypedHeader (..), isPrimitive)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Control.Monad.State (execState, get, State, MonadState (put), evalState)

type Conversor = Reader (Map TypedHeader ([Text], Predicate ValidVal))

type Scope = [(ValidVal, (Char, Int))]

data Query = Query
    { qSelect :: [(Text, Text)] -- SELECT a.b, c.d 
    , qFrom :: [(Text, (Char, Int))]     -- FROM arts a1, cuisines c1
    , qWhere :: [Text]
    } deriving (Show, Eq, Ord)

qRender :: Query -> Text
qRender Query {qSelect = select', qFrom = from', qWhere = where'}
    = "SELECT "<>T.intercalate ", " (map (\(a, b) -> a<>"."<>b) select')
    <> " FROM "<>T.intercalate ", " (map (\(a, (c, i)) -> a<>" "<>T.pack (c : show i)) from')
    <> " WHERE "<>T.intercalate " AND " where'

renderPred :: Scope -> Predicate ValidVal -> Conversor Text
renderPred scope predicate = do
    case predicate of
        PAlways -> return "(1)"
        PAnd p1 p2 -> (\s1 s2 -> s1<>" AND "<>s2) <$> renderPred scope p1 <*> renderPred scope p2
        POr p1 p2 -> (\s1 s2 -> s1<>" OR "<>s2) <$> renderPred scope p1 <*> renderPred scope p2
        PEquals v1 v2 -> return $ renderCmp "=" v1 v2
        PGreaterT v1 v2 -> return $ renderCmp ">" v1 v2
        PLessT v1 v2 -> return $ renderCmp "<" v1 v2
        PredCall txt vvs -> do
            let predHeader = HDefinition txt $ map (NE.head . vvType) vvs
            predDef' <- asks (M.lookup predHeader)
            let (varNames, predDef) = fromJust predDef'
            subquery <- renderAssoc predHeader varNames predDef
            let start = "ROW("<>T.intercalate "," (map renderVal' vvs)<>") IN ("
            return $ start<>qRender subquery<>")"

        where
            renderCmp cmp v1 v2 = do
                let s1 = renderVal' v1
                    s2 = renderVal' v2
                 in s1 <> cmp <> s2
            renderVal' = renderVal scope

renderVal :: Scope -> ValidVal -> Text
renderVal scope val
    | isJust (lookup val scope) =
        let (c, i) = fromJust (lookup val scope)
         in T.pack $ c : show i
renderVal _ ValidVal {vvContents = VLitBool b} = tShow b
renderVal _ ValidVal {vvContents = VLitString b} = "'"<>b<>"'" -- TODO escape
renderVal _ ValidVal {vvContents = VLitInt b} = tShow b
renderVal scope val = error $ "renderVal: scope = "<>show scope<>" and val = "<>show val

renderAssoc :: TypedHeader -> [Text] -> Predicate ValidVal -> Conversor Query
renderAssoc header varNames predicate = do
    let scope = mkScope header
    (joinTables, conditions, scope') <- removeIndirections predicate scope
    predWhere <- renderPred scope' predicate
    return Query
        { qFrom = joinTables
        , qSelect = undefined
        , qWhere = predWhere : conditions
    }
        where
            mkScope (HDefinition _ types) = expandScope [] $ zip varNames types
            mkScope (HPermission _ act res) = expandScope [] $ zip varNames [TActor act, TResource res]

expandScope :: Scope -> [(Text, ValidType)] -> Scope
expandScope s1 pairs = execState (traverse addPair pairs) s1
    where
        addPair :: (Text, ValidType) -> State Scope ()
        addPair (name, typ) = do
            let (char, _) = fromJust $ T.uncons name
            wholeScope <- get
            let collisions = filter ((==char) . fst . snd) wholeScope
            let highestNum = 1 + foldr (max . snd . snd) 0 collisions
            put $ (ValidVal (VVar name) (NE.fromList [typ]), (char, highestNum)) : wholeScope


removeIndirections :: Predicate ValidVal -> Scope -> Conversor ([(Text, (Char, Int))], [Text], Scope)
removeIndirections predicate prevScope = return . unzip3 $ evalState (concat <$> traverse extractInfo values) prevScope
    where
        values :: [ValidVal]
        values = getValues predicate
        getValues PAlways = []
        getValues (PredCall _ args) = args
        getValues (PAnd p1 p2) = getValues p1 ++ getValues p2
        getValues (POr p1 p2) = getValues p1 ++ getValues p2
        getValues (PEquals v1 v2) = [v1, v2]
        getValues (PGreaterT v1 v2) = [v1, v2]
        getValues (PLessT v1 v2) = [v1, v2]

        extractInfo :: ValidVal -> State Scope [((Text, (Char, Int)), Text, (ValidVal, (Char, Int)))]
        extractInfo v = case vvContents v of
            VLitInt _ -> return []
            VLitBool _ -> return []
            VLitString _ -> return []
            VVar _ -> return []
            VVarField inner txt -> do
                let innerVal = ValidVal inner (NE.fromList $ NE.tail $ vvType v)
                innerResults <- extractInfo innerVal
                if isPrimitive (NE.head (vvType v))
                    then return innerResults
                    else do
                        curScope <- get
                        -- expand scope with new table
                        -- add the table to tables
                        -- create conditions
                        -- return all three
                        undefined 
