{-# LANGUAGE OverloadedStrings #-}

module Conversion where

import Syntax (Value (..), Predicate(..), GEntity (entityTable))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.RWS (asks)
import qualified Data.Text as T
import Types (ValidType (..), ValidVal (ValidVal, vvContents, vvType), tShow, TypedHeader (..), isPrimitive, TypedBody)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Control.Monad.State (get, MonadState (put), StateT (runStateT), evalStateT, lift)
import Control.Monad (forM)

type Conversor = Reader (Map TypedHeader ([Text], Predicate ValidVal))

type ConversorState = StateT Scope Conversor

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

convert :: M.Map TypedHeader TypedBody -> [Text]
convert m = map qRender queries
    where
        l = M.toList m
        queries = map renderAssoc' l
        renderAssoc' :: (TypedHeader, TypedBody) -> Query
        renderAssoc' (header, (names, predicate)) = runReader (renderAssoc header names predicate) m

renderPred :: Predicate ValidVal -> ConversorState Text
renderPred predicate = do
    case predicate of
        PAlways -> return "(1)"
        PAnd p1 p2 -> (\s1 s2 -> s1<>" AND "<>s2) <$> renderPred p1 <*> renderPred p2
        POr p1 p2 -> (\s1 s2 -> s1<>" OR "<>s2) <$> renderPred p1 <*> renderPred p2
        PEquals v1 v2 -> renderCmp "=" v1 v2
        PGreaterT v1 v2 -> renderCmp ">" v1 v2
        PLessT v1 v2 -> renderCmp "<" v1 v2
        PredCall txt vvs -> do
            let predHeader = HDefinition txt $ map (NE.head . vvType) vvs
            predDef' <- asks (M.lookup predHeader)
            let (varNames, predDef) = fromJust predDef'
            subquery <- lift $ renderAssoc predHeader varNames predDef
            renderedVals <- mapM renderVal vvs
            let start = "ROW("<>T.intercalate "," renderedVals<>") IN ("
            return $ start<>qRender subquery<>")"

        where
            renderCmp cmp v1 v2 = do
                s1 <- renderVal v1
                s2 <- renderVal v2
                return $ s1 <> cmp <> s2



-- render a value. There are different cases:
-- literal -> render as literal
-- variable -> opens up two more cases
--    type of variable is primitive -> OH GOD DESIGN PROBLEM
--    type of variable is not primitive -> look it up in scope, render
-- value.field -> opens up two more cases
--    type of field is primitive -> "${render value}.field"
--    type of field is not primitive -> look it up in scope, render
renderVal :: ValidVal -> ConversorState Text
renderVal ValidVal {vvContents = VLitBool b} = return $ tShow b
renderVal ValidVal {vvContents = VLitString b} = return $ "'"<>b<>"'" -- TODO escape
renderVal ValidVal {vvContents = VLitInt b} = return $ tShow b
renderVal val@ValidVal {vvContents = VVarField v f} = do
    scope <- get
    case lookup val scope of
        Just (c, i) -> return . T.pack $ c : show i
        Nothing -> do
            tableThing <- renderVal (ValidVal v (NE.fromList (NE.tail (vvType val))))
            return $ tableThing <> "." <> f
renderVal val = do
    scope <- get
    case lookup val scope of
        Just (c, i) -> return . T.pack $ c : show i
        Nothing -> error $ "renderVal: scope = "<>show scope<>" and val = "<>show val


renderAssoc :: TypedHeader -> [Text] -> Predicate ValidVal -> Conversor Query
renderAssoc header varNames predicate = flip evalStateT [] $ do
    select' <- mkScope header
    (joinTables, conditions) <- removeIndirections predicate
    predWhere <- renderPred predicate
    return Query
        { qFrom = joinTables ++ zip varNames select' -- TODO not varNames but table names
        , qSelect = map (\(c, i) -> (T.pack $ c : show i, "some_pk")) select'
        , qWhere = predWhere : conditions
    }
        where
            mkScope (HDefinition _ types) = expandScope $ zipWith makeVal varNames types
            mkScope (HPermission _ act res) = expandScope $ zipWith makeVal varNames [TActor act, TResource res]
            makeVal name typ = ValidVal (VVar name) (NE.fromList [typ])

-- add pairs to the scope, turning the value into a unique (char, int) combination
-- this (char, int) should be then turned into the relevant table nickname
expandScope :: [ValidVal] -> ConversorState [(Char, Int)]
expandScope values = forM values $ \val -> do
    let char = head (show $ NE.head $ vvType val)
    wholeScope <- get
    let collisions = filter ((==char) . fst . snd) wholeScope
    let highestNum = 1 + foldr (max . snd . snd) 0 collisions
    put $ (val, (char, highestNum)) : wholeScope
    return (char, highestNum)


removeIndirections :: Predicate ValidVal -> ConversorState ([(Text, (Char, Int))], [Text])
removeIndirections predicate = unzip . concat <$> traverse extractInfo values
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

        extractInfo :: ValidVal -> ConversorState [((Text, (Char, Int)), Text)]
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
                        tableNickname <- head <$> expandScope [v]
                        let tableFullName = case NE.head (vvType v) of
                                TActor ent -> entityTable ent
                                TResource ent -> entityTable ent
                                _ -> error "illegal state"
                        let condition = "("<>txt<>" = some other key)"
                        -- return both
                        return $ ((tableFullName, tableNickname), condition) : innerResults
