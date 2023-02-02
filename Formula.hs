module Formula where

import Prelude hiding (not)
import qualified Data.Set as Set

newtype Wrapper a = W a

newtype Label l x = L { getL :: (l, x) }

data Formula f a
    = Var a
    | NtV a -- (Not (Var x))
    | Const Bool
    | Not (f (Formula f a))
    | And (f (Formula f a)) (f (Formula f a))
    | Or  (f (Formula f a)) (f (Formula f a))


type FormulaT = Formula Wrapper Int 

data Atomic a

data CNF
    = Conj [CNF]
    | Disj [Formula Atomic Int] 
    -- 只能有Var, Const和NtV

    
not :: Formula Atomic Int -> Formula Atomic Int
not (Const b) = Const $ if b then False else True
not (Var n)   = NtV n


vars :: Ord a => Formula Wrapper a -> Set.Set a
vars (Var a) = Set.singleton a
vars (Const _) = Set.empty
vars (Not (W expr)) = vars expr
vars (And (W e1) (W e2)) = Set.union (vars e1) (vars e2)
vars (Or  (W e1) (W e2)) = Set.union (vars e1) (vars e2)

freshVarGen :: Set.Set Int -> Int
freshVarGen variables =
    case Set.lookupMax variables of {
        Nothing -> error "作为参数的变量集合为空";
        Just n  -> n + 1
    }


labelingNodes :: Int -> Formula Wrapper Int -> (Int, Formula (Label Int) Int)
-- 会给叶节点也打一个冗余的label
labelingNodes v expr = case expr of {
    (Var a)   ->   (v, Var a);
    (Const b) -> (v, Const b);
    -- 不是我不想用@，用@类型检查过不了
    Not (W x)   ->
       let (v1, x') = labelingNodes (succ v) x
       in  (v1, Not (L (v, x')));
    And (W x) (W y) ->
        let (v1, x') = labelingNodes (succ v) x
            (v2, y') = labelingNodes (succ v1) y
        in (v2, And (L (v, x')) (L (v1, y')));
    Or (W x) (W y) ->
        let (v1, x') = labelingNodes (succ v) x
            (v2, y') = labelingNodes (succ v1) y
        in (v2, Or (L (v, x')) (L (v1, y')));   
}

extractName :: Label Int (Formula (Label Int) Int) -> Formula Atomic Int
extractName (L (_, (Var a)))   = Var a
extractName (L (_, (Const b))) = Const b
extractName (L (n, _))         = Var n

{-
(<-->), (-->) :: Formula Wrapper Int -> Formula Wrapper Int -> Formula Wrapper Int
-- biconditional connective / conditional connective
x --> y = (not x) `or` y
x <--> y = (x --> y) `and` (y --> x)
-}

encodeTSEITIN :: Formula Wrapper Int -> CNF
encodeTSEITIN expr = 
    let newVar    = freshVarGen $ vars expr
        (p1,expr') = labelingNodes newVar expr
        in Conj $ [Disj [Var p1]] ++ go (L (p1,expr')) where
    go (L (_, (Var _))) = []
    go (L (_,(Const _))) = []
    go (L (p,(Not x))) = 
        let a = Var p
            b = extractName x
        in [Disj [b, a], Disj [not a,not b]] ++ go x
    go (L (p, (And x y))) = 
        let a = Var p
            b = extractName x
            c = extractName y
        in [Disj [not a,b], Disj [not a,c], Disj [not b,not c,a]] ++ go x ++ go y
    go (L (p, (Or x y))) = 
        let a = Var p
            b = extractName x
            c = extractName y
        in [Disj [not c,a], Disj [not b, a], Disj [not a, b, c] ] ++ go x ++ go y

