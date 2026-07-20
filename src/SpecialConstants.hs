{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module SpecialConstants where

import Dedukti.AbsDedukti (Exp, Bind, Tree(EIdent), Tree(QIdent))
import DeduktiOperations (splitApp, splitAbs)
import CommonConcepts (SCat, mainCats, lookupConstant, lookupConstantFull)
import Informath (Tree(..), GExp, GExps, GListExp)

import PGF (Expr, mkCId, mkApp)
import qualified Data.Set as S
import qualified Data.List as L

-- special constants that don't belong to lexical categories

---- TODO: apply to all conversions, not just Dedukti2MathCore

lambdaFlatten :: Exp -> (Exp, [Either Bind Exp])
lambdaFlatten exp = case splitApp exp of
  (f@(EIdent _), xs) -> (f, L.concatMap flatten xs)
  _ -> (exp, [])
 where
   flatten :: Exp -> [Either Bind Exp] 
   flatten exp = case splitAbs exp of
     (xs, body) -> L.map Left xs ++ [Right body]

data CallBacks =  CallBacks {
  callBind :: Bind -> Expr,
  callIdent :: Exp -> Expr,
  callExp  :: Exp -> Expr,
  callKind  :: Exp -> Expr,
  callProp  :: Exp -> Expr,
  callProof :: Exp -> Expr,
  callTerm :: Exp -> Expr
  }

specialDedukti2Informath :: CallBacks -> Exp -> Maybe (Expr, SCat)
specialDedukti2Informath callbacks exp = case lambdaFlatten exp of
  (EIdent (QIdent s), args) -> case lookupConstantFull s of
    Just (cat, fun, argcats, _) | S.member cat mainCats ->
      return (mkApp (mkCId fun) (L.map (convertArg callbacks) (L.zip argcats args)), cat)
    _ -> Nothing
  _ -> Nothing
 where
   convertArg ::  CallBacks -> (String, Either Bind Exp) -> Expr
   convertArg callbacks arg = case arg of
     (_, Left bind) -> callBind callbacks bind
     ("Exp", Right exp) -> callExp callbacks exp
     ("Kind", Right exp) -> callKind callbacks exp
     ("Prop", Right exp) -> callProp callbacks exp
     ("Proof", Right exp) -> callProof callbacks exp
     ("Ident", Right exp) -> callIdent callbacks exp
     ("Term", Right exp) -> callTerm callbacks exp
     (_, Right exp) -> callExp callbacks exp ---- ??

--- this should be on a more general level

gExps :: [GExp] -> GExps
gExps exps = case exps of
  [exp] -> GOneExps exp
  _ -> GManyExps (GListExp exps)
