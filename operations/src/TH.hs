{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module TH where

import Language.Haskell.TH

import Types

genGADT :: String -> [Name] -> Q [Dec]
genGADT gadtname ns = do
  let foo = mkName gadtname
  rows <- mapM (rowGADT gadtname) ns
  singletons <- mapM createSingletons ns
  return $ [
    DataD [] foo [PlainTV $ mkName "a"] Nothing (concat rows) []
         ]

rowGADT :: String -> Name -> Q [Con]
rowGADT gadtname name = do
  n <- getName name
  let n' = mkName n
  return [GadtC [mkName $ "Create"++n]
                [(Bang NoSourceUnpackedness NoSourceStrictness,ConT n')]
                (AppT (ConT $ mkName gadtname) (AppT (ConT $ mkName "Entity") (ConT n'))),
          GadtC [mkName $ "Set"++n]
                [(Bang NoSourceUnpackedness NoSourceStrictness,ConT $ mkName $ n++"Id"),
                (Bang NoSourceUnpackedness NoSourceStrictness,ConT n')]
                (AppT (ConT $ mkName gadtname) (AppT (ConT $ mkName "Entity") (ConT n'))),
          GadtC [mkName $ "Delete"++n]
                [(Bang NoSourceUnpackedness NoSourceStrictness,ConT $ mkName $ n++"Id")]
                (AppT (ConT $ mkName gadtname) (TupleT 0)),
          GadtC [mkName $ "Get"++n]
                [(Bang NoSourceUnpackedness NoSourceStrictness,ConT $ mkName $ n++"Id")]
                (AppT (ConT $ mkName gadtname) (AppT (ConT $ mkName "Maybe") (ConT n')))
           ]

createSingletons :: Name -> Q [Dec]
createSingletons name = do
  n <- getName name
  return [
     ValD (VarP $ mkName $ "create"++n) (NormalB (AppE (AppE (VarE $ mkName ".") (VarE $ mkName "singleton")) (VarE $ mkName $ "Create"++n))) [] {-,
    ValD (VarP $ mkName $ "set"++n) (NormalB (InfixE (Just (UnboundVarE $ mkName "singleton")) (VarE $ mkName ".") (Just (UnboundVarE $ mkName $ "Set"++n)))) [],
    ValD (VarP $ mkName $ "delete"++n) (NormalB (InfixE (Just (UnboundVarE $ mkName "singleton")) (VarE $ mkName ".") (Just (UnboundVarE $ mkName $ "Delete"++n)))) [],
    ValD (VarP $ mkName $ "get"++n) (NormalB (InfixE (Just (UnboundVarE $ mkName "singleton")) (VarE $ mkName ".") (Just (UnboundVarE $ mkName $ "Get"++n)))) [] -}
         ]

getName :: Name -> Q String
getName n = do
  TyConI (DataD _ qname _ _ _ _) <- reify n
  return $ nameBase qname
