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
         ] ++ concat singletons

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
                (AppT (ConT $ mkName gadtname) (AppT (ConT $ mkName "Maybe") (AppT (ConT $ mkName "Entity") (ConT n'))))
           ]

createSingletons :: Name -> Q [Dec]
createSingletons name = do
  n <- getName name
  return [
     ValD (VarP $ mkName $ "create"++n) (NormalB (AppE (AppE (VarE $ mkName ".") (VarE $ mkName "singleton")) (ConE $ mkName $ "Create"++n))) [],
     FunD (mkName $ "set"++n) [Clause [VarP $ mkName "x",VarP $ mkName "y"] (NormalB (AppE (AppE (VarE $ mkName "$") (UnboundVarE $ mkName "singleton")) (AppE (AppE (ConE $ mkName $ "Set"++n) (VarE $ mkName "x")) (VarE $ mkName "y")))) []],
     ValD (VarP $ mkName $ "delete"++n) (NormalB (AppE (AppE (VarE $ mkName ".") (VarE $ mkName "singleton")) (ConE $ mkName $ "Delete"++n))) [],
     ValD (VarP $ mkName $ "get"++n) (NormalB (AppE (AppE (VarE $ mkName ".") (VarE $ mkName "singleton")) (ConE $ mkName $ "Get"++n))) []
         ]

getName :: Name -> Q String
getName n = do
  TyConI (DataD _ qname _ _ _ _) <- reify n
  return $ nameBase qname
