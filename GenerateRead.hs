{-# LANGUAGE TemplateHaskell #-}

{-
Generating function to parse plain data constructors without arguments.
https://gist.github.com/zsol/6479547
-}

module GenerateRead where

import Language.Haskell.TH
import Data.Maybe
 
constructors :: Info -> Maybe [Name]
constructors (TyConI (DataD _cxt _name _bnds cons _names)) = Just $ catMaybes $ map getName cons
  where getName (NormalC name _types) = Just name
        getName _ = Nothing
constructors _ = Nothing
 
consToClause :: Name -> Q Clause
consToClause cons = do
  lit <- litP $ stringL $ nameBase cons
  body <- normalB $ appE [| Just |] (conE cons)
  return $ Clause [lit] body []
 
fallbackClause :: Q Clause
fallbackClause = do
  pat <- wildP
  body <- normalB $ [| Nothing |]
  return $ Clause [pat] body []
 
mkRead :: Name -> Q [Dec]
mkRead dataName = do
  tyInfo <- reify dataName
  let Just conss = constructors tyInfo
      clauses = map consToClause conss
  fun <- funD (mkName $ "read" ++ (nameBase dataName)) (clauses ++ [fallbackClause])
  return [fun]
