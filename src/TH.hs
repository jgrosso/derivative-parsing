{-# LANGUAGE TemplateHaskell #-}


module TH
  ( liftFree
  ) where


import Control.Monad.Free
import Data.Char (toLower)
import Language.Haskell.TH


-- Adapted from https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html
-- This is totally partial (no pun intended) and hacky since it's for my specific use case. Please don't use this more than you have to.
liftFree :: Name -> Q [Dec]
liftFree t =
    reify t >>= \(TyConI (DataD _ _ tyVarBndrs _ constructors _)) ->
      (++) <$>
        [d| mkFree :: Functor f => (() -> f ()) -> Free f ()
            mkFree f = liftF $ f () |] <*>
        mapM (liftFClause tyVarBndrs) constructors
  where
    liftFClause tyVarBndrs (NormalC name fields) =
      createDef $ if isTerminal then terminalBody else if noParams then noParamsBody else if hasParams then takesParamsBody else unsupportedError
      where
        (KindedTV lastType StarT) =
          last tyVarBndrs

        createDef body =
          funD (mkName $ constructorToFunctionCase $ nameBase name) [clause [] (normalB body) []]

        (_, VarT lastField) =
          last fields

        isTerminal =
          null fields || lastType /= lastField

        noParams =
          length fields == 1 && lastType == lastField

        hasParams =
          length fields > 1 && lastType == lastField

        terminalBody =
          appE (varE $ mkName "mkFree") (appE (varE $ mkName "const") (conE name))

        noParamsBody =
          appE (varE $ mkName "mkFree") (conE name)

        takesParamsBody =
          appE (appE (varE $ mkName ".") (varE $ mkName "mkFree")) (conE name)
    liftFClause _ _ =
      unsupportedError

    unsupportedError =
      error "Unsupported data constructor type!"

    constructorToFunctionCase "" = ""
    constructorToFunctionCase (first:rest) =
      toLower first : rest
