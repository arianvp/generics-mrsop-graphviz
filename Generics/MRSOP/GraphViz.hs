{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Generics.MRSOP.GraphViz where
import Data.Proxy
import Text.Dot
import Generics.MRSOP.TH
import Generics.MRSOP.Opaque
import Generics.MRSOP.Base
import Generics.MRSOP.Util
import Control.Monad

import Generics.MRSOP.Examples.SimpTH


{-showFix
  :: forall ki fam codes ix. (Show1 ki, IsNat ix, HasDatatypeInfo ki fam codes ix)
  => Fix ki codes ix
  -> String
showFix (Fix rep) = elimRep show1 showFix mconcat rep-}
  
visualizeNA :: (Show1 ki, HasDatatypeInfo ki fam codes)
            => Proxy fam -> NA ki (Fix ki codes) a -> Dot NodeId
-- TODO: This recursive call is problematic, as we have no way of infering HasDatatypeInfo
-- as we threw away the `fam` in which the `NA` is present
visualizeNA _ (NA_I i) = visualizeFix i
visualizeNA _ (NA_K k) = node [("label", show1 k)]


{-
-- | This version does not use HasDatatypeInfo
visualizeFix' :: forall ki codes ix. (IsNat ix, Show1 ki) => Fix ki codes ix -> Dot NodeId
visualizeFix' (Fix rep) = 
  case sop rep of
    Tag c prod -> do
      constr <- node [("label", show (getNat (Proxy :: Proxy ix)) ++ ":" ++ show c)]
      fields <- elimNPM visualizeNA prod
      traverse (constr .->.) fields
      pure constr
-} 


visualizeFix
  :: forall ki fam codes ix. (Show1 ki, IsNat ix , HasDatatypeInfo ki fam codes) 
  => Fix ki codes ix
  -> Dot NodeId
visualizeFix (Fix rep) =
  case sop rep of
    Tag c prod ->
      let
        info = datatypeInfo (Proxy :: Proxy fam) (getSNat (Proxy :: Proxy ix))
        constrInfo = constrInfoLkup c info
      in do
        constr <- node [("label", constructorName constrInfo)]
        fields <- elimNPM (visualizeNA (Proxy :: Proxy fam)) prod 
        traverse (constr .->.) fields
        pure constr


func = deep @FamStmtString (SDecl $ test1 "fib" "n" "aux")

res = visualizeFix func
