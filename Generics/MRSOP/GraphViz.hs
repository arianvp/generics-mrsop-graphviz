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

showDatatypeName :: DatatypeName -> String
showDatatypeName (Name str) = str
showDatatypeName (x :@: y) = showDatatypeName x ++ "(" ++  showDatatypeName y ++ ")"
 

visualizeNA :: (Show1 ki, HasDatatypeInfo ki fam codes)
            => Proxy fam -> NA ki (Fix ki codes) a -> Dot NodeId
visualizeNA Proxy x = 
  case x of 
    NA_I i -> visualizeFix i
    NA_K k -> node [("label", show1 k)]

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
        constr <- node [("label", constructorName constrInfo ++ " :: " ++ showDatatypeName (datatypeName info))]
        fields <- elimNPM (visualizeNA (Proxy :: Proxy fam)) prod 
        traverse (constr .->.) fields
        pure constr
