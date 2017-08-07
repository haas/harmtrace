{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}

module HarmTrace.Base.Instances where

import Control.DeepSeq

import HarmTrace.Base.MusicRep

--------------------------------------------------------------------------------
-- NFData instances for HarmTrace-Base
--------------------------------------------------------------------------------

instance NFData Mode where
  rnf MinMode = ()
  rnf MajMode = ()
