{-|
Module      : Horrocubes.Deserialization.
Description : Utility functions to deserialize bytestrings.
License     : Apache-2.0
Maintainer  : angel.castillo@horrocubes.io
Stability   : experimental

This script contains a set of utility functions to deserialise BuiltinByteStrings.
-}

-- MODULE DEFINITION ----------------------------------------------------------

module Horrocubes.Deserialisation
(
   builtinByteStringToInt
) where

-- IMPORTS --------------------------------------------------------------------

import PlutusTx.Builtins
import PlutusTx.Prelude  hiding (Semigroup (..), unless)

-- DEFINITIONS ----------------------------------------------------------------

{-# INLINABLE builtinByteStringToInt #-}
-- | @builtinByteString index acc bs@ Reads an integer of arbitrary size from a
-- @builtinByteStringToInt@.
builtinByteStringToInt :: Integer -> Integer -> BuiltinByteString -> Integer
builtinByteStringToInt index acc bs =
  if index < lengthOfByteString bs
  then  builtinByteStringToInt (index + 1) ((acc * 256) + (indexByteString bs index)) bs
  else acc