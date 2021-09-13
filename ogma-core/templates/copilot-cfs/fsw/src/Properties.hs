{-# LANGUAGE DataKinds #-}

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))

position :: Stream Position
position = extern "my_position" Nothing

-- FIXME: 16 is a magic number
data Position = Position
  { tlmHeader   :: Field "TlmHeader" (Array 16 Word8)
  , aircraft_id :: Field "aircraft_id" Word32 
  , time_gps    :: Field "time_gps" Double
  , time_boot   :: Field "time_boot" Double
  , latitute    :: Field "latitude"  Double
  , longitude   :: Field "longitude" Double
  , altitude_abs :: Field "altitude_abs" Double
  , altitude_rel :: Field "altitude_rel" Double
  , vn :: Field "vn" Double
  , ve :: Field "ve" Double
  , vd :: Field "vd" Double
  , hdg :: Field "hdg" Double
  , hdop :: Field "hdop" Word16
  , vdop :: Field "vdop" Word16
  , numSats :: Field "numSats" Int64
  }

instance Struct Position where
  typename _ = "position_t"  -- Name of the type in C

  -- Function to translate Vec to list of Value's, order should match struct.
  toValues v = [ Value (Array Word8) (tlmHeader v)
               , Value Word32 (aircraft_id v)
               , Value Double (time_gps    v)
               , Value Double (time_boot   v)
               , Value Double (latitute    v)
               , Value Double (longitude   v)
               , Value Double (altitude_abs v)
               , Value Double (altitude_rel v)
               , Value Double (vn v)
               , Value Double (ve v)
               , Value Double (vd v)
               , Value Double (hdg v)
               , Value Word16 (hdop v)
               , Value Word16 (vdop v)
               , Value Int64 (numSats v)
               ]

-- We need to provide an instance to Typed with a bogus Vec
instance Typed Position where
  typeOf = Struct (Position (Field $ array []) (Field 0) (Field 0) (Field 0) (Field 0) (Field 0) (Field 0) (Field 0) (Field 0) (Field 0) (Field 0) (Field 0) (Field 0) (Field 0) (Field 0))

spec = do
  -- Trigger that always executes, splits the vec into seperate args.
  -- trigger "split" true [arg $ vecs # x, arg $ vecs # y]
  trigger "split" (position # vn > 10) []

main = reify spec >>= compile "position"
