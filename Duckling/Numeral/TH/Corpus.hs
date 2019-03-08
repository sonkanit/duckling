-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.TH.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale TH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "ศูนย์"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "หนึ่ง"
             ]
  , examples (NumeralValue 2)
             [ "สอง"
             ]
  , examples (NumeralValue 3)
             [ "สาม"
             ]
  , examples (NumeralValue 4)
             [ "สี่"
             ]
  , examples (NumeralValue 5)
             [ "ห้า"
             ]
  , examples (NumeralValue 6)
             [ "หก"
             ]
  , examples (NumeralValue 7)
             [ "เจ็ด"
             ]
  , examples (NumeralValue 8)
             [ "แปด"
             ]
  , examples (NumeralValue 9)
             [ "เก้า"
             ]
  , examples (NumeralValue 10)
             [ "สิบ"
             ]
  , examples (NumeralValue 11)
             [ "สิบเอ็ด"
             ]
  , examples (NumeralValue 15)
             [ "สิบห้า"
             ]
  , examples (NumeralValue 17)
             [ "สิบเจ็ด"
             ]
  , examples (NumeralValue 20)
             [ "20"
             , "ยี่สิบ"
             ]
  , examples (NumeralValue 22)
             [ "ยี่สิบสอง"
             ]
  , examples (NumeralValue 24)
             [ "24"
             , "ยี่สิบสี่"
             ]
  , examples (NumeralValue 26)
             [ "ยี่สิบหก"
             ]
  , examples (NumeralValue 28)
             [ "ยี่สิบแปด"
             ]
  , examples (NumeralValue 50)
             [ "ห้าสิบ"
             ]
  , examples (NumeralValue 34)
             [ "สามสิบสี่"
             ]
  ]
