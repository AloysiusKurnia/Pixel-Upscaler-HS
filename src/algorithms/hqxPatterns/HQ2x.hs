module Src.Algorithms.HQXPatterns.HQ2x where

import Src.Algorithms.HQX
import Src.Algorithms.Common

getPixelValueHQ2x :: Square3x3 RGBPixel -> Square2x2 RGBPixel
getPixelValueHQ2x neighborhood
  | pattern `elem` [0, 1, 4, 32, 128, 5, 132, 160, 33, 129, 36, 133, 164, 161, 37, 165] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [2, 34, 130, 162] = (
      blendHQ2x neighborhood P2_00 B2_22,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [16, 17, 48, 49] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern `elem` [64, 65, 68, 69] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_21,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern `elem` [8, 12, 136, 140] = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [3, 35, 131, 163] = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [6, 38, 134, 166] = (
      blendHQ2x neighborhood P2_00 B2_22,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [20, 21, 52, 53] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern `elem` [144, 145, 176, 177] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern `elem` [192, 193, 196, 197] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_21,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern `elem` [96, 97, 100, 101] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern `elem` [40, 44, 168, 172] = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [9, 13, 137, 141] = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [18, 50] = (
      blendHQ2x neighborhood P2_00 B2_22,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern `elem` [80, 81] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_21,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [72, 76] = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern `elem` [10, 138] = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 66 = (
      blendHQ2x neighborhood P2_00 B2_22,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_21,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 24 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern `elem` [7, 39, 135] = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [148, 149, 180] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern `elem` [224, 228, 225] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern `elem` [41, 169, 45] = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [22, 54] = (
      blendHQ2x neighborhood P2_00 B2_22,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern `elem` [208, 209] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_21,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [104, 108] = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern `elem` [11, 139] = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [19, 51] = (
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_00 B2_11
        else blendHQ2x neighborhood P2_00 B2_60,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_90,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern `elem` [146, 178] = (
      blendHQ2x neighborhood P2_00 B2_22,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_90,
      blendHQ2x neighborhood P2_10 B2_20,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_11 B2_12
        else blendHQ2x neighborhood P2_11 B2_61)
  | pattern `elem` [84, 85] = (
      blendHQ2x neighborhood P2_00 B2_20,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_01 B2_11
        else blendHQ2x neighborhood P2_01 B2_60,
      blendHQ2x neighborhood P2_10 B2_21,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_90)
  | pattern `elem` [112, 113] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_22,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_10 B2_12
        else blendHQ2x neighborhood P2_10 B2_61,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_90)
  | pattern `elem` [200, 204] = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_90,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_11 B2_11
        else blendHQ2x neighborhood P2_11 B2_60)
  | pattern `elem` [73, 77] = (
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_00 B2_12
        else blendHQ2x neighborhood P2_00 B2_61,
      blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_90,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern `elem` [42, 170] = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_90,
      blendHQ2x neighborhood P2_01 B2_21,
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_10 B2_11
        else blendHQ2x neighborhood P2_10 B2_60,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [14, 142] = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_90,
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_01 B2_12
        else blendHQ2x neighborhood P2_01 B2_61,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 67 = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_21,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 70 = (
      blendHQ2x neighborhood P2_00 B2_22,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_21,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 28 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 152 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 194 = (
      blendHQ2x neighborhood P2_00 B2_22,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_21,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 98 = (
      blendHQ2x neighborhood P2_00 B2_22,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 56 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 25 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern `elem` [26, 31] = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern `elem` [82, 214] = (
      blendHQ2x neighborhood P2_00 B2_22,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_21,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [88, 248] = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_22,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [74, 107] = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_21,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 27 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_10,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 86 = (
      blendHQ2x neighborhood P2_00 B2_22,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_21,
      blendHQ2x neighborhood P2_11 B2_10)
  | pattern == 216 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_10,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 106 = (
      blendHQ2x neighborhood P2_00 B2_10,
      blendHQ2x neighborhood P2_01 B2_21,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 30 = (
      blendHQ2x neighborhood P2_00 B2_10,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 210 = (
      blendHQ2x neighborhood P2_00 B2_22,
      blendHQ2x neighborhood P2_01 B2_10,
      blendHQ2x neighborhood P2_10 B2_21,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 120 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_22,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_10)
  | pattern == 75 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_10,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 29 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 198 = (
      blendHQ2x neighborhood P2_00 B2_22,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_21,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 184 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 99 = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 57 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 71 = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_21,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 156 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 226 = (
      blendHQ2x neighborhood P2_00 B2_22,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 60 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 195 = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_21,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 102 = (
      blendHQ2x neighborhood P2_00 B2_22,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 153 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 58 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 83 = (
      blendHQ2x neighborhood P2_00 B2_11,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      blendHQ2x neighborhood P2_10 B2_21,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 92 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_11,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 202 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      blendHQ2x neighborhood P2_01 B2_21,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 78 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      blendHQ2x neighborhood P2_01 B2_12,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 154 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 114 = (
      blendHQ2x neighborhood P2_00 B2_22,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      blendHQ2x neighborhood P2_10 B2_12,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 89 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_22,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 90 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern `elem` [55, 23] = (
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_00 B2_11
        else blendHQ2x neighborhood P2_00 B2_60,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_90,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern `elem` [182, 150] = (
      blendHQ2x neighborhood P2_00 B2_22,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_90,
      blendHQ2x neighborhood P2_10 B2_20,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_11 B2_12
        else blendHQ2x neighborhood P2_11 B2_61)
  | pattern `elem` [213, 212] = (
      blendHQ2x neighborhood P2_00 B2_20,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_01 B2_11
        else blendHQ2x neighborhood P2_01 B2_60,
      blendHQ2x neighborhood P2_10 B2_21,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_90)
  | pattern `elem` [241, 240] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_22,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_10 B2_12
        else blendHQ2x neighborhood P2_10 B2_61,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_90)
  | pattern `elem` [236, 232] = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_90,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_11 B2_11
        else blendHQ2x neighborhood P2_11 B2_60)
  | pattern `elem` [109, 105] = (
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_00 B2_12
        else blendHQ2x neighborhood P2_00 B2_61,
      blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_90,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern `elem` [171, 43] = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_90,
      blendHQ2x neighborhood P2_01 B2_21,
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_10 B2_11
        else blendHQ2x neighborhood P2_10 B2_60,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [143, 15] = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_90,
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_01 B2_12
        else blendHQ2x neighborhood P2_01 B2_61,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 124 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_11,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_10)
  | pattern == 203 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_10,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 62 = (
      blendHQ2x neighborhood P2_00 B2_10,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 211 = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_10,
      blendHQ2x neighborhood P2_10 B2_21,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 118 = (
      blendHQ2x neighborhood P2_00 B2_22,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_10)
  | pattern == 217 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_10,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 110 = (
      blendHQ2x neighborhood P2_00 B2_10,
      blendHQ2x neighborhood P2_01 B2_12,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 155 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_10,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 188 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 185 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_22,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 61 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 157 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 103 = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 227 = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_21,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 230 = (
      blendHQ2x neighborhood P2_00 B2_22,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 199 = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_21,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 220 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_11,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 158 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 234 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      blendHQ2x neighborhood P2_01 B2_21,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 242 = (
      blendHQ2x neighborhood P2_00 B2_22,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      blendHQ2x neighborhood P2_10 B2_12,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 59 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 121 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_22,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 87 = (
      blendHQ2x neighborhood P2_00 B2_11,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_21,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 79 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_12,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 122 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 94 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 218 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 91 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 229 = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 167 = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 173 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 181 = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 186 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 115 = (
      blendHQ2x neighborhood P2_00 B2_11,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      blendHQ2x neighborhood P2_10 B2_12,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 93 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_11,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 206 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      blendHQ2x neighborhood P2_01 B2_12,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern `elem` [205, 201] = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_10
        else blendHQ2x neighborhood P2_10 B2_70,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern `elem` [174, 46] = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_10
        else blendHQ2x neighborhood P2_00 B2_70,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [179, 147] = (
      blendHQ2x neighborhood P2_00 B2_11,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_10
        else blendHQ2x neighborhood P2_01 B2_70,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern `elem` [117, 116] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_12,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_10
        else blendHQ2x neighborhood P2_11 B2_70)
  | pattern == 189 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 231 = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 126 = (
      blendHQ2x neighborhood P2_00 B2_10,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_10)
  | pattern == 219 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_10,
      blendHQ2x neighborhood P2_10 B2_10,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 125 = (
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_00 B2_12
        else blendHQ2x neighborhood P2_00 B2_61,
      blendHQ2x neighborhood P2_01 B2_11,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_90,
      blendHQ2x neighborhood P2_11 B2_10)
  | pattern == 221 = (
      blendHQ2x neighborhood P2_00 B2_12,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_01 B2_11
        else blendHQ2x neighborhood P2_01 B2_60,
      blendHQ2x neighborhood P2_10 B2_10,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_90)
  | pattern == 207 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_90,
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_01 B2_12
        else blendHQ2x neighborhood P2_01 B2_61,
      blendHQ2x neighborhood P2_10 B2_10,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 238 = (
      blendHQ2x neighborhood P2_00 B2_10,
      blendHQ2x neighborhood P2_01 B2_12,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_90,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_11 B2_11
        else blendHQ2x neighborhood P2_11 B2_60)
  | pattern == 190 = (
      blendHQ2x neighborhood P2_00 B2_10,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_90,
      blendHQ2x neighborhood P2_10 B2_11,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_11 B2_12
        else blendHQ2x neighborhood P2_11 B2_61)
  | pattern == 187 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_90,
      blendHQ2x neighborhood P2_01 B2_10,
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_10 B2_11
        else blendHQ2x neighborhood P2_10 B2_60,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 243 = (
      blendHQ2x neighborhood P2_00 B2_11,
      blendHQ2x neighborhood P2_01 B2_10,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_10 B2_12
        else blendHQ2x neighborhood P2_10 B2_61,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_90)
  | pattern == 119 = (
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_00 B2_11
        else blendHQ2x neighborhood P2_00 B2_60,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_90,
      blendHQ2x neighborhood P2_10 B2_12,
      blendHQ2x neighborhood P2_11 B2_10)
  | pattern `elem` [237, 233] = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_100,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern `elem` [175, 47] = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_100,
      blendHQ2x neighborhood P2_01 B2_12,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_20)
  | pattern `elem` [183, 151] = (
      blendHQ2x neighborhood P2_00 B2_11,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_100,
      blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern `elem` [245, 244] = (
      blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_11,
      blendHQ2x neighborhood P2_10 B2_12,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_100)
  | pattern == 250 = (
      blendHQ2x neighborhood P2_00 B2_10,
      blendHQ2x neighborhood P2_01 B2_10,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 123 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_10,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_10)
  | pattern == 95 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_10,
      blendHQ2x neighborhood P2_11 B2_10)
  | pattern == 222 = (
      blendHQ2x neighborhood P2_00 B2_10,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_10,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 252 = (
      blendHQ2x neighborhood P2_00 B2_21,
      blendHQ2x neighborhood P2_01 B2_11,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_100)
  | pattern == 249 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_22,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_100,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 235 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_21,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_100,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 111 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_100,
      blendHQ2x neighborhood P2_01 B2_12,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_22)
  | pattern == 63 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_100,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_21)
  | pattern == 159 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_100,
      blendHQ2x neighborhood P2_10 B2_22,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 215 = (
      blendHQ2x neighborhood P2_00 B2_11,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_100,
      blendHQ2x neighborhood P2_10 B2_21,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 246 = (
      blendHQ2x neighborhood P2_00 B2_22,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      blendHQ2x neighborhood P2_10 B2_12,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_100)
  | pattern == 254 = (
      blendHQ2x neighborhood P2_00 B2_10,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_100)
  | pattern == 253 = (
      blendHQ2x neighborhood P2_00 B2_12,
      blendHQ2x neighborhood P2_01 B2_11,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_100,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_100)
  | pattern == 251 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      blendHQ2x neighborhood P2_01 B2_10,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_100,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 239 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_100,
      blendHQ2x neighborhood P2_01 B2_12,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_100,
      blendHQ2x neighborhood P2_11 B2_11)
  | pattern == 127 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_100,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_20,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_20,
      blendHQ2x neighborhood P2_11 B2_10)
  | pattern == 191 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_100,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_100,
      blendHQ2x neighborhood P2_10 B2_11,
      blendHQ2x neighborhood P2_11 B2_12)
  | pattern == 223 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_20,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_100,
      blendHQ2x neighborhood P2_10 B2_10,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_20)
  | pattern == 247 = (
      blendHQ2x neighborhood P2_00 B2_11,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_100,
      blendHQ2x neighborhood P2_10 B2_12,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_100)
  | pattern == 255 = (
      if isDiff neigborhood 4 2
        then blendHQ2x neighborhood P2_00 B2_0
        else blendHQ2x neighborhood P2_00 B2_100,
      if isDiff neigborhood 2 6
        then blendHQ2x neighborhood P2_01 B2_0
        else blendHQ2x neighborhood P2_01 B2_100,
      if isDiff neigborhood 8 4
        then blendHQ2x neighborhood P2_10 B2_0
        else blendHQ2x neighborhood P2_10 B2_100,
      if isDiff neigborhood 6 8
        then blendHQ2x neighborhood P2_11 B2_0
        else blendHQ2x neighborhood P2_11 B2_100)
  where pattern = hq2xGetPattern neighborhood
