module Src.Algorithms.HQXPatterns.HQ3X where

import Src.Algorithms.HQXPatterns.HQXCommon
import Src.Algorithms.Common

data PositionPatternHQ3x = P3_00 | P3_01 | P3_02 | P3_10 | P3_11 | P3_12 | P3_20 | P3_21 | P3_22
data BlendTypeHQ3x = B3_1M | B3_1U | B3_1L | B3_2 | B3_4 | B3_5 | B3_C | B3_1 | B3_3 | B3_6 | B3_1R | B3_ | B3_1D

blendHQ3x :: Square3x3 RGBPixel -> PositionPatternHQ3x -> BlendTypeHQ3x -> RGBPixel
blendHQ3x neighborhood P3_00 B3_1M = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 1 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_00 B3_1U = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 2 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_00 B3_1L = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 4 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_00 B3_2 = interpHQX 2 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 4 neighborhood) (getNthColorFromNeighborhood 2 neighborhood)
blendHQ3x neighborhood P3_00 B3_4 = interpHQX 4 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 4 neighborhood) (getNthColorFromNeighborhood 2 neighborhood)
blendHQ3x neighborhood P3_00 B3_5 = interpHQX 5 (getNthColorFromNeighborhood 4 neighborhood) (getNthColorFromNeighborhood 2 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_00 B3_C = getNthColorFromNeighborhood 5 neighborhood

blendHQ3x neighborhood P3_01 B3_1 = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 2 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_01 B3_3 = interpHQX 3 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 2 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_01 B3_6 = interpHQX 1 (getNthColorFromNeighborhood 2 neighborhood) (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_01 B3_C = getNthColorFromNeighborhood 5 neighborhood

blendHQ3x neighborhood P3_02 B3_1M = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 3 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_02 B3_1U = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 2 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_02 B3_1R = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 6 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_02 B3_2 = interpHQX 2 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 2 neighborhood) (getNthColorFromNeighborhood 6 neighborhood)
blendHQ3x neighborhood P3_02 B3_4 = interpHQX 4 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 2 neighborhood) (getNthColorFromNeighborhood 6 neighborhood)
blendHQ3x neighborhood P3_02 B3_5 = interpHQX 5 (getNthColorFromNeighborhood 2 neighborhood) (getNthColorFromNeighborhood 6 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_02 B3_C = getNthColorFromNeighborhood 5 neighborhood

blendHQ3x neighborhood P3_10 B3_1 = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 4 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_10 B3_3 = interpHQX 3 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 4 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_10 B3_6 = interpHQX 1 (getNthColorFromNeighborhood 4 neighborhood) (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_10 B3_C = getNthColorFromNeighborhood 5 neighborhood

blendHQ3x neighborhood P3_11 _ = getNthColorFromNeighborhood 5 neighborhood

blendHQ3x neighborhood P3_12 B3_1 = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 6 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_12 B3_3 = interpHQX 3 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 6 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_12 B3_6 = interpHQX 1 (getNthColorFromNeighborhood 6 neighborhood) (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_12 B3_C = getNthColorFromNeighborhood 5 neighborhood

blendHQ3x neighborhood P3_20 B3_1M = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 7 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_20 B3_1D = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 8 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_20 B3_1L = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 4 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_20 B3_2 = interpHQX 2 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 8 neighborhood) (getNthColorFromNeighborhood 4 neighborhood)
blendHQ3x neighborhood P3_20 B3_4 = interpHQX 4 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 8 neighborhood) (getNthColorFromNeighborhood 4 neighborhood)
blendHQ3x neighborhood P3_20 B3_5 = interpHQX 5 (getNthColorFromNeighborhood 8 neighborhood) (getNthColorFromNeighborhood 4 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_20 B3_C = getNthColorFromNeighborhood 5 neighborhood

blendHQ3x neighborhood P3_21 B3_1 = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 8 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_21 B3_3 = interpHQX 3 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 8 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_21 B3_6 = interpHQX 1 (getNthColorFromNeighborhood 8 neighborhood) (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_21 B3_C = getNthColorFromNeighborhood 5 neighborhood

blendHQ3x neighborhood P3_22 B3_1M = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 9 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_22 B3_1D = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 8 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_22 B3_1R = interpHQX 1 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 6 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_22 B3_2 = interpHQX 2 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 6 neighborhood) (getNthColorFromNeighborhood 8 neighborhood)
blendHQ3x neighborhood P3_22 B3_4 = interpHQX 4 (getNthColorFromNeighborhood 5 neighborhood) (getNthColorFromNeighborhood 6 neighborhood) (getNthColorFromNeighborhood 8 neighborhood)
blendHQ3x neighborhood P3_22 B3_5 = interpHQX 5 (getNthColorFromNeighborhood 6 neighborhood) (getNthColorFromNeighborhood 8 neighborhood) (getNthColorFromNeighborhood 5 neighborhood)
blendHQ3x neighborhood P3_22 B3_C = getNthColorFromNeighborhood 5 neighborhood

getPixelValueHQ3x :: Square3x3 RGBPixel -> Square3x3 RGBPixel
getPixelValueHQ3x neighborhood
  | pattern `elem` [0, 1, 4, 32, 128, 5, 132, 160, 33, 129, 36, 133, 164, 161, 37, 165] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [2, 34, 130, 162] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [16, 17, 48, 49] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [64, 65, 68, 69] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [8, 12, 136, 140] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [3, 35, 131, 163] = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [6, 38, 134, 166] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [20, 21, 52, 53] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [144, 145, 176, 177] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern `elem` [192, 193, 196, 197] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern `elem` [96, 97, 100, 101] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [40, 44, 168, 172] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [9, 13, 137, 141] = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [18, 50] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [80, 81] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern `elem` [72, 76] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [10, 138] = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 66 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 24 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [7, 39, 135] = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [148, 149, 180] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern `elem` [224, 228, 225] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern `elem` [41, 169, 45] = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [22, 54] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [208, 209] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern `elem` [104, 108] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [11, 139] = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [19, 51] = (
      if isDiff 2 6
        then blendHQ3x neighborhood P3_00 B3_1L
        else blendHQ3x neighborhood P3_00 B3_2,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_6,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_5,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [146, 178] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_1,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_5,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_6,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_22 B3_1D
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [84, 85] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_02 B3_1U
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_6,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_1,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_5)
  | pattern `elem` [112, 113] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_20 B3_1L
        else blendHQ3x neighborhood P3_20 B3_2,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_6,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_5)
  | pattern `elem` [200, 204] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_5,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_6,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_22 B3_1R
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [73, 77] = (
      if isDiff 8 4
        then blendHQ3x neighborhood P3_00 B3_1U
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_6,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_5,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [42, 170] = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_5,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_6,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_20 B3_1D
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [14, 142] = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_5,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_6,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_02 B3_1R
        else blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 67 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 70 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 28 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 152 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 194 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 98 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 56 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 25 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [26, 31] = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [82, 214] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern `elem` [88, 248] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern `elem` [74, 107] = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 27 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 86 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 216 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 106 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 30 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 210 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 120 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 75 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 29 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 198 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 184 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 99 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 57 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 71 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 156 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 226 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 60 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 195 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 102 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 153 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 58 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 83 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 92 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 202 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 78 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 154 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 114 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 89 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 90 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [55, 23] = (
      if isDiff 2 6
        then blendHQ3x neighborhood P3_00 B3_1L
        else blendHQ3x neighborhood P3_00 B3_2,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_6,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_5,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [182, 150] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_1,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_5,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_6,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_22 B3_1D
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [213, 212] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_02 B3_1U
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_6,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_1,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_5)
  | pattern `elem` [241, 240] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_20 B3_1L
        else blendHQ3x neighborhood P3_20 B3_2,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_6,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_5)
  | pattern `elem` [236, 232] = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_5,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_6,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_22 B3_1R
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [109, 105] = (
      if isDiff 8 4
        then blendHQ3x neighborhood P3_00 B3_1U
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_6,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_5,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [171, 43] = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_5,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_6,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_20 B3_1D
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [143, 15] = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_5,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_6,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_02 B3_1R
        else blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 124 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 203 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 62 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 211 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 118 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 217 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 110 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 155 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 188 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 185 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 61 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 157 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 103 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 227 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 230 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 199 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 220 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 158 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 234 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 242 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1L,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 59 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 121 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 87 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 79 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1R,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 122 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 94 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 218 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 91 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 229 = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 167 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 173 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 181 = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 186 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 115 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 93 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 206 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern `elem` [205, 201] = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_1M
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern `elem` [174, 46] = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_1M
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [179, 147] = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_1M
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern `elem` [117, 116] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_1M
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 189 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 231 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 126 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 219 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 125 = (
      if isDiff 8 4
        then blendHQ3x neighborhood P3_00 B3_1U
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_6,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_5,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 221 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_02 B3_1U
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_6,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_1,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_5)
  | pattern == 207 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_5,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_6,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_02 B3_1R
        else blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 238 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_5,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_6,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_22 B3_1R
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 190 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_1,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_5,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_6,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_22 B3_1D
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 187 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_5,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_6,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_20 B3_1D
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 243 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_20 B3_1L
        else blendHQ3x neighborhood P3_20 B3_2,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_6,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_5)
  | pattern == 119 = (
      if isDiff 2 6
        then blendHQ3x neighborhood P3_00 B3_1L
        else blendHQ3x neighborhood P3_00 B3_2,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_6,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_5,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern `elem` [237, 233] = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern `elem` [175, 47] = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_2)
  | pattern `elem` [183, 151] = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern `elem` [245, 244] = (
      blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 250 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 123 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 95 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 222 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 252 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 249 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 235 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1M,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 111 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 63 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 159 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 215 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 246 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 254 = (
      blendHQ3x neighborhood P3_00 B3_1M,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 253 = (
      blendHQ3x neighborhood P3_00 B3_1U,
      blendHQ3x neighborhood P3_01 B3_1,
      blendHQ3x neighborhood P3_02 B3_1U,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 251 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      blendHQ3x neighborhood P3_02 B3_1M,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_2,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 239 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      blendHQ3x neighborhood P3_02 B3_1R,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_1,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      blendHQ3x neighborhood P3_22 B3_1R)
  | pattern == 127 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_2,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_4,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_4,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      blendHQ3x neighborhood P3_22 B3_1M)
  | pattern == 191 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1D,
      blendHQ3x neighborhood P3_21 B3_1,
      blendHQ3x neighborhood P3_22 B3_1D)
  | pattern == 223 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_4,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_01 B3_C
        else blendHQ3x neighborhood P3_01 B3_3,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_2,
      if isDiff 4 2
        then blendHQ3x neighborhood P3_10 B3_C
        else blendHQ3x neighborhood P3_10 B3_3,
      blendHQ3x neighborhood P3_11 B3_,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_12 B3_C
        else blendHQ3x neighborhood P3_12 B3_3,
      blendHQ3x neighborhood P3_20 B3_1M,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_21 B3_C
        else blendHQ3x neighborhood P3_21 B3_3,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_4)
  | pattern == 247 = (
      blendHQ3x neighborhood P3_00 B3_1L,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_1,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      blendHQ3x neighborhood P3_20 B3_1L,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_2)
  | pattern == 255 = (
      if isDiff 4 2
        then blendHQ3x neighborhood P3_00 B3_C
        else blendHQ3x neighborhood P3_00 B3_2,
      blendHQ3x neighborhood P3_01 B3_C,
      if isDiff 2 6
        then blendHQ3x neighborhood P3_02 B3_C
        else blendHQ3x neighborhood P3_02 B3_2,
      blendHQ3x neighborhood P3_10 B3_C,
      blendHQ3x neighborhood P3_11 B3_,
      blendHQ3x neighborhood P3_12 B3_C,
      if isDiff 8 4
        then blendHQ3x neighborhood P3_20 B3_C
        else blendHQ3x neighborhood P3_20 B3_2,
      blendHQ3x neighborhood P3_21 B3_C,
      if isDiff 6 8
        then blendHQ3x neighborhood P3_22 B3_C
        else blendHQ3x neighborhood P3_22 B3_2)
  where pattern = hqxGetPattern neighborhood
        isDiff a b = isDifferent (getNthColorFromNeighborhood a neighborhood) (getNthColorFromNeighborhood b neighborhood)
