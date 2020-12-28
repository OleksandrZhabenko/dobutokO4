-- |
-- Module      :  DobutokO.Sound.Presentation
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- This module contains different representations for the data.

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP #-}

module DobutokO.Sound.Presentation (
  -- * Sound repesentations
  SoundI (..)
  , SoundFN (..)
  , SoundT (..)
  -- * Sound time intervals representations
  , Timity (..)
  , Timity1 (..)
  , IntervalTim (..)
  , IntervalTimI (..)
  , IntervalG (..)
  , IntervalMG (..)
) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import Data.Bifunctor 
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.Functional.Basics
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=804
/* code that applies only to GHC 8.4.* and higher versions */
import Data.Semigroup
#endif
#endif
import Data.Monoid

-- | An 'Int' parameter is an index of the 'SoundI' sound file in the sorted in the ascending order 'V.Vector' of them (the corresponding files or their 
-- names) representing the whole composition.
data SoundI = Si Int Float Float OvertonesO | SAi Int Float Float Float OvertonesO | SAbi Int Float Float Float OvertonesO
  deriving Eq

----------------------------------------------------------------------------------

-- | An 'FilePath' parameter is a name of the sound file in the current directory with the filetype (supported by SoX) being given by 'String' representing 
-- the whole composition.
data SoundFN = Sn FilePath String Float Float | SAn FilePath String Float Float Float | SAbn FilePath String Float Float Float
  deriving Eq

----------------------------------------------------------------------------------

-- | The first 'Float' parameter is a time moment (starting from 0) of the playing the sound being represented by 'OvertonesO', the second one is its 
-- duration. The third one is its maximum amplitude by an absolute value. The fourth one is the minimum duration that can provide a needed human 
-- feeling of perception (some impression) for the sound. The further one(s) is(are) some adjustment(s) parameter(s).
data SoundT = StO Float Float Float Float OvertonesO | SAtO Float Float Float Float Float OvertonesO | 
  SAbtO Float Float Float Float Float OvertonesO deriving Eq

----------------------------------------------------------------------------------

-- | The first 'Float' parameter is a time moment (starting from 0) of the playing the sound, the second one is its duration in seconds (with a negative 
-- values corresponding to the pause duration --- the silent \"sound\"), the third one is the minimum duration that can provide a needed human 
-- feeling of perception (some impression) for the sound.
data Timity = Time Float Float Float deriving Eq

instance Ord Timity where 
  compare (Time t01 t11 t21) (Time t02 t12 t22) 
    | t01 /= t02 = compare t01 t02
    | abs t11 /= abs t12 = compare (abs t11) (abs t12)
    | otherwise = compare (abs t22) (abs t21)

instance Show Timity where
  show (Time t0 t1 t2) = showFFloat Nothing t0 ":(" ++ showFFloat Nothing t1 "):(" ++ showFFloat Nothing t2 ")"

-- | The first 'Float' parameter is a time moment (starting from 0) of the playing the sound, the second one is its duration in seconds (with a negative 
-- values corresponding to the pause duration --- the silent \"sound\"), the third one is a parameter to specify more complex behaviour for the sound. 
data Timity1 a = Time1 Float Float a 

instance (Eq a) => Eq (Timity1 a) where
  (==) (Time1 x1 x2 a0) (Time1 x3 x4 a1)
    | a0 /= a1 = False
    | x1 /= x3 = False
    | otherwise = x2 == x4

instance (Ord a) => Ord (Timity1 a) where 
  compare (Time1 t01 t11 a0) (Time1 t02 t12 a1) 
    | t01 /= t02 = compare t01 t02
    | abs t11 /= abs t12 = compare (abs t11) (abs t12)
    | otherwise = compare a0 a1

instance (Show a) => Show (Timity1 a) where
  show (Time1 t0 t1 a1) = showFFloat Nothing t0 ":(" ++ showFFloat Nothing t1 "):(" ++ show a1 ++ ")"  

instance Functor Timity1 where
  fmap f (Time1 t1 t2 a0) = Time1 t1 t2 (f a0)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=804
/* code that applies only to GHC 8.4.* and higher versions */
-- | Since base-4.9.0.0. Idempotent semigroup (band) (x <> x == x) if @Semigroup a@ is idempotent (is a band).
instance (Semigroup a) => Semigroup (Timity1 a) where
  (<>) (Time1 t01 t11 a0) (Time1 t02 t12 a1) = Time1 (min t01 t02) ((signum (t11 * t12)) * (max (t01 + (abs t11)) (t02 + (abs t12)) - min t01 t02)) 
          (a0 <> a1)
#endif
#endif

-- | 'Float' interval representation with no order of the arguments preserved.
data IntervalTim = Empty | I Float Float | UniversalI

instance Eq IntervalTim where
  (==) (I x1 x2) (I y1 y2) 
    | x1 /= y1 = if x1 == y2 then x2 == y1 else False
    | otherwise = x2 == y2
  (==) UniversalI UniversalI = True
  (==) Empty Empty = True
  (==) _ _ = False

instance Ord IntervalTim where 
  compare (I x01 x02) (I x11 x12) 
    | min x01 x02 == min x11 x12 = compare (max x01 x02) (max x11 x12)
    | otherwise = compare (min x01 x02) (min x11 x12)
  compare UniversalI UniversalI = EQ
  compare Empty Empty = EQ
  compare _ UniversalI = LT
  compare _ Empty = GT
  compare UniversalI _ = GT
  compare _ _ = LT

instance Show IntervalTim where
  show Empty = "()"
  show (I x1 x2) 
    | compare x1 x2 /= GT = "[" ++ showFFloat Nothing x1 ", " ++ showFFloat Nothing x2 "]"
    | otherwise = "[" ++ showFFloat Nothing x2 ", " ++ showFFloat Nothing x1 "]"
  show UniversalI = "(-Infinity..+Infinity)"

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=804
/* code that applies only to GHC 8.4.* and higher versions */
-- | Since base-4.9.0.0. Idempotent semigroup (x <> x == x) -- band. 
instance Semigroup IntervalTim where
  (<>) (I x01 x02) (I x11 x12) = I (minimum [x01,x02,x11,x12]) (maximum [x01,x02,x11,x12])
  (<>) Empty x = x
  (<>) x Empty = x
  (<>) _ _ = UniversalI
#endif
#endif

instance Monoid IntervalTim where
  mempty = Empty
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__<=802
/* code that applies only to GHC 8.2.* and lower versions */
  mappend Empty x = x
  mappend x Empty = x
  mappend (I x01 x02) (I x11 x12) = I (minimum [x01,x02,x11,x12]) (maximum [x01,x02,x11,x12])
  mappend _ _ = UniversalI
#endif
#endif
  
-- | Another 'Float' interval representation with no order of the arguments preserved. Since base-4.9.0.0 has different instance of 'Semigroup' 
-- than 'IntervalTim'.
data IntervalTimI = Empty2 | II Float Float | UniversalII
  
instance Eq IntervalTimI where
  (==) (II x1 x2) (II y1 y2) 
    | x1 /= y1 = if x1 == y2 then x2 == y1 else False
    | otherwise = x2 == y2
  (==) Empty2 Empty2 = True
  (==) UniversalII UniversalII = True
  (==) _ _ = False

instance Ord IntervalTimI where 
  compare (II x01 x02) (II x11 x12) 
    | min x01 x02 == min x11 x12 = compare (max x01 x02) (max x11 x12)
    | otherwise = compare (min x01 x02) (min x11 x12)
  compare Empty2 Empty2 = EQ
  compare Empty2 _ = LT
  compare UniversalII UniversalII = EQ
  compare UniversalII _ = GT
  compare _ Empty2 = GT
  compare _ _ = LT

instance Show IntervalTimI where
  show Empty2 = "()"
  show (II x1 x2) 
    | compare x1 x2 /= GT = "[" ++ showFFloat Nothing x1 ", " ++ showFFloat Nothing x2 "]"
    | otherwise = "[" ++ showFFloat Nothing x2 ", " ++ showFFloat Nothing x1 "]"
  show UniversalII = "(-Infinity..+Infinity)"

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=804
/* code that applies only to GHC 8.4.* and higher versions */
-- | Since base-4.9.0.0. Idempotent semigroup (x <> x == x) -- band. (<>) can be understood as an intersection of the sets.
instance Semigroup IntervalTimI where
  (<>) Empty2 x = Empty2
  (<>) x Empty2 = Empty2
  (<>) (II x01 x02) (II x11 x12) = if compare (max (min x01 x02) (min x11 x12)) (min (max x01 x02) (max x11 x12)) /= GT 
    then II (max (min x01 x02) (min x11 x12)) (min (max x01 x02) (max x11 x12)) 
    else Empty2
  (<>) (II x y) _ = II x y
  (<>) _ (II x y) = II x y
  (<>) _ _ = UniversalII
#endif
#endif  

-- | Can be understood as an intersection of the sets.
instance Monoid IntervalTimI where
  mempty = Empty2
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__<=802
/* code that applies only to GHC 8.2.* and lower versions */
  mappend Empty2 x = x
  mappend x Empty2 = x
  mappend (II x01 x02) (II x11 x12) = if compare (max (min x01 x02) (min x11 x12)) (min (max x01 x02) (max x11 x12)) /= GT 
    then II (max (min x01 x02) (min x11 x12)) (min (max x01 x02) (max x11 x12)) 
    else Empty2
  mappend _ _ = UniversalII
#endif
#endif
    
-- | The first 'Float' parameter is some adjustment parameter for the playing sound being represented by 'OvertonesO'.
data SoundTim = StOm Timity Float OvertonesO | SAtOm Timity Float Float OvertonesO | SAbtOm Timity Float Float OvertonesO
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------------------

-- | Generalized interval representation.
data IntervalG a b = IG a b deriving (Eq, Ord)

instance (Show a, Show b) => Show (IntervalG a b) where
  show (IG x y) = "[|" ++ show x ++ " __ " ++ show y ++ "|]"

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=804
/* code that applies only to GHC 8.4.* and higher versions */
-- | Since base-4.9.0.0. Idempotent semigroup (x <> x == x) and rectangular band (x <> y <> z == x <> z)
-- For more information, please, refer to: https://en.wikipedia.org/wiki/Band_(mathematics)
instance Semigroup (IntervalG a b) where
  (<>) (IG x0 _) (IG _ w1) = IG x0 w1
#endif
#endif  

instance Functor (IntervalG a) where
  fmap f (IG a b) = IG a (f b)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
-- | Since base-4.8.0.0. 
instance Bifunctor IntervalG where
  bimap f g (IG x y) = IG (f x) (g y)
#endif
#endif  

-- | Generalized interval representation which is a Monoid instance.
data IntervalMG a = IMG a a | UniversalG deriving (Eq, Ord)

instance (Show a) => Show (IntervalMG a) where
  show (IMG x y) = "[|" ++ show x ++ " __ " ++ show y ++ "|]"
  show UniversalG = "(-InfinityMG..+InfinityMG)"

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=804
/* code that applies only to GHC 8.4.* and higher versions */
-- | Since base-4.9.0.0. Idempotent semigroup (x <> x == x) and rectangular band (x <> y <> z == x <> z)
-- For more information, please, refer to: https://en.wikipedia.org/wiki/Band_(mathematics)
instance Semigroup (IntervalMG a) where
  (<>) (IMG x0 _) (IMG _ w1) = IMG x0 w1
  (<>) (IMG x y) _ = IMG x y
  (<>) _ (IMG x y) = IMG x y
  (<>) _ _ = UniversalG
#endif
#endif  

instance Monoid (IntervalMG a) where
  mempty = UniversalG
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__<=802
/* code that applies only to GHC 8.2.* and lower versions */
  mappend UniversalG x = x
  mappend x UniversalG = x
  mappend (IMG a1 a2) (IMG a3 a4) = IMG a1 a4
#endif
#endif
