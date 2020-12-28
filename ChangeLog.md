# Revision history for dobutokO4

## 0.1.0.0 -- 2020-06-02

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2020-06-02

* First version revised A. Fixed issue with being not compiled for GHC <= 8.4.*.

## 0.2.0.0 -- 2020-06-09

* Second version. Added new functions to the DobutokO.Sound.Faded module with 'N' ending to generalize the name(s) of the resulting 
file(s). Some minor code improvements.

## 0.2.1.0 -- 2020-06-09

* Second version. Added a README.markdown file. 

## 0.3.0.0 -- 2020-06-09

* Third version. Added a new data type to the DobutokO.Sound.Presentation module (IntervalMG), some changes to the data types already present. 
Added more instances. Some minor documentation improvements.

## 0.4.0.0 -- 2020-06-09

* Fourth version. Added moreF-based functions to the DobutokO.Sound.Faded module. They allow to create a sequence of sounds with frequency modulation.

## 0.5.0.0 -- 2020-06-19

* Fifth version. Added new functions that allow to reduce unpleasant noise for the sounds created with DobutokO.Sound.Faded module functions. 

## 0.6.0.0 -- 2020-06-23

* Sixth version. Changed a lot of Double arguments in the DobutokO.Sound.Faded to Float ones. This change is inspired by: https://www.youtube.com/watch?v=FYTZkE5BZ-0

## 0.7.0.0 -- 2020-06-24

* Seventh version. Completed changes from Double to Float started in the 0.6.0.0 version. Moved DobutokO.Sound.Faded.double42Float4 function to Data.Vector.DoubleZipped 
module.

## 0.8.0.0 -- 2020-08-16

* Eighth version. Changed the dependency boundaries.
