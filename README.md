# marxan.toolbox
utiliy functions to perform conservation planning using the software [Marxan](https://marxansolutions.org/) from the R environment. 
The package is well suit for planning exercises on sea, land and freshwater systems. 
the package was developed with a mindset of conservation planning of freshwater biodiversity. 
Therefore, the package includes methods for: 
1. obtaining a Digital Elevation Model DEM for the extent of interest
2. extraction of the associated river network
3. delineation of sub-basin polygons
4. delineation of the basin area of interest (modelling region)
5. training of habitat distribution models and their spatial projections onto the modelling region
6. conservation planning via Marxan. This involves the preparation of marxan input data, calibration of the BLM input parameter, run of marxan from R environment and a method to get the best solution.

author:  Daniel Romero Mujalli<br>
email:   daniel.romero@supsi.ch<br>
updated: 20251104<br> 

This package was developed in the context of the Biodiversa+ project FUNACTION

## Installation
(can be installed via remotes or devtools)<br>
type on R console:<br>
> remotes::install_github("danielrm84/marxan.toolbox")

## Dependencies
"googledrive"<br>
["riverPlanningTools"](https://github.com/josesalgr/riverPlanningTools/tree/v0.1) by José Salgado-Rojas<br>
"scales"<br>
"terra"<br>
"rivnet"<br>
"elevatr"<br>

## Usage
please consult the corresponding documentation which can be achived by typing the indicated R command

### get digital elevation model
  > ?marxan.toolbox::getDEM()<br>

### extract river network
  > ?marxan.toolbox::rivnet_extract_river()<br>

### delineate sub-basin polygons
  > ?marxan.toolbox::get_rivnet_SC()<br>

### delineate basin area (modelling region)
  > ?marxan.toolbox::delineate_basin()<br>

### Marxan conservation planning
planning unit input file "pu.csv"
  > ?marxan.toolbox::crt_pu()<br>
planning unit vs conservation feature input file "puvspr.csv"
  > ?marxan.toolbox::crt_puvspr()<br>
conservation feature input file "spec.csv"
  > ?marxan.toolbox::crt_spec()<br>
boundary length file "bound.csv"
  > ?marxan.toolbox::crt_bound()<br>
calibration of the BLM parameter value
  > ?marxan.toolbox::calibrate_blm()<br>
get the best solution found by the marxan optimization algorithm
  > ?marxan.toolbox::get_best_solution()<br>
run marxan from R environment
  > ?marxan.toolbox::run_marxan()<br>




