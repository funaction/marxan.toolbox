# marxan.toolbox
utiliy functions to prepare marxan input data, calibrate the BLM input parameter, run marxan from R environment and get the best solution

author:  Daniel Romero Mujalli<br>
email:   danielrm84@gmail.com<br>
updated: 20251028<br> 

## Installation
(can be installed via remotes or devtools)<br>
type on R console:<br>
> devtools::install_github("danielrm84/marxan.toolbox")

## Dependencies
"googledrive"<br>
"riverPlanningTools" by José Salgado-Rojas link:
https://github.com/josesalgr/riverPlanningTools/tree/v0.1<br>
"scales"<br>
"terra"<br>
"rivnet"<br>
"elevatr"<br>

## Usage
please consult the corresponding documentation which can be achived by typing the indicated R command

### method for
- planning unit input file "pu.csv"
  > ?marxan.toolbox::crt_pu()<br>
- planning unit vs conservation feature input file "puvspr.csv"
  > ?marxan.toolbox::crt_puvspr()<br>
- conservation feature input file "spec.csv"
  > ?marxan.toolbox::crt_spec()<br>
- boundary length file "bound.csv"
  > ?marxan.toolbox::crt_bound()<br>
- calibration of the BLM parameter value
  > ?marxan.toolbox::calibrate_blm()<br>
- get the best solution found by the marxan optimization algorithm
  > ?marxan.toolbox::get_best_solution()<br>
- run marxan from R environment
  > ?marxan.toolbox::run_marxan()<br>

### delineate basin
  > ?marxan.toolbox::delineate_basin()<br>
