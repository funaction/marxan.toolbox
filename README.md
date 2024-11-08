# marxan.toolbox
utiliy functions to prepare marxan input data, calibrate the BLM input parameter, run marxan from R environment and get the best solution

author:  Daniel Romero Mujalli<br/>
email:   danielrm84@gmail.com<br/>
updated: 20241108<br/> 

## Installation
(requires devtools)<br/>
type on R console:<br/>
> devtools::install_github("danielrm84/marxan.toolbox")

## Dependencies
"riverPlanningTools" by José Salgado-Rojas<br/>
pkg link:<br/>
https://github.com/josesalgr/riverPlanningTools/tree/v0.1

## Usage
please consult the corresponding documentation which can be achived by typing the indicated R command followed by the character ">"

### create the 
- planning unit input file "pu.csv"
  > ?marxan.toolbox::crt_pu()<br/>
- planning unit vs conservation feature input file "puvspr.csv"
  > ?marxan.toolbox::crt_puvspr()<br/>
- conservation feature input file "spec.csv"
  > ?marxan.toolbox::crt_spec()<br/>
- boundary length file "bound.csv"
  > ?marxan.toolbox::crt_bound()<br/>
- calibrattion of the BLM parameter value
  > ?marxan.toolbox::calibrate_blm()<br/>
- get the best solution found by the marxan optimization algorithm
  > ?marxan.toolbox::get_best_solution()<br/>
- run marxan from R environment
  > ?marxan.toolbox::run_marxan()<br/>

