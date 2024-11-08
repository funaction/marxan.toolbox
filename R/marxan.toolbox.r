#'#############################################################
#'
#'              MAIN: TOOLBOX FOR
#'                    SYSTEMATIC CONSERVATION PLANNING
#'                    USING MARXAN
#'                    
#' description: utiliy functions to prepare marxan input data,
#'              calibrate the BLM input parameter, run marxan
#'              from R environment and get the best solution
#'
#' author:       Daniel Romero Mujalli
#' email:        danielrm84@gmail.com / daniel.romero@supsi.ch
#'
#' created:      20241105
#' last update:  20241108
#'
#'#############################################################
#'#############################################################
#' crt_pu
#'
#' DESCRIPTION
#' create the planning unit input file, called "pu.csv". It
#' returns the pu data frame object, and writes the pu.csv
#' file to outdir directory if requested by the user 
#' (default = TRUE)
#'
#' PARAMETERS
#' @param id the planning unit identification number (vector) 
#'
#' @param cost the acquisition cost of the planning unit 
#' (vector)
#' roundtables with stakeholders can help defining the costs 
#' (one can use surrogates, eg., landuse, human foot print, 
#' ...)
#'
#' @param status selectability status of the planning unit 
#' (vector). Admitted values: 
#' 0, open for optimization
#' 2, lock-in, must be selected in the solution
#' 3, lock-out, is forbidden, not selectable
#'
#' @param crt_output_file whether to write the output to a csv
#' file default TRUE
#'
#' @param outdir directory to write the output pu.csv file
#' default = getwd(), currently directory
#'
#' OUTPUT
#' @return pu data frame
#' data format (based on column ids): id, cost, status
#'
#' @export
crt_pu <- function(
    id
    ,cost
    ,status
    ,crt_output_file = TRUE
    ,outdir = getwd()
)
{
    fname <- "pu.csv"
    pu <- data.frame(
        "id"      = id
        ,"cost"   = cost
        ,"status" = status  
    )
    if(crt_output_file)
        write.table(pu, file.path(outdir,fname)
                   ,sep = ",",quote = FALSE, row.names = FALSE
                   )
    return(pu)
}
#'#############################################################
#' crt_puvspr
#'
#' DESCRIPTION
#' create the planning unit vs conservation feature input file, 
#' called "puvspr.csv". The function returns the puvspr data 
#' frame object, and writes the result to outdir directory if 
#' requested by the user (default = TRUE)
#'
#' PARAMETERS
#' @param df dataframe containing in its column names the 
#' species names and the variable names of their predicted 
#' occurrences/abundances per planning unit id 
#'
#' @param species_names vector with the names of the species
#'
#' @param predictions_colnames vector of variable names in df
#' identifying the predicted occurrences / abundances.
#' predictions_colnames needs to have the same length and
#' follow the same order as species_names. E.g., 
#' if species_names     = c("sp1",      "sp2"), 
#' predictions_colnames = c("pred_sp1", "pred_sp2")
#'
#' @param pu_id the planning unit identification number 
#' (vector)
#'
#' @param crt_output_file whether to write the output to a csv
#' file default TRUE
#'
#' @param outdir directory to write the output pu.csv file
#' default = getwd(), currently directory
#'
#' OUTPUT
#' @return pu data frame
#' data format (column ids): species, pu, amount
#'
#' @export
crt_puvspr <- function(
    df
    ,species_names
    ,predictions_colnames
    ,pu_id
    ,crt_output_file = TRUE
    ,outdir = getwd()
)
{
    fname <- "puvspr.csv"
    # species in ordinal numbers
    # sp: a species (spp, plural)
    spp <- species_names
    if(sum(is.character(species_names)) > 0)
        spp = seq(from = 1, to = length(species_names), by = 1)
    x = data.frame() # create empty dataframe
    for(i in 1:length(pu_id))
        x <- rbind(x,data.frame(
            "species" = spp
            ,"pu" = rep(x = pu_id[i], times = length(species_names))
            ,"amount" = t(df[i,predictions_colnames])[,1]
        ))
    # write x to file
    if(crt_output_file)
        write.table(x, file.path(outdir,fname)
                   ,sep = ",",quote = FALSE, row.names = FALSE
                   )
    return(x)
}
#'#############################################################
#' crt_spec
#'
#' DESCRIPTION
#' create the conservation feature input file, called 
#' "spec.csv". The function returns the spec data 
#' frame object, and writes the result to outdir directory if 
#' requested by the user (default = TRUE)
#' the spec file contains information about each of the
#' conservation features being considered, such as their name, 
#' target amount, and the penalty if the target is not met.
#'
#' PARAMETERS
#' @param puvspr object returned by crt_puvspr() or the puvspr
#' file read as dataframe
#'
#' @param species_names vector with the names of the species
#'
#' @param percent_target vector, target percent coverage per
#' species (in proportions, i.e., in range (0, 1)). Its length
#' must be equal to the length of species names. If single
#' value, then all species have the same target. Default = 0.25
#'
#' @param penalty vector species penalty factor spf. 
#' Penalization for not achieving the target. Must be of length
#' equal to species names (i.e., one spf value per species). If
#' single value, then the same penalization is assumed for all
#' species. Default = 100
#'
#' @param crt_output_file whether to write the output to a csv
#' file default TRUE
#'
#' @param outdir directory to write the output pu.csv file
#' default = getwd(), currently directory
#'
#' OUTPUT
#' @return pu data frame
#' data format (column ids): id, target, name, spf
#'
#' @export
crt_spec <- function(
    puvspr
    ,species_names
    ,percent_target = 0.25
    ,penalty = 100
    ,crt_output_file = TRUE
    ,outdir = getwd()
)
{
    # species names
    id <- unique(puvspr$species)
    # adjust inputs
    if(length(percent_target) == 1)
        percent_target <- rep(percent_target,times = length(id))
    if(length(penalty) == 1)
        penalty <- rep(penalty, times = length(id))
    # set target per species, based on percent_target
    target <- vector()
    for (i in 1:length(id))
        target <- c(target
        ,sum(puvspr$amount[puvspr$species == id[i]]) * percent_target[i]
        )
    # create the spec dataframe object
    spec <- data.frame(
        "id" = id
        ,"target" = target
        #,"status" = deprecated
        ,"name" =  species_names
        ,"spf" = penalty 
    )
    if(crt_output_file)
        write.table(spec, file.path(outdir,"spec.csv")
           ,sep = ",",quote = FALSE, row.names = FALSE
           )
    return(spec)
}
#'#############################################################
#' crt_bound
#'
#' DESCRIPTION
#' create the boundary length file (bound.csv) which contains
#' information on the effective length of shared boundaries
#' between planning units (connectivity). The value of the
#' variable "boundary" is essentially a relative measure of
#' how important it is to include one planning unit in the
#' reserve system, given the inclusion of the other.
#' The function depends on the R package "riverPlanningTools"
#' developed by José Salgado-Rojas (Virgilio's group)
#' pkg link:
#' https://github.com/josesalgr/riverPlanningTools/tree/v0.1
#'
#' PARAMETERS
#' @param df dataframe which column names include the
#' var_name_pu_id, var_name_nextdown, var_name_distance_length
#' variable names
#'
#' @param pu object returned by crt_pu() or the pu file read 
#' as dataframe. Used to validate the ids in bound file
#'
#' @param var_name_pu_id character name in df that identifies
#' the data on the planning unit identification number
#'
#' @param var_name_nextdown character name in df that 
#' identifies the nextdown identification number
#'
#' @param var_name_distance_length character name in df that 
#' identifies the distance (length) between planning units
#' this is, between pu and nextdown_pu
#'
#' @param connection_limit numeric; characterize stream
#' connectivity until a maximum of connection_limit number of 
#' connections in the flow of the river network Default = 10
#'
#' @param distance_from_meters_to_km boolean, whether to
#' convert distance length from meters to kilometers. Makes
#' sense only if distance data is in meters. Default = FALSE
#'
#' @param boundary_sqrt_adjust adjust the magnitude of boundary
#' by dividing its value by its square root. Recommended
#' Default TRUE
#'
#' @param crt_output_file booolean, whether to write the output
#' to a csv file default TRUE
#'
#' @param outdir directory to write the output pu.csv file
#' default = getwd(), currently directory
#'
#' OUTPUT
#' @return pu data frame
#' data format (column ids): id1, id2, boundary
#'
#' @export
crt_bound <- function(
    df
    ,pu 
    ,var_name_pu_id
    ,var_name_nextdown
    ,var_name_distance_length
    ,connection_limit = 10
    ,distance_from_meters_to_km = FALSE
    ,boundary_sqrt_adjust = TRUE
    ,crt_output_file = TRUE
    ,outdir = getwd()
)
{
    x <- riverPlanningTools::boundaryBuilder(
         file = df
        ,connection_limit = connection_limit
        ,id = var_name_pu_id
        ,nxt = var_name_nextdown
        ,len = var_name_distance_length
        )
    # if requested, convert distance from meters to km
    if(distance_from_meters_to_km) 
        x$distance <- x$distance / 1e3
    # adjust variable names
    names(x) <- c("id1", "id2", "boundary")
    # define boundary as 1 / sqrt(distance)
    if(boundary_sqrt_adjust)
        x$boundary <- 1 / sqrt(x$boundary)
    # remove lines with ids not present in pu
    rm <- which(!x$id2 %in% pu$id)
    x <- x[-rm,]
    # create boundary.csv file
    if(crt_output_file)
        write.table(x, file.path(outdir,"bound.csv")
           ,sep = ",",quote = FALSE, row.names = FALSE
           )
    # return bound object
    return(x)
}
#'#############################################################
#' calibrate_blm
#'
#' DESCRIPTION
#' Function to calibrate the blm parameter value in input.dat
#' file. The boundary length modifier (BLM) controls how much
#' emphasis to place on minimising the overall reserve system
#' boundary length relative to the reserve system cost.
#' Higher blm values will produce a more compact reserve 
#' system. However, what is low / high depends on the values
#' of the planning unit costs. The blm value should be in the 
#' same order of magnitude of Maximum Cost Planning Unit.
#' It is strongly recommended to pass to this function
#' the max(cost) / max(boundary ratio) such that the function
#' finds a reasonable value range for the blm calibration
#'
#' PARAMETERS
#' @param marxan_dir directory where the input.dat file is
#' to be found
#'
#' @param cost_bound_ratio numeric value resulting from the
#' ratio max(cost) / max(boundary). Default = 0
#'
#' @param min_x numeric, user defined minimum value of blm to 
#' be explored during calibration. 
#' Omitted if cost_bound_ration > 0
#'
#' @param max_x numeric, user defined maximum value of blm to 
#' be explored during calibration. 
#' Omitted if cost_bound_ration > 0
#'
#' @param increment numeric, increment steps from min_x to
#' max_x to create the full range of blm values to explore
#' during the calibration
#' Omitted if cost_bound_ration > 0
#'
#' OUTPUT
#' @return the blm optimum
#'
#' @export
#'
calibrate_blm <- function(
    marxan_dir = getwd()
    ,min_x = 0
    ,max_x = 15
    ,increment = 0.5
    ,cost_bound_ratio = 0
    )
{
    # since marxan requieres the input.dat file to be in the
    # same working directory of that of the execution call, 
    # the function switches wd just  for the purpose of 
    # running marxan. Then, the original working directory 
    # is restored back to the user current directory
    current_dir <- getwd()
    setwd(marxan_dir)

    # create directory to store calibration output
    outdir <- file.path("calibration")
    if(!dir.exists(outdir))
        dir.create(outdir, recursive = FALSE)
    # set blm test values using cost_bound_ratio
    if(cost_bound_ratio > 0)
    {
        max_x <- next_ten(cost_bound_ratio)
        increment <- max_x / 100
        min_x <- 0    
    }
    # set of blm values to test
    blm <- seq(from = min_x, to = max_x, by = increment)
    # perform calibration
    result <- NULL
    for(value in blm)
    {
        # read input
        input_file <- list.files(pattern = "input.dat"
                                ,full.names = TRUE
                                )
        input <- readLines(input_file, n = -1L)
        # locate BLM parameter and set the new value
        blm_line <- grep(pattern = "BLM", x = input)
        input[blm_line] <- paste("BLM",value,sep = " ")
        # adjust the calibration output directory
        if(!dir.exists(file.path(outdir,value)))
            dir.create(file.path(outdir,value),recursive = F)
        outdir_line <- grep(pattern = "OUTPUTDIR", x = input)
        input[outdir_line] <- paste("OUTPUTDIR"
                                   ,file.path(outdir,value)
                                   ,sep = " "
                                   )
        # overwrite input.dat with new input data
        write(x = input, file = file.path("input.dat"))
        # system invoke OS marxan command
        cmd <- list.files(pattern = "x64")
        if(tolower(Sys.info()[1]) == "linux")
            cmd <- paste("./",cmd, sep ="")      
        # run marxan
        system(command = cmd[1], wait = TRUE, invisible = TRUE)
        # compile calibration results
        summary <- read.csv(file.path(file.path(outdir,value
                                               ,"output_sum.csv"
                                               )
                                     )
                            ,header = TRUE
                            )
        # order by score
        summary <- summary[order(summary$Score),]
        if(is.null(result))
        {
            result <- summary[1,]

        } else {
            result <- rbind(result,summary[1,])
        }
    }
    # attach the blm values explored during calibration
    result$BLM <- blm
    # write calibration result to file
    write.table(result,"blm_calibration.csv", sep = ","
               ,quote = FALSE, row.names = FALSE
               )
    # plot calibration result
    index <- knee(y = result$Cost, x = result$Connectivity)
    blm_opt <- blm[index]
    border_color <- "darkseagreen3"
    plot(y = result$Cost, x = result$Connectivity
        ,las = 0, pch = 21
        ,col = border_color
        ,main = "BLM calibration"
        ,xlab = "connectivity"
        ,ylab = "cost"
        )
    lines(result$Cost[index] ~ result$Connectivity[index], 
	     type = "p", pch = 21, col = border_color, cex = 1.3
         ,bg = "darkred"
         )
	text(x = sum(range(result$Connectivity))/2
        ,y = sum(range(result$Cost)) / 2
	    ,labels = bquote("selected BLM = " ~ .(blm_opt))
	    )
    # restablish original working directory (current_dir)
    setwd(current_dir)
    # result BLM optimum
    return(blm_opt)
}

#'#############################################################
#'		     		GET BEST (MARXAN) SOLUTION			
#' DESCRIPTION 
#' returns the best marxan solution located in
#' marxan_dir/output/output_best.csv
# 
#' PARAMETERS
#' @param marxan_dir marxan directory (where input.dat file is 
#' located)
#'
#' OUTPUT
#' @return (vector) with best marxan solution. A value of 1
#' means that that planning unit was selected as part of the
#' solution by the optimization algorithm. Zero otherwise
#'
#' @export
get_best_solution <- function (marxan_dir = getwd())
{
    datadir <- file.path(marxan_dir,"output")
    marxan_best <- file.path(datadir,"output_best.csv")
    best <- read.table(marxan_best, sep = ",", header = TRUE)
    # return best solution
    return (best$SOLUTION)
}

#'#############################################################
#'		     		RUN MARXAN (FROM R)			
#' DESCRIPTION 
#' run the marxan optimization algorithm from R
# 
#' PARAMETERS
#' @param marxan_dir marxan directory (where input.dat file is 
#' located)
#'
#' OUTPUT
#' None
#'
#' @export
run_marxan <- function (marxan_dir = getwd())
{
    # since marxan requieres the input.dat file to be in the
    # same working directory of that of the execution call, 
    # the function switches wd just  for the purpose of 
    # running marxan. Then, the original working directory 
    # is restored back to the user current directory
    file_exists <- TRUE
    if(!file.exists("input.dat"))
    {
        current_dir <- getwd()
        setwd(marxan_dir)
        file_exists <- FALSE
    }    
    # system invoke OS marxan command
    cmd <- list.files(pattern = "x64")
    if(tolower(Sys.info()[1]) == "linux")
        cmd <- paste("./",cmd, sep ="")      
    # run marxan
    system(command = cmd[1], wait = TRUE, invisible = TRUE)
    # restablish original working directory (current_dir)
    if(!file_exists)
        setwd(current_dir)
}

#'#############################################################
#'		     		NEXT TEN			
#' DESCRIPTION 
#' returns the next ten of the number x
#' If x is a vector v, then x = max(v)
# 
#' PARAMETERS
#' @param x numeric or vector
#'
#' OUTPUT
#' @return the next ten of x (e.g., if x = 41, then return 50)
#'
#' @export
next_ten <- function (x)
{
    # catch exeption: returns unity if the value is less than 1
    if(max(x) < 1)
        return(1)
    y <- floor(max(x)) + seq(0,9,1)
    return (y[y %% 10 == 0])
}
#'#############################################################
#'		     	    KNEE DECISION METHOD		
#'
#' DESCRIPTION
#' Developed according to the Knee method described in
#' Coffey et al (2019)
#' The knee is defined as the intersection point of two lines
#' that minimizes the SSE (sum of squared errors)
#'
#' PARAMETERS
#' @param y vector of length > 2. E.g., in the context of
#' this marxan toolbox package, the cost values
#'
#' @param x vector of the same length as y. E.g., in the 
#' context of this marxan toolbox package, the boundary or
#' connectivity values
#'
#' OUTPUT
#' @return the index where the optimal value is located
#'
#' @export
knee <- function(y
                ,x = seq(from = 1, to = length(y), by = 1)
		        )
{
	# if lenght(y) == 3, there is only one solution
	if(length(y) == 3) { return (2) }
	else
	{
		v <- vector()
		# explore potential knee points
		for(i in 2:(length(y) - 1))
		{
			# adjust linear models to fit a line before and after
			# of the selected knee point
			lm1 <- lm(y[1:i] ~ x[1:i]) # before
			lm2 <- lm(y[i:length(y)] ~ x[i:length(x)]) # after
		
			# get the residuals from each model
			a <- as.vector(unlist(lm1$residuals, use.names = FALSE))
			b <- as.vector(unlist(lm2$residuals, use.names = FALSE))
		
			# SSE of knee point i
			v[i] = sum(a^2) + sum(b^2)
		}
		# return the knee point evaluated on y
		return(which(v == min(v, na.rm = TRUE)))
	}
}