#'#############################################################
#'		     	    EXTRACT HYDROLINKED DATA		
#'
#' DESCRIPTION
#' method to extract data based on the hydrological connection.
#' The function makes use of the polygons ids stored in the
#' upstream variable (obtained after applying 
#' marxan.toolbox::get_rivnet_SC) to build the upstream
#' connectivity. By default, cummulative predictor values
#' are extracted (i.e., fun = "sum")
#'
#' PARAMETERS
#' @param x (SpatVector) as given by 
#' marxan.toolbox::get_rivnet_SC. If another SpatVector of
#' polygons is used, make sure that it has its hydrological 
#' connectivity stored in a variable called "upstream" 
#' REQUIRED
#'
#' @param predictor_layer (SpatRaster or SpatRaster stack)
#' from which to extract the data 
#' REQUIRED
#'
#' @param fun (character) function to summarize the extracted
#' data
#' DEFAULT "sum"
#'
#' @param ... further arguments passed to terra::extract
#' 
#' OUTPUT
#' @return River object. Information details in Rivnet package
#' documentation
#'
#' @export
extract_hydrolinked_data <- function(
    x,
    predictor_raster,
    fun = "sum",
    ...
){
    # extract raster values by applying fun
    # do not consider missing data in the calculations and bind
    # output to the original SpatVector
    y <- terra::extract(
        x = predictor_layer, 
        y = x,
        fun = fun,
        ID = FALSE,
        na.rm = TRUE,
        bind = TRUE,
        ...
    )

    # using parse() and eval() methods to extract the numeric id vectors
    # stored as string in upstream variable
    varnames <- names(predictor_layer)
    result <- y
    for(i in 1:nrow(y)){
        polygon_id <- eval(parse(text = y$upstream[i]))
        
        result[i, names(result) %in% varnames] <- 
            sapply(
                X = y[y$SCID %in% polygon_id, names(y) %in% varnames], 
                FUN = fun
            )
    }
    return(result)
}
