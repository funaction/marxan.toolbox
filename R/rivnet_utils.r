#'#############################################################
#'		     	    RIVNET EXTRACT RIVER		
#'
#' DESCRIPTION
#' function to extract the river network associated to the
#' provided DEM and outlet(s), using the "extract_river"
#' method of package "rivnet"
#'
#' PARAMETERS
#' @param dem_fname (character) file name of the digital 
#' elevation model DEM of the catchment of interest. The DEM 
#' should embrace all rivers upstream of the selected outlet. 
#' If streams are cut by the DEM, the method can fail to 
#' delineate the river network. In addition, the DEM projection
#' should be in meters (e.g., the european epsg:3035) 
#' REQUIRED
#'
#' @param outlet (vector, matrix, or data frame) if vector of
#' length 1, the user will be asked to interactively
#' draw the outlet(s) on the DEM map. If vector length > 1, 
#' the odd components identify the longitudinal (x) 
#' coordinates, and the even components the latitudinal (y) 
#' coordinates. If a matrix, it should have 2 columns 
#' (for x and y coordinates respectively) and number of rows 
#' equal to the number of outlets. If a data frame, it should 
#' have components 'outlet$x', 'outlet$y' identifying the 
#' respective coordinates.
#' Default = 1
#'
#' @param locations (spatvector) optional. To display point
#' locations of interest to use as a reference when drawing
#' the outlet(s)
#' DEFAULT NULL
#'
#' @param country_map (spatvector) optional. Display country
#' borders when manually selecting the outlets
#' DEFAULT NULL
#'
#' @param reference_river (spatVector) optional. To help
#' manually setting the outlets using a reference river layer
#'
#' @param crt_outlets_log (boolean) whether to write the selected
#' outlets to a file
#' DEFAULT FALSE
#'
#' @param ... further arguments passed to 
#' rivnet::extract_river
#' 
#' OUTPUT
#' @return River object. Information details in Rivnet package
#' documentation
#'
#' @export
rivnet_extract_river <- function(
    dem_fname,
    outlet = 1,
    locations = NULL,
    country_map = NULL,
    reference_river = NULL,
    crt_outlets_log = FALSE,
    ...
){
    if(is.vector(outlet) && length(outlet) == 1)
        outlet <- select_outlets_from_map(
            n_outlets = outlet,
            dem = terra::rast(dem_fname),
            sites = locations,
            country_map = country_map,
            reference_river = reference_river
        )
    # extract the river network
    rivnet_result <- rivnet::extract_river(
        outlet = outlet,
        DEM = dem_fname,
        ...
    )
    # create outlets log file
    if(crt_outlets_log){
        outlet <- matrix(unlist(outlet), ncol = 2)
        outlet <- data.frame(x = outlet[,1], y = outlet[,2])
        write.table(
            x = outlet,
            file = "outlets_log.csv",
            sep = ",",
            row.names = FALSE,
            quote = FALSE
        )
    }
    # return river object
    return(rivnet_result)
}


#'#############################################################
#'	    GET RIVER NETWORK AND SUBCATCHMENTS AS SPATVECTORS 		
#'
#' DESCRIPTION
#' function to obtain the river network and its subcatchment
#' polygons as spatvectors from a river object (as returned
#' by marxan.toolbox::rivnet_extract_river) 
#' 
#'
#' PARAMETERS
#' @param river (river object) as given by 
#' marxan.toolbox::rivnet_extract_river
#' REQUIRED
#'
#' @param SC_red (positive integer) scaling factor affecting
#' the resolution or area size of the subcatchment polygons
#' Default = 20
#'
#' @param thrA_factor (numeric double) scaling factor to
#' thrA parameter in OCNet::aggreate_OCN: threshold value on 
#' drainage area used to derive the aggregated network. 
#' If thrA = 0, no aggregation is performed
#' DEFAULT = 0.002
#'
#' @param crs (character) coordinate reference system used
#' when creating the SC layer. It is recommended to input the
#' same crs used for the extraction of the river network
#'
#' @param outdir (character) output directory where to store
#' the river and SC shape files
#' DEFAULT CURRENT WORKING DIRECTORY
#'
#' @param tag (character) custom tag to add to the ouput river 
#' and SC shape files. Eg., catchment id, country, ... 
#' DEFAULT "" (empty character)
#'
#' @param collection (boolean) if TRUE returns the subcatchments
#' SC and rivernetwork as a spatvector collection. Otherwise,
#' returns the SC spatvector object only
#' DEFAULT FALSE
#' 
#' OUTPUT
#' @return River object. Information details in Rivnet package
#' documentation
#'
#' @export
get_rivnet_SC <- function(
    river,
    SC_res = 20,
    thrA_factor = 0.002,
    crs = "epsg:3035",
    outdir = ".",
    tag = "",
    collection = FALSE
){

    # 1. aggregate river object:
    # more details at ?OCNet::aggregate_OCN()
    # if mytrheshold / thrA = 0, no aggregation is performed
    #f <- 0.002 # scaling factor. Default f = 0.002
    mythreshold <- thrA_factor * river$FD$nNodes * river$cellsize^2
    
    # maxReachLength: default Inf. Values lower than 
    # OCN$cellsize*sqrt(2) are not allowed. 
    # If maxReachLength < 2*OCN$cellsize, every RN node is also 
    # an AG node.
    # maxReachLength is used as boundary condition to partition
    # river segments (e.g., if a river segment is longer than
    # maxReachLength, it will split the segment into two or more
    # smaller segments. This influece the resulting resolution of
    # the subcathment polygons)
    SC_res <- 20 # scaling factor to the resolution or size area
    # of SC polygons
    max_reach_length <- SC_res * river$cellsize * sqrt(2)

    message("building river network")
    # build the network
    rivernetwork <- rivnet::aggregate_river(
        river = river,
        thrA = mythreshold,
        streamOrderType = "Strahler", # alternatively, "Shreve"
        maxReachLength = max_reach_length,
        equalizeLengths = TRUE, # def FALSE
        breakpoints = NULL,
        displayUpdates = FALSE 
    )
    # write river network to shapefile
    fname = file.path(outdir,paste0(tag,"rivernetwork.gpkg"))
    rivnet::river_to_shapefile(
        river = rivernetwork, 
        filename = fname,
        atts = names(rivernetwork$AG),
        overwrite = TRUE
    )
    # read rivetnetwork as spatvector
    r <- terra::vect(fname)
    message("done")
    message("creating SC spatvector")
    # create subcatchment spatraster values
    df <- data.frame(
        rivernetwork$FD$X, 
        rivernetwork$FD$Y, 
        rivernetwork$FD$toSC
    )
    names(df) <- c("x","y","value")

    sc_rast <- terra::rast(
        x = df, 
        type = "xyz", 
        crs = crs, 
        digits = 6, 
        extent = NULL
    )

    # convert from spatraster to spatvector
    sc <- terra::as.polygons(
        x = sc_rast,
        trunc = TRUE,  # truncate values to integer
        dissolve = TRUE,  # combine cells with same values
        values = TRUE,
        extent = FALSE
    )
    # add nextdown and other river attributes to SC layer
    sc <- cbind(sc, r[,2:ncol(r)])

    # adjust names
    # subcatchment id value as "SCID"
    names(sc)[1] <- "SCID"
    
    # write SC Spatvector to file
    fname <- file.path(outdir,paste0(tag,"SC.gpkg"))
    terra::writeVector(
        x = sc, 
        filename = fname, 
        overwrite = TRUE
    )
    # if CRS for rivernetwork r is unknown, then set them to
    # the SC CRS and overwrite the river shapefile
    info_crs <- terra::crs(r, describe = T)[,1]
    if(is.na(info_crs) || 
    grepl("unknown|undefined|na", tolower(info_crs))){

        terra::crs(r) <- terra::crs(sc)
        fname = file.path(outdir,paste0(tag,"rivernetwork.gpkg"))
        terra::writeVector(r, fname, overwrite = TRUE)
    }

    message("done")
    # return spatvector object(s)
    if(collection)
        return(c(sc,r))
    return(sc)
}


#'#############################################################
#'		     	    SELECT OUTLETS FROM MAP		
#'
#' DESCRIPTION
#' function to select n outlets by drawing points interactively
#' on a given DEM
#'
#' PARAMETERS
#' @param n_outlets (numeric) requested outlet points to draw
#' Default = 1
#'
#' @param dem (spatraster) DEM layer to draw the outlets on
#' REQUIRED
#'
#' @param sites (spatvector) reference locations (e.g., 
#' sampling sites)
#' DEFAULT NULL
#'
#' @param country_map (spatvector) reference map with country
#' borders
#' DEFAULT NULL
#'
#' @param reference_river (spatvector) reference river layer
#' DEFAULT NULL
#' 
#' OUTPUT
#' @return n outlets with geographic coordinates
#'
select_outlets_from_map <- function(
    n_outlets = 1,
    dem,
    sites = NULL,
    country_map = NULL,
    reference_river = NULL
){
    message("waiting for usr input on map")
    # plot the dem map
    terra::plot(dem, main = "click on desired outlet(s)")
    if(!is.null(sites))
        terra::plot(
            sites, 
            bg = "orange",
            pch = 21, 
            cex = 1.2,
            add = TRUE
        )
    if(!is.null(reference_river))
        terra::plot(
            reference_river,
            col = "white",
            add = TRUE
        )
    # select n outlets by drawing points
    outlet <- terra::click(
        dem, 
        xy = TRUE,      # return coordiantes
        type = "p",     # draw points
        col = "gray", 
        n = n_outlets
    )
    message("done")
    Sys.sleep(3) # 3sec delay before proceeding
    dev.off()
    # keep coordinates only (i.e., columns 1 and 2)
    outlet <- outlet[,1:2]
    # return outlet coordinates
    return(outlet)
}