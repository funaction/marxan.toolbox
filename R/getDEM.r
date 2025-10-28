#'#############################################################
#'		     	    GET DIGITAL ELEVATION MODEL		
#'
#' DESCRIPTION
#' function to obtain a digital elevation model or DEM from
#' the Amazon Web Services Terrain Tiles and the Open Topography 
#' global datasets API,
#' by using the method get_elev_raster of package elevatr.
#'
#' PARAMETERS
#' @param raster_template (spatraster) spatraster object that
#' contains the region of interest from which to obtain the DEM
#' REQUIRED
#'
#' @param zoom_level (ranges from 1 to 14) affects the
#' resolution of the DEM. More details at
#' <https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution>
#'
#' @param country_map (spatvector) optional. To display country
#' borders when manually selecting the extent of interest
#' Default = NULL
#'
#' @param sites (spatvector) optional. To display point
#' locations/sites of interest to use as a reference when drawing
#' the extent of interest
#' DEFAULT NULL
#'
#' @param outfile if not FALSE, write the DEM to this output file
#' DEFAULT = FALSE 
#'
#' @param draw_extent (boolean) whether to define the extent of
#' interest by drawing it directly on the map. The default value
#' is set to TRUE, because depending on the resolution the
#' resulting DEM could require a large disk space
#' DEFAULT = TRUE
#'
#' @param ... further arguments passed to 
#' elevatr::get_elev_raster
#' 
#' OUTPUT
#' @return spatraster object (DEM)
#'
#' @export
getDEM <- function(
    raster_template,
    zoom_level = 11,
    country_map = NULL,
    sites = NULL,
    outfile = FALSE,
    draw_extent = TRUE,
    ...
){

    if(draw_extent){
        message("waiting usr to draw extent")
        terra::plot(
            raster_template,
            main = "draw extent (two clicks)"
        )
        if(!is.null(country_map))
            terra::plot(country_map, lwd = 1.5, add = T)
        if(!is.null(sites))
            terra::plot(
                sites, 
                bg = "orange",
                pch = 21, 
                cex = 1.2, 
                add = T
            )
        extent = terra::draw(
            x = "extent", 
            col = "orange", 
            lwd = 2, 
            id = FALSE, # show numeric id on map?
            n = 2, # number of clicks
            xpd = FALSE # drawing outside plot area forbidden
        )
        raster_template <- terra::crop(raster_template, extent)
        Sys.sleep(3) # 3sec delay before proceeding
        message("done")
        dev.off()     
    }

    # get DEM
    message("obtaining DEM ...")
    dem <- elevatr::get_elev_raster(
	    locations = raster_template,
	    z = zoom_level,
	    ...
    )
    message("done")

    # convert to spatraster
    dem <- terra::rast(dem)
    info_crs <- terra::crs(dem, describe = T)[,1]
    if(is.na(info_crs) || 
    grepl("unknown|undefined|na", tolower(info_crs))){
        terra::crs(dem) <- terra::crs(raster_template)
    }
    
    # adjust names
    names(dem) <- "altitude"

    # write dem to output file
    if(outfile != FALSE)
        terra::writeRaster(
            x = dem, 
            filename = outfile, 
            overwrite = TRUE
        )

    # return DEM
    return(dem)
}
