#'#############################################################
#'		     	    DELINEATE BASIN		
#'
#' DESCRIPTION
#' function to interactively delineate a basin. Returns a 
#' spatial vector with polygon/s representing the basin area of 
#' interest. If a country is not provided, then a valid extent
#' or reference mask is required.
#'
#' PARAMETERS
#' @param country (Char) country name of watershed location
#' Default = NULL
#'
#' @param extent (vector) extent of interest with format:
#' (xmin, xmax, ymin, ymax). If an extent is provided, country
#' will be omitted
#' Default = NULL
#'
#' @param mask (raster / spatial vector) reference map layer 
#' depicting the watershed
#' Default = NULL
#'
#' @param sites (spatial vector) of sampling sites. This
#' optional item can help with the delineation of the basin
#' area of interest
#' Default = FALSE
#'
#' @param rivers (spatial vector) custom river network to use
#' instead of the hydrosheds rivers used by the package
#' Default = FALSE
#'
#' @param lakes (spatial vector) custom lakes polygons to use
#' instead of the hydrosheds lakes used by the package
#' Default = FALSE
#'
#' @param basin (spatial vector) hydroshed basin of level other
#' than lev10 which is the one used by the package
#' Default = FALSE
#'
#' @param outfile (character) filename where to write the 
#' output delineated basin as SpatVector (make sure that
#' filename has the appropriate extension, e.g., *.gpkg)
#' Default = FALSE
#'
#' @param aggregate_polygons (boolean) if TRUE, a spatial 
#' vector with a single polygon describing the basin area of
#' interest is produced
#' Default = TRUE
#' 
#' OUTPUT
#' @return SpatVector of basin area of interest
#'
#' @export
delineate_basin <- function(
        country = NULL,
        extent = NULL, 
        mask = NULL, 
        sites = NULL, 
        rivers = NULL,
        lakes = NULL,
        basin = NULL,
        outfile = FALSE,
        aggregate_polygons = TRUE
        )
{
        # catch exception: undefined country or area of interest
        if(is.null(country) & is.null(extent) & is.null(mask))
                return(print(paste(
                        "no job to be done:", 
                        "undefined country/area of interest",
                        sep = " "
                        )
                ))

        # data preparation: involves clipping/cropping to the region
        # of interest
        message("preparing data ...")
        if(is.null(mask)){
                map <- marxan.toolbox:::mask
        } else {
                map <- mask
        }
         
        if(!is.null(extent))
                map <- terra::crop(map, extent)
        if(!is.null(country) && is.null(mask) && is.null(extent))
                map <- map[tolower(map$name) %in% tolower(country)]

        if(is.null(sites))
                sites <- marxan.toolbox:::sites
        sites <- terra::crop(sites, map)

        if(is.null(rivers))
                rivers <- marxan.toolbox:::rivers
        rivers <- terra::crop(rivers, map)

        if(is.null(lakes))
                lakes <- marxan.toolbox:::lakes
        lakes <- terra::crop(lakes, map)
                
        if(is.null(basin))
                basin <- marxan.toolbox:::basin
        basin <- terra::crop(basin, map)
        
        message("done")

        # draw map
        message("display interactive map: waiting for user inputs ...")
        
        mapborder <- "black"
        mapcolor <- "white"
        basinborder <- "black"
        basincolor <- "white"
        rivercolor <- "lightgray"#"deepskyblue3"
        lakeborder <- "darkgray"
        lakecolor <- "lightgray"
        sitecolor <- "orange"
        selectioncolor <- "darkseagreen4"
        
        terra::plot(map, border = mapborder, col = mapcolor, lwd = 2)
        terra::plot(basin, border = basinborder, col = basincolor, add = TRUE)
        terra::plot(rivers, col = rivercolor, add = TRUE)
        terra::plot(
                lakes, 
                border = lakeborder, 
                col = scales::alpha(
                        colour = lakecolor, 
                        alpha = 0.5
                ), 
                add = TRUE
        )
        terra::plot(sites, pch = 21, cex = 1, bg = sitecolor, add = TRUE)
        
        # interactive procedure
        i <- 1 # loop control
        cancel <- FALSE
        while(i < 20 && cancel == FALSE){
                
                selection <- terra::click(
                        x = basin, 
                        n = 1,
                        type = "n", 
                        show = FALSE
                )
                if(i <= 1){
                        if(!is.null(selection))
                                id <- selection$HYBAS_ID
                } else {
                        if(!is.null(selection))
                                id <- c(id, selection$HYBAS_ID)
                }

                # update plot with selected regions
                terra::plot(
                        basin[basin$HYBAS_ID %in% tail(id, n = 1)],
                        col = scales::alpha(
                                colour = selectioncolor, 
                                alpha = 0.5),
                        add = TRUE
                )                        

                i <- i + 1 # update loop control
                if(is.null(selection))
                        cancel <- TRUE
        }
        # close plotting device
        dev.off()

        # prepare output data and write outfile, if requested
        output <- basin[basin$HYBAS_ID %in% id]
        if(aggregate_polygons)
                 output <- terra::aggregate(output)

        # write outfile
        if(outfile != FALSE)
                terra::writeVector(
                        x = output, 
                        filename = outfile, 
                        overwrite = TRUE
                )

        message("job sucessful")
        # return selected basin
        return(output)
}
