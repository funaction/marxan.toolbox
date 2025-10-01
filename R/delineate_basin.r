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
#' @param outfile (Char) filename where to write the 
#' output delineated basin as SpatVector (make sure that
#' filename has the appropriate extension, e.g., *.gpkg)
#' Default = FALSE
#'
#' @param download_dir (Char) path directory where to store
#' downloaded resources. This will be used to get missing
#' GIS data layers from a repository
#' Default = a temporary directory
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
        download_dir = tempdir(),
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
                maskfile <- file.path(download_dir,"westEU.gpkg")
                if(file.exists(maskfile))
                {
                        map <- terra::vect(maskfile)
                } else {
                        map <- terra::vect(
                                get_gdrive_resource(
                                        marxan.toolbox:::masklink,
                                        download_dir
                                )
                        )
                }
        } else {
                map <- mask
        }
         
        if(!is.null(extent))
                map <- terra::crop(map, extent)
        if(!is.null(country) && is.null(mask) && is.null(extent))
                map <- map[tolower(map$name) %in% tolower(country)]

        if(is.null(sites)){
                sitesfile <- file.path(download_dir,"funaction_sites.gpkg")
                if(file.exists(sitesfile))
                {
                        sites <- terra::vect(sitesfile)
                } else {
                        sites <- terra::vect(
                                get_gdrive_resource(
                                        marxan.toolbox:::siteslink, 
                                        download_dir
                                )
                        )
                }
                
        }
                
        sites <- terra::crop(sites, map)

        if(is.null(rivers)){
                riversfile <- file.path(download_dir,"riversEU.gpkg")
                if(file.exists(riversfile)){
                        rivers <- terra::vect(riversfile)
                } else {
                        rivers <- terra::vect(
                                get_gdrive_resource(
                                        marxan.toolbox:::riverslink, 
                                        download_dir
                                )
                        )
                }
        }
                
        rivers <- terra::crop(rivers, map)

        if(is.null(lakes)){
                lakesfile <- file.path(download_dir,"lakesEU.gpkg")
                if(file.exists(lakesfile)){
                        lakes <- terra::vect(lakesfile)
                } else {
                        lakes <- terra::vect(
                                get_gdrive_resource(
                                        marxan.toolbox:::lakeslink, 
                                        download_dir
                                )
                        )
                }
        }        
        lakes <- terra::crop(lakes, map)
                
        if(is.null(basin)){
                basinfile <- file.path(download_dir,"basinEU.gpkg")
                if(file.exists(basinfile))
                {
                        basin <- terra::vect(basinfile)
                } else {
                        basin <- terra::vect(
                                get_gdrive_resource(
                                        marxan.toolbox:::basinlink, 
                                        download_dir
                                )
                        )
                }
        }       
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
        while(i < 50 && cancel == FALSE){
                
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


#'#############################################################
#'		     	    GET GDRIVE RESOURCE		
#'
#' DESCRIPTION
#' (internal) download google drive file by id and returns the
#' unzipped filename
#'
#' PARAMETERS
#' @param id (Char) google drive file identification
#' REQUIRED
#'
#' @param temp_dir (Char) path directory where to store 
#' downloaded resources
#' Default = FALSE i.e., a temporary directory
#' 
#' OUTPUT
#' @return downdloaded filename
#'
get_gdrive_resource <- function(id, temp_dir = FALSE){

        # create temporary file name and directory
        temp_file <- tempfile(fileext = ".zip")
        if(temp_dir == FALSE)
                temp_dir <- tempdir() 
        
        # download requested drive resource id
        googledrive::drive_deauth()
        gfile <- googledrive::drive_get(googledrive::as_id(id))
        googledrive::drive_download(
                file = gfile, 
                path = temp_file,
                overwrite = TRUE
        )

        # unzip file
        fname <- unzip(
                zipfile = temp_file, 
                exdir = temp_dir
        )

        # return downloaded file
        return( fname )

}
