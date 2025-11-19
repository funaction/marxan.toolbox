#'#############################################################
#'		   DRAW SUBSET BY CLICKING		
#'
#' DESCRIPTION
#' function to interactively select a subset of polygons.
#' Returns a spatial vector made out of the
#' selected polygon/s. Reference layers can be optionally 
#' provided.
#'
#' PARAMETERS
#' @param x (spatial vector) target spatvector layer from which
#' to extract a subset by clicking on a plot.
#' Required
#'
#' @param polygons_ids (character) variable name in x that 
#' identifies its polygons.
#' Required
#'
#' @param map (spatvector) reference layer: country borders
#' Default NULL
#'
#' @param sites (spatvector) reference layer: sampling locations
#' Default = NULL
#'
#' @param rivers (spatvector) reference layer: river network
#' Default = NULL
#'
#' @param lakes (spatvector) reference layer: lakes
#' Default = NULL
#'
#' @param outfile (Char) filename where to write the subset
#' output (make sure that the file name has the appropriate 
#' extension, e.g., *.gpkg)
#' Default = FALSE
#'
#' @param aggregate_polygons (boolean) if TRUE, merge polygons 
#' that share borders
#' Default = TRUE
#' 
#' OUTPUT
#' @return spatvector subset
#'
#' @export
draw_subset_by_clicking <- function(
        x,
        polygons_ids,
        map = NULL, 
        sites = NULL, 
        rivers = NULL,
        lakes = NULL,
        outfile = FALSE,
        aggregate_polygons = TRUE
        )
{
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

        add = FALSE        
        if(!is.null(map)){
                terra::plot(
                        map, 
                        border = mapborder, 
                        col = mapcolor, 
                        lwd = 2,
                )
                add = TRUE
        }
                
        terra::plot(
                x, 
                border = basinborder, 
                col = basincolor,
                main = "select by clicking",
                add = add
        )
        
        
        if(!is.null(rivers))
                terra::plot(rivers, col = rivercolor, add = TRUE)
        if(!is.null(lakes))
                terra::plot(
                        lakes, 
                        border = lakeborder, 
                        col = scales::alpha(
                                colour = lakecolor, 
                                alpha = 0.5
                        ), 
                        add = TRUE
                )
        if(!is.null(sites))
                terra::plot(sites, pch = 21, cex = 1, bg = sitecolor, add = TRUE)
        
        # interactive procedure
        output <- draw_by_clicking(
                layer = x, 
                IDs = polygons_ids,
                aggregate_polygons = aggregate_polygons,
                outfile = outfile
        )
        
        # close plotting device
        Sys.sleep(3) # 3sec delay before proceeding
        dev.off()

        message("job successful")
        # return selected basin
        return(output)
}



#'#############################################################
#'		DELINEATE BASIN	DEPRECATED	
#'
#' DESCRIPTION
#' function to interactively delineate a basin or modeling
#' region of interest. Returns a spatial vector made out of the
#' selected polygon/s. If a country is not provided, then a 
#' valid extent or reference mask is required.
#'
#' PARAMETERS
#' @param reference_layer (spatial vector) Reference layer
#' on which to draw by clicking the subset layer area of interest.
#' If not provided, a hydroshed basin of level lev10 is used
#' Default = NULL
#'
#' @param polygons_ids (character) variable name 
#' identifier that contains the polygons IDs in reference_layer.
#' If not provided, it takes the value used by hydrosheds to
#' identify basin polygons
#' Default "HYBAS_ID"
#'
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
#' vector with a single polygon describing the area of
#' interest is returned
#' Default = TRUE
#' 
#' OUTPUT
#' @return SpatVector of basin area of interest
#'
#' DEPRECATED
delineate_basin <- function(
        reference_layer = NULL,
        polygons_ids = "HYBAS_ID",
        country = NULL,
        extent = NULL, 
        mask = NULL, 
        sites = NULL, 
        rivers = NULL,
        lakes = NULL,
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
         
        if(!is.null(extent) && !is.logical(extent))
                map <- terra::crop(map, extent)
        if(!is.null(country) && (is.null(extent) || is.logical(extent)))
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

        # If requested, ie., extent = TRUE, enable the user to select the 
        # extent of interest directly from the map with the sampling sites
        if(!is.null(extent) && is.logical(extent) && extent == TRUE){
                terra::plot(
                        map, 
                        main = "select extent of interest (two clicks)"
                )
                sitecolor <- "orange"
                terra::plot(
                        sites, 
                        pch = 21, 
                        cex = 1, 
                        bg = sitecolor, 
                        add = TRUE
                )
                extent = terra::draw(
                        x = "extent", 
                        col = "darkseagreen4", 
                        lwd = 2, 
                        id = FALSE, # show numeric id on map?
                        n = 2, # number of clicks
                        xpd = FALSE # drawing outside plot area forbidden
                )
                map <- terra::crop(map, extent)
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
                
        if(is.null(reference_layer)){
                basinfile <- file.path(download_dir,"basinEU.gpkg")
                if(file.exists(basinfile))
                {
                        reference_layer <- terra::vect(basinfile)
                } else {
                        reference_layer <- terra::vect(
                                get_gdrive_resource(
                                        marxan.toolbox:::basinlink, 
                                        download_dir
                                )
                        )
                }
        }       
        reference_layer <- terra::crop(reference_layer, map)
        
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
        
        terra::plot(map, border = mapborder, col = mapcolor, lwd = 2)
        terra::plot(
                reference_layer, 
                border = basinborder, 
                col = basincolor, 
                add = TRUE
        )
        terra::plot(rivers, col = rivercolor, add = TRUE)
        terra::plot(
                lakes,
                main = "selection of study region", 
                border = lakeborder, 
                col = scales::alpha(
                        colour = lakecolor, 
                        alpha = 0.5
                ), 
                add = TRUE
        )
        terra::plot(sites, pch = 21, cex = 1, bg = sitecolor, add = TRUE)
        
        # interactive procedure
        output <- draw_by_clicking(
                layer = reference_layer, 
                IDs = polygons_ids
        )
        
        # close plotting device
        Sys.sleep(3) # 3sec delay before proceeding
        dev.off()

        message("job successful")
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


#'#############################################################
#'		     	    DRAW BY CLICKING		
#'
#' DESCRIPTION
#' (internal) draw selection by clicking on plotted reference
#' layer. Return the selected subset vector
#'
#' PARAMETERS
#' @param layer (spatial vector) Reference layer
#' on which to draw by clicking the subset layer area of interest.
#' Required
#'
#' @param IDs (character) variable name 
#' identifier that contains the polygons IDs in the reference 
#' layer.
#' Default Required
#'
#' @param aggregate_polygons (boolean) if TRUE, a spatial 
#' vector with a single polygon describing the area of
#' interest is returned
#' Default = TRUE
#'
#' @param outfile (Char) filename where to write the 
#' output delineated basin as SpatVector (make sure that
#' filename has the appropriate extension, e.g., *.gpkg)
#' Default = FALSE
#'
#' @param selectioncolor color value to highlight selection
#' 
#' OUTPUT
#' @return (spatvector) selected subset
#'
draw_by_clicking <- function(
        layer, 
        IDs,
        aggregate_polygons = TRUE,
        outfile = FALSE,
        selectioncolor = "darkseagreen4"
){
        i <- 1 # loop control
        cancel <- FALSE
        while(i < 100 && cancel == FALSE){
                
                selection <- terra::click(
                        x = layer, 
                        n = 1,
                        type = "n", 
                        show = FALSE
                )
                if(i <= 1){
                        if(!is.null(selection))
                                id <- selection[[IDs]]
                } else {
                        if(!is.null(selection))
                                id <- c(id, selection[[IDs]])
                }

                # update plot with selected regions
                id_to_plot <- which(
                        as.vector(unlist(layer[[IDs]])) %in% tail(id, n = 1)
                )
                if(!is.null(selection))
                        terra::plot(
                                layer[id_to_plot],
                                col = scales::alpha(
                                        colour = selectioncolor, 
                                        alpha = 0.5),
                                add = TRUE
                        )                        

                i <- i + 1 # update loop control
                if(is.null(selection))
                        cancel <- TRUE
        }

        # prepare output data and write outfile, if requested
        selected_ids <- which(as.vector(unlist(layer[[IDs]])) %in% id)
        output <- layer[selected_ids]
        if(aggregate_polygons)
                 output <- terra::aggregate(output)

        # write outfile
        if(outfile != FALSE)
                terra::writeVector(
                        x = output, 
                        filename = outfile, 
                        overwrite = TRUE
                )
        # return selected layer subset
        return(output)
}