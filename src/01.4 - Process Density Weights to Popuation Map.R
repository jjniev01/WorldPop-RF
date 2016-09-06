#####
## Purpose of script: Project final population map (popmap) to 2010 and
##    2015 estimates based on the 2011 Revision of World Urbanization
##		Prospects (http://esa.un.org/unpd/wup/index.htm) and also adjusted
##		to U.N. total population numbers for each country.

##	Population/census data shapefile should be updated with columns
##		'ADMINID', 'ADMINPOP' and 'ISO'
##	(NOTE: takes place of admin.shp in Benin example)

##	GRUMP shapefile is set below, e.g. af_as_lac_urban.shp, a subset of
##		GRUMP world shapefile with columns 'ISOURBID' and 'POP' used in code
##		(already in shapefile)

##	Urban and rural growth rates to adjust to 2010 and also 2015
##		(growth rates excel file)

##	Other notes:
##		Check in initial population census shapefile for a RECNO and POP fields - if there are, delete
##	Geometry of census shp should be checked. if issues, topology may need to be corrected/adjusted (ex. VNM)
#####

#####
##  BEGIN FUNCTION DEFINITIONS
##  Assume GR_years has already been defined
##  Define the function that will calculate the GR for urb and rur:
require(dplyr)
require(plyr)

GR_calculator <- function(dat = grdf,
                          iso = NULL, 
                          target_years = c(), 
                          urb = FALSE, orig_year = NULL){
  ##  Subset the data to the country in question:
  dat <- dat %>% filter(ISO == iso)
  
  ##  Determine if we are calculating for urban GRs:
  if(urb == FALSE){
    type <- "RUR"
  }else{type <- "URB"}
  
  ##  From the data frame, get all the urban or rural AGRs to be used 
  ##  in calculations:
  ##    Create two offset index sequencers to put together the growth
  ##      rates to be extracted:
  ##    Upper year set:
  uyset <- c(1995,2000,2005,2010,2015,2020,2025,2030)
  ##    Lower year set:
  lyset <- c(1990,1995,2000,2005,2010,2015,2020,2025)
  
  ##  Create a vector of the columns to retrive the type specific 
  ##    growth rate values from:
  grcols <- paste0(lyset,"_",uyset,".",type,"AGR")
  
  ##  Retrieve the growth rate values as a vector:
  agr <- unlist(dat[,grcols], use.names = FALSE)
  
  ##  Divide those values by 100 to put them in decimal percent:
  agr <- agr/100
  
  ##  Create an empty vector to store the growth rates calculated 
  ##  which will be returned at the end of the function:
  gr_vec <-c()
  
  ##  For every year in the target years:
  for(ty in target_years){
    ##  Get nyears to apply AGRs for all time steps:
    ##    Get the magnitude year difference based upon whether or not
    ##    census year is less than the target year:
    ##      If census year < target year, then cyty <- TRUE
    if(orig_year < ty){
      cyty <- TRUE
    }else{cyty <- FALSE}
    
    ##      Calculate a vector of magnitude of all the year differences
    if(cyty){
      nyears <- abs(c(1995,2000,2005,2010,2015,2020,2025,2030) - orig_year)
    }else{
      nyears <- abs(orig_year - c(1990,1995,2000,2005,2010,2015,2020,2025))
    }
    ##      Number of years is the minimum of the following:
    nyears <- pmin(5, nyears)
    
    ##  Determine for each time step if the rate will be applied 
    ##  "forward" or "backward":
    ##    First logic gate: 
    gate1 <- ((ty < orig_year) & (ty < uyset) & (orig_year > lyset))
    ##    Second logic gate:
    gate2 <- ((ty > orig_year) & (ty > lyset) & (orig_year < uyset))
    ##  Determine forward (1) backward(-1) or none(0):
    fbn <- c()
    for(g in 1:length(gate1)){
      if(gate1[g]){
        fbn <-c(fbn,-1)
      }else{
        ifelse(gate2[g], fbn <- c(fbn, 1),fbn <- c(fbn,0))
      }
    }
    
    ##  Calculate the GR:
    gr <- prod(exp(agr * nyears) ** fbn)
    ##  Add the new GR to the list of GRs:
    gr_vec <- c(gr_vec, gr)  
  }
  ##  Return the GR vector:
  return(gr_vec)
}


##  Create a function to get the population values for the UN Adjust 
##    for all target years:
popRetriever <- function(dat = grdf,iso = NULL, target_years = c()){
  ##  Create an empty vector to store the population values to 
  ##    be returned:
  pops <- c()
  
  ##  Subset the data based upon the country name:
  dat <- dat %>% filter(ISO == iso)
  
  ##  For every year in target_years:
  for(y in target_years){
    ##  Define the pop-year column to retrieve from:
    f <- paste0("POP_",y)
    
    ##  Retrieve the value for that year and the country and store it:
    pops <-c(pops, dat[,f])
  }
  ##  Return the population values multiplied by 1000
  pops <- pops *1000
  return(pops)
}

##  END:  FUNCTION DEFINITIONS
#####


#####
##	BEGIN:	Set per-country configuration options

##	Parse main configuration file, which will set the country and root_path
##		variables:
source(paste0(root_path,"/src/01.0 - Configuration.py.r"))

##  Load the Growth Rate data frame that is located in the source folder; 
##    it gets loaded under the variabel name grdf and is necessary for 
##    the growth rate calculator and the UN Adjustment population value 
##    retrieval:
load(paste0(root_path,"/src/GrowthRateDF_2014.RData"))

##	Round to whole population counts?
round_counts <- FALSE


##	Integrate Urban dataset:
##		NOTE: If you set this to True then map production will use the
##			Raster representation of whatever you've used as the Urban data
##			layer and "burn" the urban class into the land cover before
##			adjusting population counts per pixel:
use_urban <- TRUE

##  Declare the year of the census data being used:
censusdat_year <- 2002

##  Declare the target years to tbe modeled:
GR_years <- c(2000, 2010)

##	Country specific population-specific variables:
##		Growth rates are estimated for urban and rural areas for the years
##		included in GR_years. (file: growth_rates.xlsx)
##
##	!!!CAUTION!!!: if we use more recent census data (see the
##			census_folder option below), it must be the rate from the more
##			recent census data to	2010 (i.e. from 2009 to 2010 for Vietnam) -
##			so calculation, for	VNM, is for one year:
##  NOTE:  The parameters for the below functions should not be changed 
##         within the function call; change the input vectors that are 
##         declared for the parameters instead.
 GR_urb <- GR_calculator(iso = country, 
                         target_years = GR_years,
                         urb = TRUE,
                         orig_year = censusdat_year)
 GR_rur <- GR_calculator(iso = country, 
                         target_years = GR_years,
                         urb = FALSE,
                         orig_year = censusdat_year)


##	Processing flags:

##	Set UNADJUST to True if we want to produce a map adjusted to UN totals
##		for each target year, False otherwise:
UNADJUST <- c( TRUE, TRUE, TRUE)


## If UNADJUST == True then we need to provide the UN total population for
##		that year - needed if you want to adjust map for U.N. esimates.
##		U.N. estimates are from the World Urbanization Prospects
##		(http://esa.un.org/unpd/wup/index.htm)
##  RWA
UNPOP <- popRetriever(iso = country,
                      target_years = GR_years)

##NAM UGM TESTS
#UNPOP <- c(1898000,2179000)
#UNPOP <- c(8396000)


##	Should we skip processing and creation for any existing data sets:
skip_existing <- FALSE

##	END:	Set per-country configuration options
#####



#####
##	NOTICE:  In practice nothing below this line should need to be regularly
##		edited unless very specific details need to be changed about the
##		modeling process (e.g. speciyfing an extent | something detailed
##		about the Python processing).
#####

#####
##	BEGIN:	Set general configuration options

## Set general paths
data_path <- paste0(root_path, "/data/", country, "/")
output_path <- paste0(root_path, "/output/", country, "/")
tmp_path <- paste0(output_path, "/tmp/")


##	END:	Set general configuration options
#####



#####
##	BEGIN: Import packages and set GeoProcessing environment

require(rjson)
require(rgdal)
require(raster)
require(snow)
require(RQGIS)


##  Set the QGIS environment (can change to set_env() if unsure of location):
my_env <- set_env("C:\\Program Files\\QGIS Essen")

##  Input projection is configured for each country based on the most
##  	appropriate output for distance/area considerations...
##	  Final projection for population maps, GCS 1984 in proj4 syntax:
final_prj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

##	Configure the geoprocessing environment settings:
## Default compression method is set to LZ77, default overwrite status is TRUE, 
## the default is to NOT build pyramids, and the default extents values and snap Raster
## path are NULL until needed and defined
geo_env <- list(overwrite = TRUE, outputCoordinateSystem = final_prj,
                extent = NULL, snap_raster = NULL)

## Make available all nodes for computing
nodes <- 6
beginCluster( nodes )

##	END: Import packages and set GeoProcessing environment
#####



#####
##	BEGIN: Define utility functions ##

##	Create a directory function to check for and create data directories if they don't exist:
ensure_dir <- function( d ){
  if ( !file.info( d )[[2]] ){
    file.create( d )
  } 
  return( d )
}


## Create a funciton to replicate the "Con()" function found in ArcGIS Raster Calculator, adds
## which the true value times the condition being true to the false value times the condition being not true
Con <- function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)
}


##	END: Define utility functions
#####



#####
##	BEGIN: Data pre-processing for needed datasets


##	Get a character vector of all existing folders in the country's data folder:
dataset_folders <- list.files(path = data_path, full.names = FALSE)


##	Check to see if an alternative census folder exists in the folder list
##		and use it for processing our maps.
##	TODO:  Eventually this needs to be pulled from the Metadata.R file
##		instead of a hard coded directory presence.
if("! New Census" %in% dataset_folders){
  census_folder <- "! New Census"
}else{
  census_folder <- ""
}


##	Datasets:
##	  Estimated population density from randomForest to be used as density
##		weightings (gridx) from the old model, a file that should be in the
##		directory (we changed formats between version numbers so check for both:
dataset_path <- Sys.glob( paste0( output_path, "predict_density.tif" ))
if(dataset_path != ""){
  popdensity_weighting <- dataset_path
}else {
  print("ERROR:  No \"predict_density\" TIF | IMG found in the output folder!  You first need to run the 1.3 R script!")
  stop()
}

##	Population data
if(census_folder != ""){
  census_path <- paste0(data_path, census_folder, "/")
}else{
  census_path <- paste0(data_path, "Census/")
}

##	Instead of hardcoding it above we'll just pull in the only shapefile
##	that should be in the directory:
dataset_name <- basename( Sys.glob( file.path( paste0( census_path, "*.shp" ) ) ) )
adminpop <- readOGR(dsn = paste0(census_path, dataset_name), layer = strsplit(dataset_name, ".shp")[[1]])

##	Set the workspace for data creation and processing:
setwd(ensure_dir(tmp_path))


##	END: Data pre-processing for needed datasets
#####



#####
##	BEGIN: Population map creation

print("PREPROCESS: Finalized density weights...")

##	Project our predicted density weighting file back to GCS1984 (i.e. final_prj):
in_path <- popdensity_weighting
out_name <- "predict_density_prj.tif"

## Designate the output path and temporarily set our working directory to it:
out_path <- paste0( output_path, "tmp/", out_name )
setwd( output_path )

## Reproject the predicted density weighting file and write it as a new Raster: 
in_raster <- brick(in_path)
print("PREPROCESS: Projecting Popdensity Weighting Raster...")
popdensity_weighting_proj <- projectRaster( in_raster, res = c(0.000833),
                                            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                            method = 'bilinear', filename = out_name, format = 'GTiff',
                                            overwrite = geo_env$overwrite, options=c('COMPRESS=LZW') ) 

## Reset our working directory
setwd(tmp_path)


##	Set our snapping environment to our new, reprojected and
##	    prediction_density layer:
popdensity_weighting_final <- popdensity_weighting_proj
geo_env$snap_raster <- popdensity_weighting_final

##  Set the working directory back to the temporary path for data creation and processing:
setwd( ensure_dir( tmp_path ) )

##	Create a temporary copy of our census data with an appropriate
##	   population field to sum and distribute:
admin_pop <- readOGR( dsn = paste0(census_path, dataset_name), layer = strsplit( dataset_name, ".shp" )[[1]] )

## Write a copy of that OGR object to the temporary path
writeOGR(admin_pop, dsn = paste0(tmp_path,"admin_Union.shp"), layer = "admin_Union", check_exists = geo_env$overwrite,
         overwrite_layer = geo_env$overwrite, driver = "ESRI Shapefile" )

## Read the admin_Union file in as an OGR object
admin_Union <- readOGR( dsn = paste0(tmp_path, "admin_Union.shp"), layer = "admin_Union")

##  If the field POP is not in the shapefile, add it and copy the ADMINPOPfield:
if(is.null(admin_Union$POP)){
  admin_Union$POP <- admin_Union$ADMINPOP
  
  ##  Save the  new field to the shapefile
  writeOGR(admin_Union, dsn = paste0(tmp_path,"admin_Union.shp"), layer = "admin_Union", check_exists = geo_env$overwrite,
           overwrite_layer = geo_env$overwrite, driver = "ESRI Shapefile" )
}


##  We need the landcover file for the country projected into our output projection:
if(!("Landcover" %in% dataset_folders)){
  print("ERROR:  No \"Landcover\" folder found!  This is required and indicates you possibly did not run the \"Data Preparation, R.r\" script | specify configuration options correctly in this Python processing script!")
  stop()
} else {
  ## Remove the folder "Landcover" from the list of dataset folders 
  dataset_folders <- dataset_folders[dataset_folders != "Landcover"]
}

##	Clip and project our land cover Raster:
dataset_folder <- "Landcover"
print( paste0("PREPROCESS:  ", dataset_folder) )


##	Instead of hardcoding it above we'll just pull in the only TIF | IMG
##		file that should be in the directory:
dataset_path <- c(Sys.glob( file.path( paste0( data_path, dataset_folder, "/", "*.img" ) ) ), Sys.glob( file.path( paste0( data_path, dataset_folder, "/", "*.tif" ) ) ) )
if(!is.null(dataset_path)){
  dataset_name <- basename( dataset_path )
} else {
  print("ERROR:  No land cover file found!")
  stop()
}

in_path <- paste0( data_path, dataset_folder, "/Derived/landcover.tif" )

output_name <- "landcover_popmap.tif"

## Check if the outpath is a valid directory and create the folder if it does not
out_path <- ensure_dir( paste0( data_path, dataset_folder, "/Derived/" ) )
landcover_path <- paste0( out_path, output_name )

## Create an Raster Brick object of the landcover (Which should already be in a projected coord sys):
#in_raster <- raster(in_path)
in_raster <- brick(in_path)

## Project the Raster and save it as a new Raster:
if (is.na(file.info( landcover_path )[[2]]) | !skip_existing){
  ##  Project the raster into WGS84:
   projectRaster(from = in_raster, res = c(0.000833), crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                 method = "ngb", filename = landcover_path, format = "GTiff",
                 overwrite = geo_env$overwrite, options = c("COMPRESS=LZW"))  
   
  #gdalwarp(in_raster, landcover_path, proj4string(in_raster),'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs', ...)
}

landcover_prj <- in_raster
landcover_wgs <- brick(landcover_path)
##  Set our currently used landcover to the WGS landcover
landcover <- brick(landcover_path)

##	If use_urban is True then we are going to burn in the Urban dataset
##		converted to binary representation into our land cover for urban/rural
##		growth rate application:
if(use_urban){
  if(!("Urban" %in% dataset_folders)){
    print("ERROR:  No \"Urban\" folder found!  This is required per your configuration of use_urban in this script and indicates you possibly did not run the \"Data Preparation, R.r\" script | specify configuration options correctly in this Python processing script!")
    stop()
  } else { 
    ## Remove Urban from the datasets folder
    dataset_folders <- dataset_folders[dataset_folder != "Urban"]
  }
  
  ##	Clip and project our land cover Raster:
  dataset_folder <- "Urban"
  print( paste0( "PREPROCESS:  ", dataset_folder ) )
  
  
  ##	Instead of hardcoding it above we'll just pull in the only TIF | IMG
  ##		file that should be in the directory:
  dataset_path <- Sys.glob( file.path( paste0( data_path, dataset_folder, "/Derived/urban_cls.tif" )))
  
  if(basename(dataset_path) == "urban_cls.tif"){
    dataset_name <- basename( dataset_path )
  } else {
    print("ERROR:  No urban cover file found!")
    stop()
  }
  
  in_path <- paste0(data_path, dataset_folder, "/Derived/", dataset_name)
  
  output_name <- "urban_popmap.tif"
  
  out_path <- ensure_dir( paste0( data_path, dataset_folder, "/Derived/") )
  urban_path <- paste0( out_path, output_name )
  
  in_raster <- brick(in_path)
  
  ## If the file is not a file (i.e. a directory) | skip_existing is NOT TRUE:
  if (is.na(file.info(urban_path)[[2]]) | !skip_existing){
    projectRaster(in_raster, to = landcover, res = c(0.000833), crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), 
                  method = "ngb", filename = urban_path, format = "GTiff", overwrite = geo_env$overwrite,
                  options=c("COMPRESS=LZW"))
  }
  
  ##	This assumes that your urban Raster, however it was created is binary
  ##		 with 1 indicating urban areas and 0 indicating non-urban:
  urban_wgs <- brick(urban_path)
  urban_prj <- in_raster
  urban <- brick(urban_path)
  
  ## Re-set the extent based on the sometimes slightly different extent
  ##    post-cropping:
  my_extent <- extent(popdensity_weighting_final)
  
  ##  Confirm that our extent matches the minimum extent for the urban data set.
  ##		Small inconsistencies can arrive as a product of the ArcGIS 
  ##		processing so this is a necessary step otherwise the stacking process
  ##		will fail in some cases:
  for(ras_set in c(urban, landcover)){
    assign("tmp_raster", raster( ras_set ))
    if (xmin(tmp_raster) > xmin(my_extent)) { my_extent@xmin <- xmin(tmp_raster) }
    if (xmax(tmp_raster) < xmax(my_extent)) { my_extent@xmax <- xmax(tmp_raster) }
    if (ymin(tmp_raster) > ymin(my_extent)) { my_extent@ymin <- ymin(tmp_raster) }
    if (ymax(tmp_raster) < ymax(my_extent)) { my_extent@ymax <- ymax(tmp_raster) }
  }
  ## Crop the urban extents
  urban <- crop(urban, my_extent)
  landcover <-crop(landcover, my_extent)
  
  ##  FRS: I added this as I believe this is a bug with the raster package.  This
  ##    shouldn't need to be run, and it will *not* fix problems in differing
  ##    rows and columns, if there's ever a problem where the crop() fails to
  ##    produce rasters of identical sizes (e.g. when there are very small diff.
  ##    in cell size for example).  This is a stop-gap hack and hopefully could
  ##    be removed if raster package works correctly in the future:
  for (var_name in c("urban", "landcover")) {
    print(paste("Fixing Extent: ", var_name, sep=""))
    eval(parse(text=paste("extent(", var_name, ") <- my_extent", sep="")))
    flush.console()
  }
  
  
  ##	Now adjust land cover by burning in our urban data:
  #landcover = landcover*(urban) + 190*(urban)
  ##	NOTE: From 2014-05-16 to 2014-09-24 this line was the previous rather
  ##		than the following, which incorrectly burned in the urban class into
  ##		the land cover and hence resulted in the incorrect, rural growth rate
  ##		being applied to the entire map, rather than the urban growth rate.
  ##		The following corrects this:
  ##  Save this land cover out for testing (can be commented):
  outPath <- paste0(output_path, "tmp/landcover_urban.tif")
  
  landcover <- landcover * Con( urban == 1,0,1 ) + 190*( urban )
  ##  FRS: I fixed this for testing... Raster() should be raster() but since it's
  ##    already a raster object we just use it directly.  And the file name is
  ##    what we set it to above...
  #writeRaster( Raster(landcover), dsn = out_path, layer = strsplit( basename( outPath ), ".tif" ), method = "GTiff",
  landcover <- writeRaster( landcover, file=outPath, method = "GTiff",
                            overwrite = geo_env$overwrite, datatype = "INT2U", NAflag = 65535, 
                            options = c("COMPRESS=LZW") )
}

##	Population redistribution procedure using population from "admin_Union.shp"
##		converted to a Raster and redistributed according to our weights:
print("POPMAP: Begin creating population redistribution...")


print("PPP: Using census file - census.shp")

## Transform the admin_Union POP data into a Raster from its current OGR onject
##
## rasterize is slow so we're going to use gdal_raster below
##
## require(gdalUtils)
## gdal_rasterize(
##  #src_datasource=gsub("/", "\\\\", paste0(tmp_path,"admin_Union.shp")
##  src_datasource=census_buffer_path,
##  l="admin_Union", 
##  dst_filename="C:\\tmp\\gridp.tif",
##  burn=1,
##  #te=c(my_extent@xmin, my_extent@ymin, my_extent@xmax, my_extent@ymax),
##  #tr=c(xres(geo_env$snap_raster), yres(geo_env$snap_raster)),
##  #ts=c(geo_env$snap_raster@ncols, geo_env$snap_raster@nrows),
##  output_raster=TRUE)

## Create the Raster containing zones to be used in zonal statistics:
temp_ras <- Con(geo_env$snap_raster > 0, 1, 0)
temp_ras[] <- NA
##  JJN 2016-09-06:  Switching to qgis:zonalstatistics fuction so a zonal raster is no longer needed.
gridz <- writeRaster(temp_ras, "C:/tmp/gridz.tif", format = "GTiff", datatype = "INT4U", overwrite = geo_env$overwrite, options=c("COMPRESS=LZW"))
system(paste0("gdal_rasterize -l admin_Union -a ADMINID ", gsub("/", "\\\\", paste0(tmp_path,"admin_Union.shp")), " C:\\tmp\\gridz.tif"))
#system(paste0("gdal_rasterize -l admin_Union -a ADMINID -a_nodata 65535 -init 65535 ", gsub("/", "\\\\", paste0(tmp_path,"admin_Union.shp")), " C:\\tmp\\gridz.tif"))
gridz <- raster("C:/tmp/gridz.tif")  ##  Needed as raster for the zonal stats portion


## Rasterize the population values in admin_Union:
gridp <- writeRaster(temp_ras, "C:/tmp/gridp.tif", format = "GTiff", datatype = "INT4U", overwrite = geo_env$overwrite, options=c("COMPRESS=LZW"))
system(paste0("gdal_rasterize -l admin_Union -a POP ", gsub("/", "\\\\", paste0(tmp_path,"admin_Union.shp")), " C:\\tmp\\gridp.tif"))
##  FRS: Potential solution...
#system(paste0("gdal_rasterize -l admin_Union -a POP -a_nodata 65535 -init 65535 ", gsub("/", "\\\\", paste0(tmp_path,"admin_Union.shp")), " C:\\tmp\\gridp.tif"))
gridp <- brick("C:/tmp/gridp.tif")


## Get the sum of the final pop density weights by zone
gridy <- writeRaster(temp_ras, "C:/tmp/gridy.tif", format = "GTiff", datatype = "FLT4S", overwrite = geo_env$overwrite, options=c("COMPRESS=LZW"))
gridy <- zonal(popdensity_weighting_final, gridz, fun = "sum", na.rm = TRUE)

## Merge the zonal results back into the shapefile and rerasterize
colnames(gridy) <- c("ADMINID", "SUM")
admin_Union_sum <- merge(admin_Union, as.data.frame(gridy), by = "ADMINID")

writeOGR(admin_Union_sum, dsn = paste0(tmp_path, "admin_Union_sum.shp"), layer = "admin_Union_sum", driver = "ESRI Shapefile", overwrite_layer = TRUE)


##  Set parameters to be passed to the qgis function:
# print("Getting and Setting QGIS parameters...")
# params <- get_args(alg = "qgis:zonalstatistics", qgis_env = my_env)
# params$INPUT_RASTER <- popdensity_weighting_final
# params$RASTER_BAND <- 1
# params$INPUT_VECTOR <- paste0(tmp_path,"admin_Union.shp")
# params$COLUMN_PREFIX <- "Q"
# params$OUTPUT_LAYER <- paste0(tmp_path,"admin_Union_sum.shp")
# admin_Union_sum <- readOGR(dsn = paste0(tmp_path, "admin_Union_sum.shp"), layer = "admin_Union_sum")
# 
# print("Calculating QGIS Zonal Statistics...")
# ##  Carry out the zonal statistics:
# run_qgis(alg = "qgis:zonalstatistics",
#          params = params,
#          qgis_env = my_env)

##  Rasterize the results of the zonal statistics.
system(paste0("gdal_rasterize -l admin_Union_sum -a Qsum ", gsub("/", "\\\\", paste0(tmp_path,"admin_Union_sum.shp")), " C:\\tmp\\gridy.tif"))
gridy <- brick("C:/tmp/gridy.tif")

##  Put gridz as a brick to save on memory:
gridz <- brick("C:/tmp/gridz.tif")

##  Clear the temprorary rasters to ave on memory:
removeTmpFiles(h = 1)

##	Read the first row of the attribute table from our census data to
##	determine the year of our census data:
census_year <- adminpop[1,"YEARPOP"]

##	Calculate population map for census year:
print(paste0("PPP: Calculating People Per Pixel ", as.character(census_year$YEARPOP)))

outPath <-paste0(output_path, country, "_ppp_v", rf_version, "_", as.character(census_year$YEARPOP), ".tif")
dataset_path <- outPath

if(is.na(file.info(dataset_path)[[1]]) | !skip_existing){
  if(round_counts){ 
    popmap <- (gridp * brick(popdensity_weighting_final) / gridy) + 0.5
    
  } else {
    popmap <- gridp * brick(popdensity_weighting_final) / gridy
  }
  
  ##	NOTE: So there's a bug in the arcpy Raster optimization that
  ##		often, even though you're specifying the .save() option will not
  ##		actually write it out to disk until it thinks that you're really
  ##		removing it from memory.  To force this to happen across versions
  ##		of ArcGIS and arcpy set the Raster object to None like this.
  ##		Discovered here:
  ##			http://gis.stackexchange.com/questions/46897/saving-rasters-in-a-python-for-loop-fails-only-on-last-iteration
  
  ##		Also, in order to implement compression and make sure we are only using
  ##		32-bit Floating output I implemented a temporary Raster save plus
  ##		call to copy Raster, subsequently removing the temporary file and
  ##		resulting output pyramid layer for reduced data storage:
  #popmap.save(outPath)
  #popmap <- NULL
  
  ## Make a copy of the popmap Raster
  popmap <- writeRaster(popmap, filename = outPath, method = "GTiff",
                        overwrite = geo_env$overwrite, datatype = "FLT4S", 
                        options = c( "COMPRESS=LZW" ))
  
  popmap <- NULL
  
  popmap <- brick(outPath)
}


i <- 1
for (popyear in GR_years){
  landcover <- landcover_wgs
  urban <- urban_wgs
  
  ## Re-set the extent based on the sometimes slightly different extent
  ##    post-cropping:
  my_extent <- extent(popdensity_weighting_final)
  
  ##  Confirm that our extent matches the minimum extent for the given data set.
  ##    Small inconsistencies can arrive as a product of the ArcGIS 
  ##		processing so this is a necessary step otherwise the stacking process
  ##		will fail in some cases:
  for(ras_set in c(popmap, landcover)){
    assign("tmp_raster", raster( ras_set ))
    if (xmin(tmp_raster) > xmin(my_extent)) { my_extent@xmin <- xmin(tmp_raster) }
    if (xmax(tmp_raster) < xmax(my_extent)) { my_extent@xmax <- xmax(tmp_raster) }
    if (ymin(tmp_raster) > ymin(my_extent)) { my_extent@ymin <- ymin(tmp_raster) }
    if (ymax(tmp_raster) < ymax(my_extent)) { my_extent@ymax <- ymax(tmp_raster) }
  }
  
  
  if(popyear != census_year$YEARPOP){
    print(paste("PPP: Calculating People Per Pixel ", as.character(popyear)))
    
    outPath <- paste0(output_path, country, "_ppp_v", rf_version, "_", as.character(GR_years[i]), ".tif")
    dataset_path <- outPath
    
    
    if(is.na(file.info(dataset_path)[[2]]) | !skip_existing){
      if(round_counts){
        ## Crop the popmap and landcover extents:
        popmap <- crop(popmap, my_extent)
        landcover <- crop(landcover, my_extent)
        
        ##  FRS: I added this as I believe this is a bug with the raster package.  This
        ##    shouldn't need to be run, and it will *not* fix problems in differing
        ##    rows and columns, if there's ever a problem where the crop() fails to
        ##    produce rasters of identical sizes (e.g. when there are very small diff.
        ##    in cell size for example).  This is a stop-gap hack and hopefully could
        ##    be removed if raster package works correctly in the future:
        for (var_name in c("popmap")) {
          print(paste("Fixing Extent: ", var_name, sep=""))
          eval(parse(text=paste("extent(", var_name, ") <- extent(landcover)", sep="")))
          flush.console()
        }
        
        popmap_year <- (popmap * (landcover != 190.0) * GR_rur[i] + popmap * (landcover == 190.0) * GR_urb[i]) + 0.5
      } else {
        ## Crop the urban extents
        popmap <- crop(popmap, my_extent)
        landcover <- crop(landcover, my_extent)
        
        ##  FRS: I added this as I believe this is a bug with the raster package.  This
        ##    shouldn't need to be run, and it will *not* fix problems in differing
        ##    rows and columns, if there's ever a problem where the crop() fails to
        ##    produce rasters of identical sizes (e.g. when there are very small diff.
        ##    in cell size for example).  This is a stop-gap hack and hopefully could
        ##    be removed if raster package works correctly in the future:
        for (var_name in c("popmap")) {
          print(paste("Fixing Extent: ", var_name, sep=""))
          eval(parse(text=paste("extent(", var_name, ") <- extent(landcover)", sep="")))
          flush.console()
        }
        
        popmap_year <- popmap * (landcover != 190.0) * GR_rur[i] + popmap * (landcover == 190.0) * GR_urb[i]
      }
      
      #popmap_year.save(outPath)
      #popmap_year <- NULL
      popmap_year <- writeRaster( popmap_year, filename = outPath, method = "GTiff",
                                  overwrite = geo_env$overwrite, datatype = "FLT4S", 
                                  options=c("COMPRESS=LZW") )
      popmap_year <- NULL
    }
    
    popmap_year <- brick(outPath)
  } else {
    popmap_year <- popmap
  }
  
  if (UNADJUST[i]){
    print(paste("PPP: Calculating People Per Pixel, UN Adjusted ", as.character(popyear)))
    outPath <- paste0(output_path, country, "_ppp_v", rf_version, "_", as.character(GR_years[i]), "_UNadj.tif")
    dataset_path <- Sys.glob( outPath )
    
    ## Sum up the 
    if (length(dataset_path) == 0 | !skip_existing){
#       ##  Kludge: Fix the extents of popmap_year and gridz:
#       ## Crop the popmap_year and gridz extents:
#       popmap_year <- crop(popmap_year, extent(landcover))
#       gridz <- crop(gridz, extent(landcover))
#       
#       ##  FRS: I added this as I believe this is a bug with the raster package.  This
#       ##    shouldn't need to be run, and it will *not* fix problems in differing
#       ##    rows and columns, if there's ever a problem where the crop() fails to
#       ##    produce rasters of identical sizes (e.g. when there are very small diff.
#       ##    in cell size for example).  This is a stop-gap hack and hopefully could
#       ##    be removed if raster package works correctly in the future:
#       for (var_name in c("popmap_year", "gridz")) {
#         print(paste("Fixing Extent: ", var_name, sep=""))
#         eval(parse(text=paste("extent(", var_name, ") <- extent(landcover)", sep="")))
#         flush.console()
#       }
#       
#       ##  Calculate the zonal statistics:
#       zonsum <- zonal(popmap_year, gridz, fun ="sum", na.rm = TRUE)
#       
#       ##  Kludge: Convert the zonal output to raster:
#       colnames(zonsum) <- c("ADMINID", "PPPSUM")
#       admin_Union_PPPSum <- merge(admin_Union, as.data.frame(zonsum), by = "ADMINID")
#       
#       writeOGR(admin_Union_PPPSum, dsn = paste0(tmp_path, "admin_Union_PPPSum.shp"), layer = "admin_Union_PPPSum", driver = "ESRI Shapefile", overwrite_layer = TRUE)
#       admin_Union_PPPSum <- readOGR(dsn = paste0(tmp_path, "admin_Union_PPPSum.shp"), layer = "admin_Union_PPPSum")
#       
#       grid_PPPSum <- writeRaster(temp_ras, "C:/tmp/grid_PPPSum.tif", format = "GTiff", datatype = "FLT4S", overwrite = geo_env$overwrite, options=c("COMPRESS=LZW"))
#       system(paste0("gdal_rasterize -l admin_Union_PPPSum -a PPPSUM ", gsub("/", "\\\\", paste0(tmp_path,"admin_Union_PPPSum.shp")), " C:\\tmp\\grid_PPPSum.tif"))
#       
#       grid_PPPsum <- brick("C:/tmp/grid_PPPSum.tif")
#       
#       const <- grid_PPPSum * 0 + UNPOP[i]
#       
#       ##  Correct the extents of grid_PPPSum and const:
#       ## Crop the popmap_year and gridz extents:
#       grid_PPPSum <- crop(grid_PPPSum, extent(landcover))
#       const <- crop(const, extent(landcover))
#       
#       ##  FRS: I added this as I believe this is a bug with the raster package.  This
#       ##    shouldn't need to be run, and it will *not* fix problems in differing
#       ##    rows and columns, if there's ever a problem where the crop() fails to
#       ##    produce rasters of identical sizes (e.g. when there are very small diff.
#       ##    in cell size for example).  This is a stop-gap hack and hopefully could
#       ##    be removed if raster package works correctly in the future:
#       for (var_name in c("grid_PPPSum", "const")) {
#         print(paste("Fixing Extent: ", var_name, sep=""))
#         eval(parse(text=paste("extent(", var_name, ") <- extent(landcover)", sep="")))
#         flush.console()
#       }
      
      if (round_counts){        
        popmap_year_adj <- (popmap_year * (UNPOP[i] / sum(popmap_year[], na.rm=T))) + 0.5 
      } else {
        popmap_year_adj <- popmap_year * (UNPOP[i] / sum(popmap_year[], na.rm=T))
      }
      
      #popmap_year_adj.save(outPath)
      #popmap_year_adj = None
      popmap_year_adj <- writeRaster(popmap_year, filename = outPath, method = "GTiff",
                                     overwrite = geo_env$overwrite, datatype = "FLT4S", 
                                     options = c("COMPRESS=LZW") )
    }
    
    popmap_year_adj <- brick( outPath )
  }
  
  i <- i + 1
}


print("PPP: Completed!")

######
##  END: PPP Calculations
######


######
##  BEGIN: PPHa Calculations
######

##	Now we need to do the same calculations but on the projected data,
##		creating output in people per hectare instead of people per pixel
##		and not converting back to geographic coordinates:
##	Set our snapping environment to the prediction density weighting layer:
## JJN:  Was having issues with the only prediction density raster in 
##       the file already being in WGS84 so this step is to ensure that
##       a suitable popdensweighting file being available in a 
##       projected coordinate system as taken from the originally imported
##       landcover raster (Hence why this is placed down here):
popdensity_weighting_pph <- projectRaster(popdensity_weighting_final, landcover_prj,
                                          method = "bilinear", 
                                          filename = paste0(output_path, "predictdensity_prj.tif"),
                                          format = "GTiff", overwrite = geo_env$overwrite,
                                          options=c("COMPRESS=LZW"))
popdensity_weighting_final <- popdensity_weighting_pph

## Diagnostic for popdensity prj:
print(paste0("Popdensity PRJ at entry:"))
print(popdensity_weighting_final)

geo_env$snap_raster <- popdensity_weighting_final

## Make sure the admin_prj.shp exists in the working directory and if
## it is not, create it:
adminpop_prj <- paste0(census_path, "Derived/census.shp")

if(!("admin_Union_prj" %in% list.files(path = getwd(), full.names = FALSE))){
  ## Read the derived census data .shp as an OGR object
  adminpop_prj<- readOGR(dsn = paste0(census_path, "Derived/", "census.shp"), layer = "census")
  
  ## Create a copy of it in the working directory
  writeOGR(adminpop_prj, dsn = paste0(getwd(), "/admin_Union_prj.shp"), layer = "admin_Union_prj", driver = "ESRI Shapefile", overwrite_layer = TRUE)
}

admin_Union_prj <- readOGR(dsn = paste0(getwd(),"/admin_Union_prj.shp"), layer = "admin_Union_prj")

## Check the status of the POP field in the new OGR object and populate it if it is empty:
if(length(admin_Union_prj$POP) == 0 | length(admin_Union_prj) == 0){
  admin_Union_prj$POP <- admin_Union_prj$ADMINPOP
  writeOGR(admin_Union_prj, dsn = paste0(getwd(), "/admin_Union_prj.shp"), layer = "admin_Union_prj", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  admin_Union_prj <- readOGR(dsn = paste0(getwd(),"/admin_Union_prj.shp"), layer = "admin_Union_prj")
}

##	Load our already projected landcover file:
landcover <- landcover_prj


##	If use_urban is True then we are going to burn in the Urban dataset
##		converted to binary representation into our land cover for urban/rural
##		growth rate application:
if (use_urban) {
  #dataset_folder <- "Urban"
  
  ##	Instead of hardcoding it above we'll just pull in the only TIF | IMG
  ##		file that should be in the directory:
  #dataset_path <- (Sys.glob( paste0(data_path, dataset_folder, "/Derived/urban_cls.tif" )))
  
  #if (!file.info(dataset_path)[[2]]){
  #  dataset_name <- basename( dataset_path )
  #}else{
  #  print("ERROR:  No urban cover file found!")
  #  stop()
  #}
  
  #urban_path <- paste0(data_path, dataset_folder, "/Derived/", dataset_name)
  
  ##	This assumes that your urban Raster, however it was created is binary
  ##		with 1 indicating urban areas and 0 indicating non-urban:
  ##  Project our urban raster:
  urban <- projectRaster(urban, landcover_prj)
  
  ##  Kludge to ensure the landcover and urban extents match:
  ##    Confirm that our extent matches the minimum extent for the given data set.
  my_extent <- extent(popdensity_weighting_final)
  
  for(ras_set in c(landcover, urban)){
    assign("tmp_raster", raster( ras_set ))
    if (xmin(tmp_raster) > xmin(my_extent)) { my_extent@xmin <- xmin(tmp_raster) }
    if (xmax(tmp_raster) < xmax(my_extent)) { my_extent@xmax <- xmax(tmp_raster) }
    if (ymin(tmp_raster) > ymin(my_extent)) { my_extent@ymin <- ymin(tmp_raster) }
    if (ymax(tmp_raster) < ymax(my_extent)) { my_extent@ymax <- ymax(tmp_raster) }
  }
  
  ##  Correct the extents of landcover and urban:
  ##  Crop the popmap_year and gridz extents:
  landcover <- crop(landcover, my_extent)
  urban <- crop(urban, my_extent)
  
  ##  FRS: I added this as I believe this is a bug with the raster package.  This
  ##    shouldn't need to be run, and it will *not* fix problems in differing
  ##    rows and columns, if there's ever a problem where the crop() fails to
  ##    produce rasters of identical sizes (e.g. when there are very small diff.
  ##    in cell size for example).  This is a stop-gap hack and hopefully could
  ##    be removed if raster package works correctly in the future:
  for (var_name in c("landcover", "urban")) {
    print(paste("Fixing Extent: ", var_name, sep=""))
    eval(parse(text=paste("extent(", var_name, ") <- my_extent", sep="")))
    flush.console()
  }
  
  ##	Now adjust land cover by burning in our urban data:
  #landcover <- landcover*(urban) + 190*(urban)
  ##	NOTE: From 2014-05-16 to 2014-09-24 this line was the previous rather
  ##		than the following, which incorrectly burned in the urban class into
  ##		the land cover and hence resulted in the incorrect, rural growth rate
  ##		being applied to the entire map, rather than the urban growth rate.
  ##		The following corrects this:
  
  landcover <- landcover * Con( urban == 1, 0, 1 ) + 190*( urban )
}

##	Begin processing people per hectare using the same output but assuming
##		our projected data have a linear unit of meters and a square pixel
##		output size of 100m, snapped to the prediction density layer.
print( paste0(  "PPHa: Using census file - census.shp" ) )

##  Project the template raster to the new coordinate system
##  Project the raster into hectares
projectRaster(temp_ras, landcover_prj, filename = paste0(tmp_path,"temp_ras.tif"),
              format = "GTiff", overwrite = geo_env$overwrite, options=c("COMPRESS=LZW"))

temp_ras <-brick(paste0(tmp_path,"temp_ras.tif"))

##  Reproject admin_Union_prj into the correct projection for PPHa calculations:
admin_Union_prj <- spTransform(admin_Union_prj, CRS(proj4string(landcover)))
writeOGR(admin_Union_prj, dsn = getwd(), layer = "admin_Union_prj", driver = "ESRI Shapefile", overwrite_layer = TRUE)


## Rasterization of the zones to be used in zonal statistics:
gridz <- writeRaster(temp_ras, "C:/tmp/gridz.img", format = "HFA", datatype = "INT4U", overwrite = geo_env$overwrite, options=c("COMPRESS=LZW"))
system(paste0("gdal_rasterize -l admin_Union_prj -a ADMINID ", gsub("/", "\\\\", paste0(tmp_path,"admin_Union_prj.shp")), " C:\\tmp\\gridz.img"))
gridz <- raster("C:/tmp/gridz.img")
##  Brick gridz to save on memory:
##JJN:  Can't because zonal requires raaster layers
#gridz <- brick("C:/tmp/gridz.img")


## Rasterize the population data:
gridp <- writeRaster(temp_ras, "C:/tmp/gridp.img", format = "HFA", datatype = "INT4U", overwrite = geo_env$overwrite, options=c("COMPRESS=LZW"))
system(paste0("gdal_rasterize -l admin_Union_prj -a POP ", gsub("/", "\\\\", paste0(tmp_path,"admin_Union_prj.shp")), " C:\\tmp\\gridp.img"))
##  FRS:  Potential solution:
#system(paste0("gdal_rasterize -l admin_Union_prj -a POP -a_nodata 65535 -init 65535 ", gsub("/", "\\\\", paste0(tmp_path,"admin_Union_prj.shp")), " C:\\tmp\\gridp.img"))
gridp <- brick("C:/tmp/gridp.img")

##  Clear the temporary files:
removeTmpFiles(h=1)

##  Kludge - Correction for the extents prior to zonal calculation
##  Re-set the extent based on the sometimes slightly different extent
##    post-cropping:
my_extent <- extent(popdensity_weighting_final)

for(ras_set in c(gridz, popdensity_weighting_final)){
  assign("tmp_raster", raster( ras_set ))
  if (xmin(tmp_raster) > xmin(my_extent)) { my_extent@xmin <- xmin(tmp_raster) }
  if (xmax(tmp_raster) < xmax(my_extent)) { my_extent@xmax <- xmax(tmp_raster) }
  if (ymin(tmp_raster) > ymin(my_extent)) { my_extent@ymin <- ymin(tmp_raster) }
  if (ymax(tmp_raster) < ymax(my_extent)) { my_extent@ymax <- ymax(tmp_raster) }
}

## Crop gridz and popdensity_weighting_final:
gridz <- crop(gridz, my_extent)
popdensity_weighting_final <- crop(popdensity_weighting_final, my_extent)

## Diagnostic for popdensity prj:
print(paste0("Popdensity PRJ after cropping:"))
print(popdensity_weighting_final)

##  FRS: I added this as I believe this is a bug with the raster package.  This
##    shouldn't need to be run, and it will *not* fix problems in differing
##    rows and columns, if there's ever a problem where the crop() fails to
##    produce rasters of identical sizes (e.g. when there are very small diff.
##    in cell size for example).  This is a stop-gap hack and hopefully could
##    be removed if raster package works correctly in the future:
for (var_name in c("gridz", "popdensity_weighting_final")) {
  print(paste("Fixing Extent: ", var_name, sep=""))
  eval(parse(text=paste("extent(", var_name, ") <- my_extent", sep="")))
  flush.console()
}


## Calculate the zonal statistics (SUM) of the final pop density weighting by zone:
gridy <- zonal(popdensity_weighting_final, gridz, fun = "sum") 

## Convert the zonal matrix back to a raster:
colnames(gridy) <- c("ADMINID", "PPHaSUM")
admin_Union_PPHaSum <- merge(admin_Union_prj, as.data.frame(gridy), by = "ADMINID")

writeOGR(admin_Union_PPHaSum, dsn = paste0(tmp_path, "admin_Union_PPHaSum.shp"), layer = "admin_Union_PPHaSum", driver = "ESRI Shapefile", overwrite_layer = TRUE)
admin_Union_PPHaSum <- readOGR(dsn = paste0(tmp_path, "admin_Union_PPHaSum.shp"), layer = "admin_Union_PPHaSum")

grid_PPHaSum <- writeRaster(temp_ras, "C:/tmp/grid_PPHaSum.tif", format = "GTiff", datatype = "FLT4S", overwrite = geo_env$overwrite, options=c("COMPRESS=LZW"))
system(paste0("gdal_rasterize -l admin_Union_PPHaSum -a PPHaSUM ", gsub("/", "\\\\", paste0(tmp_path,"admin_Union_PPHaSum.shp")), " C:\\tmp\\grid_PPHaSum.tif"))

gridy <- brick("C:/tmp/grid_PPHaSum.tif")


##	Read the first row of the attribute table from our census data to
##		determine the year of our census data:
census_year <- as.data.frame(adminpop[1,"YEARPOP"])[1,1]


##	Calculate population map for census year:
print(paste0("PPHa: Calculating People Per Hectare ", as.character(census_year)))
outPath <- paste0(output_path, country, "_pph_v", rf_version, "_", as.character(census_year), ".tif")
dataset_path <- outPath

if( is.na(file.info(dataset_path)[[2]]) | !skip_existing){
  if (round_counts){
    ##  Kludge - Correction for the extents prior to zonal calculation
    ##  Re-set the extent based on the sometimes slightly different extent
    ##    post-cropping:
    my_extent <- extent(popdensity_weighting_final)
    
    for(ras_set in c(gridp, gridy, popdensity_weighting_final)){
      assign("tmp_raster", raster( ras_set ))
      if (xmin(tmp_raster) > xmin(my_extent)) { my_extent@xmin <- xmin(tmp_raster) }
      if (xmax(tmp_raster) < xmax(my_extent)) { my_extent@xmax <- xmax(tmp_raster) }
      if (ymin(tmp_raster) > ymin(my_extent)) { my_extent@ymin <- ymin(tmp_raster) }
      if (ymax(tmp_raster) < ymax(my_extent)) { my_extent@ymax <- ymax(tmp_raster) }
    }
    ## Crop gridp, gridy, and popdensity_weighting_final:
    gridp <- crop(gridp, my_extent)
    gridy <- crop(gridy, my_extent)
    popdensity_weighting_final <- crop(popdensity_weighting_final, my_extent)
    
    ## Diagnostic for popdensity prj:
    print(paste0("Popdensity PRJ after cropping:"))
    print(popdensity_weighting_final)
    
    ##  FRS: I added this as I believe this is a bug with the raster package.  This
    ##    shouldn't need to be run, and it will *not* fix problems in differing
    ##    rows and columns, if there's ever a problem where the crop() fails to
    ##    produce rasters of identical sizes (e.g. when there are very small diff.
    ##    in cell size for example).  This is a stop-gap hack and hopefully could
    ##    be removed if raster package works correctly in the future:
    for (var_name in c("gridp","gridy", "popdensity_weighting_final")) {
      print(paste("Fixing Extent: ", var_name, sep=""))
      eval(parse(text=paste("extent(", var_name, ") <- my_extent", sep="")))
      flush.console()
    }
    
    popmap <- (gridp * popdensity_weighting_final / gridy) + 0.5 
  } else {    
    ## Kludge - Correction for the extents prior to zonal calculation
    ## Re-set the extent based on the sometimes slightly different extent
    ##    post-cropping:
    my_extent <- extent(popdensity_weighting_final)
    
    for(ras_set in c(gridp, gridy, popdensity_weighting_final)){
      assign("tmp_raster", raster( ras_set ))
      if (xmin(tmp_raster) > xmin(my_extent)) { my_extent@xmin <- xmin(tmp_raster) }
      if (xmax(tmp_raster) < xmax(my_extent)) { my_extent@xmax <- xmax(tmp_raster) }
      if (ymin(tmp_raster) > ymin(my_extent)) { my_extent@ymin <- ymin(tmp_raster) }
      if (ymax(tmp_raster) < ymax(my_extent)) { my_extent@ymax <- ymax(tmp_raster) }
    }
    ## Crop the gridp, gridy, and popdensity_weighting_final:
    gridp <- crop(gridp, my_extent)
    gridy <- crop(gridy, my_extent)
    popdensity_weighting_final <- crop(popdensity_weighting_final, my_extent)
    
    ## Diagnostic for popdensity prj:
    print(paste0("Popdensity PRJ after cropping:"))
    print(popdensity_weighting_final)
    
    ##  FRS: I added this as I believe this is a bug with the raster package.  This
    ##    shouldn't need to be run, and it will *not* fix problems in differing
    ##    rows and columns, if there's ever a problem where the crop() fails to
    ##    produce rasters of identical sizes (e.g. when there are very small diff.
    ##    in cell size for example).  This is a stop-gap hack and hopefully could
    ##    be removed if raster package works correctly in the future:
    for (var_name in c("gridp","gridy", "popdensity_weighting_final")) {
      print(paste("Fixing Extent: ", var_name, sep=""))
      eval(parse(text=paste("extent(", var_name, ") <- my_extent", sep="")))
      flush.console()
    }
    
    popmap <- gridp * popdensity_weighting_final / gridy
  }
  
  #popmap.save(outPath)
  #popmap = NULL
  popmap <- writeRaster(popmap, filename = outPath, method = "GTiff",
                        overwrite = geo_env$overwrite, datatype = "FLT4S", NAflag = -999, 
                        options = c( "COMPRESS=LZW" ) )

  popmap <- NULL
  popmap <- brick(outPath)
}

i = 1
for( popyear in GR_years){
  if (popyear != census_year){
    print(paste0("PPHa: Calculating People Per Hectare ", as.character(popyear)))
    outPath <- paste0( output_path, country, "_pph_v", rf_version, "_", as.character(GR_years[i]), ".tif" )
    dataset_path <- outPath
    
    if (is.na(file.info(dataset_path)[2]) | !skip_existing){
      if (round_counts){
        ##  Fix the extents:
        popmap<- crop(popmap, extent(landcover))
        
        ##  FRS: I added this as I believe this is a bug with the raster package.  This
        ##    shouldn't need to be run, and it will *not* fix problems in differing
        ##    rows and columns, if there's ever a problem where the crop() fails to
        ##    produce rasters of identical sizes (e.g. when there are very small diff.
        ##    in cell size for example).  This is a stop-gap hack and hopefully could
        ##    be removed if raster package works correctly in the future:
        for (var_name in c("popmap")) {
          print(paste("Fixing Extent: ", var_name, sep=""))
          eval(parse(text=paste("extent(", var_name, ") <- extent(landcover)", sep="")))
          flush.console()
        }
        
        popmap_year <-  (popmap * (landcover != 190.0) * GR_rur[i] + popmap * (landcover == 190.0) * GR_urb[i]) + 0.5 
      } else {
        ##  Fix the extents:
        popmap<- crop(popmap, extent(landcover))
        
        ##  FRS: I added this as I believe this is a bug with the raster package.  This
        ##    shouldn't need to be run, and it will *not* fix problems in differing
        ##    rows and columns, if there's ever a problem where the crop() fails to
        ##    produce rasters of identical sizes (e.g. when there are very small diff.
        ##    in cell size for example).  This is a stop-gap hack and hopefully could
        ##    be removed if raster package works correctly in the future:
        for (var_name in c("popmap")) {
          print(paste("Fixing Extent: ", var_name, sep=""))
          eval(parse(text=paste("extent(", var_name, ") <- extent(landcover)", sep="")))
          flush.console()
        }
        popmap_year <- popmap * (landcover != 190.0) * GR_rur[i] + popmap * (landcover == 190.0) * GR_urb[i]
      }
      
      #popmap_year.save(outPath)
      #popmap_year = None
      popmap_year <- writeRaster(popmap_year, filename = outPath, method = "GTiff",
                                 overwrite = geo_env$overwrite, datatype = "FLT4S",  
                                 options = c( "COMPRESS=LZW" ) )
      popmap_year <- NULL
    }
    
    popmap_year <- brick(outPath)
    ##  Diagnostic for the popmap outputs:
    print(paste0("Year of Pop Map:", census_year))
    print(popmap_year)
    
  } else {popmap_year <- popmap}
  
  if (UNADJUST[i]){
    print(paste0("PPHa: Calculating People Per Hectare, UN Adjusted ", as.character(popyear)))
    outPath <- paste0(output_path, country, "_pph_v", rf_version, "_", as.character(GR_years[i]), "_UNadj.tif")
    dataset_path <- outPath
    
    if(is.na(file.info(dataset_path)[[2]]) | !skip_existing){
#       my_extent <- extent(popdensity_weighting_final)
#       
#       for(ras_set in c(gridz, popmap_year)){
#         assign("tmp_raster", raster( ras_set ))
#         if (xmin(tmp_raster) > xmin(my_extent)) { my_extent@xmin <- xmin(tmp_raster) }
#         if (xmax(tmp_raster) < xmax(my_extent)) { my_extent@xmax <- xmax(tmp_raster) }
#         if (ymin(tmp_raster) > ymin(my_extent)) { my_extent@ymin <- ymin(tmp_raster) }
#         if (ymax(tmp_raster) < ymax(my_extent)) { my_extent@ymax <- ymax(tmp_raster) }
#       }
#       ## Crop the gridp, gridy, and popdensity_weighting_final:
#       gridz <- crop(gridz, my_extent)
#       popmap_year <- crop(popmap_year, my_extent)
#       
#       ##  FRS: I added this as I believe this is a bug with the raster package.  This
#       ##    shouldn't need to be run, and it will *not* fix problems in differing
#       ##    rows and columns, if there's ever a problem where the crop() fails to
#       ##    produce rasters of identical sizes (e.g. when there are very small diff.
#       ##    in cell size for example).  This is a stop-gap hack and hopefully could
#       ##    be removed if raster package works correctly in the future:
#       for (var_name in c("gridz","popmap_year")) {
#         print(paste("Fixing Extent: ", var_name, sep=""))
#         eval(parse(text=paste("extent(", var_name, ") <- my_extent", sep="")))
#         flush.console()
#       }
#       zonsum <- zonal(popmap_year, gridz, fun = "sum")
#       
#       ## Kludge: Convert the zonal output to raster
#       colnames(zonsum) <- c("ADMINID", "PPHaSUM")
#       admin_Union_PPHaSum <- merge(admin_Union_prj, as.data.frame(zonsum), by = "ADMINID")
#       
#       writeOGR(admin_Union_PPHaSum, dsn = paste0(tmp_path, "admin_Union_PPHaSum.shp"), layer = "admin_Union_PPHaSum", driver = "ESRI Shapefile", overwrite_layer = TRUE)
#       admin_Union_PPHaSum <- readOGR(dsn = paste0(tmp_path, "admin_Union_PPHaSum.shp"), layer = "admin_Union_PPHaSum")
#       
#       grid_PPHaSum <- writeRaster(temp_ras, "C:/tmp/grid_PPHaSum.tif", format = "GTiff", datatype = "FLT4S", overwrite = geo_env$overwrite, options=c("COMPRESS=LZW"))
#       #system(paste0("gdal_rasterize -l admin_Union_PPHaSum -a PPHaSUM ", gsub("/", "\\\\", paste0(tmp_path,"admin_Union_PPHaSum.shp")), " C:\\tmp\\grid_PPHaSum.tif"))
#       
#       grid_PPHaSum <- brick("C:/tmp/grid_PPHaSum.tif")
#       
#       const <- grid_PPHaSum * 0 + UNPOP[i]
#       
#       
#       ## Kludge: Fix the popmap_year extents
#       popmap_year <- crop(popmap_year, extent(landcover))
#       const <- crop(const, extent(landcover))
#       grid_PPHaSum <- crop(grid_PPHaSum, extent(landcover))
#       
#       for (var_name in c("popmap_year","const", "grid_PPHaSum")){
#         print(paste("Fixing Extent: ", var_name, sep=""))
#         eval(parse(text=paste("extent(", var_name, ") <- extent(landcover)", sep="")))
#         flush.console()
#       }
#       

      if (round_counts){
        #popmap_year_adj <- (popmap_year * (const / grid_PPHaSum)) + 0.5         
        popmap_year_adj <- (popmap_year * (UNPOP[i] / sum(popmap_year[], na.rm=T))) + 0.5         
      } else {
        #popmap_year_adj <- popmap_year * (const / grid_PPHaSum)
        popmap_year_adj <- (popmap_year * (UNPOP[i] / sum(popmap_year[], na.rm=T)))
        
        #popmap_year_adj.save(outPath)
        #popmap_year_adj = None
        popmap_year_adj <- writeRaster(popmap_year_adj, filename = outPath, method = "GTiff",
                                       overwrite = geo_env$overwrite, datatype = "FLT4S", 
                                       options = c( "COMPRESS=LZW" ) )
        popmap_year_adj <- NULL
      }
    }
    
    popmap_year_adj <- brick(outPath)
    
    ##  Diagnostic for Popmap:
    print(paste0("Popmap ADJ year: ", census_year))
    print(popmap_year_adj)
  }
  
  i <- i + 1
}

print("PPHa: Completed!")

print("COMPLETED:  Succesffully created population map outputs!")

##  Remove the temporary files to clear up disk space:
removeTmpFiles(h=1)
## End the cluster
endCluster()

##	END: Population map creation
#####