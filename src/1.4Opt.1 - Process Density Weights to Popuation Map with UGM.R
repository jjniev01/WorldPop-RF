#####
## Purpose of script: Project final population map (popmap) to 2010 and
##    2015 and 2020 estimates based on the 2011 Revision of World Urbanization
##		Prospects (http://esa.un.org/unpd/wup/index.htm) and also adjusted
##		to U.N. total population numbers for each country and the Urban Growth 
##    Model  developed by Linard et al. 2013.

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


##  !NOTE!  
##  1.4 (THE REGULAR SCRIPT) MUST BE RUN PRIOR TO RUNNING THIS OPTIONAL SCRIPT
#####



#####
##	BEGIN:	Set per-country configuration options

##	Parse main configuration file, which will set the country and root_path
##		variables:
source("01.0 - Configuration.py.r")


##	Round to whole population counts?
round_counts <- FALSE

##	Integrate Urban dataset:
##		NOTE: If you set this to True then map production will use the
##			Raster representation of whatever you've used as the Urban data
##			layer and "burn" the urban class into the land cover before
##			adjusting population counts per pixel:
use_urban <- TRUE

##  Utilize the Urban Growth Model in population predictions?
use_ugm <- TRUE

##  Declare the UGM constants utilized:
##  Estimated annual decrease in pop. density 
##  (0.01 is the medium scenario from Angel et al.)
densdr <- 0.01

##  Number of years to model (i.e. start year is 2010 and end year is 2020):
##    NOTE:  Model is currently parameterized and limited to a 10 year 
##           difference
n_years <- 10

##	TODO: Eventually this can be set in the Metadata.r file and pulled
##		via JSON but for now we will set it here, you must make
##		sure it matches the versioning in the Metadata.r file:

##	Country specific population-specific variables:
##		Growth rates are estimated for urban and rural areas for the years
##		included in GR_years. (file: growth_rates.xlsx)
##
##	!!!CAUTION!!!: if we use more recent census data (see the
##			census_folder option below), it must be the rate from the more
##			recent census data to	2010 (i.e. from 2009 to 2010 for Vietnam) -
##			so calculation, for	VNM, is for one year:
##  These are for Uganda (input census data is for 2002):
GR_years <- c(2010, 2015, 2020)
GR_urb <- c(1.784610171, 1.379195006, 1.322468413)
GR_rur <- c(1.124849442, 1.073565841, 1.061019831)

# GR_years <- c(2010, 2015, 2020)
# GR_urb <- c(0.9690879, 5.43, 5.3)
# GR_rur <- c(0.9927266, 2.95, 2.76)
#GR_years <- C(2000)
#GR_urb <- c(0.861741446)
#GR_rur <- c(0.97488919]67345071)

##	Processing flags:
##	Set UNADJUST to True if we want to produce a map adjusted to UN totals
##		for 2010, False otherwise:
UNADJUST <- c(TRUE, TRUE, TRUE)
#UNADJUST <- c(TRUE)

## If UNADJUST == True then we need to provide the UN total population for
##		that year - needed if you want to adjust map for U.N. esimates.
##		U.N. estimates are from the World Urbanization Prospects
##		(http://esa.un.org/unpd/wup/index.htm)
##  These are for Uganda (input census data is for 2002):
UNPOP <- c(10837000, 12482000, 14123000)
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

##	END: Define utility functions
#####

#####
##	BEGIN: Data pre-processing for needed datasets

##	Get a character vector of all existing folders in the country's data folder:
dataset_folders <- list.files(path = data_path, full.names = FALSE)

##	Datasets:
##	  Population redistribution maps produced from 1.4, specifically the 
##    population maps for 2010 (both ppp and pph, UN and non-UN adjusted) (i.e.
##    there will be 4 maps total brought in 2010 ppp, 2010 UN ppp, 2010 pph, and
##    2010 UN pph):
dataset_paths <- Sys.glob( paste0( output_path, country, "_pp*2010*.tif" ))
if(dataset_paths == ""){
  print("ERROR:  No \"population map\" TIFs | IMGs found in the output folder!  You first need to run the 1.3 R script!")
  stop()
}

##	Set the workspace for data creation and processing to the country's output 
##  folder:
setwd(ensure_dir(output_path))

##	END: Data pre-processing for needed datasets
#####



######
##  BEGIN:  CREATION OF PREDICTED POPULATION USING THE UGM
##
##  If the option to utilize the UGM is true:
if(use_uGM){
  print("Beginning calculation of predicted population using the Urban Growth Model")
  ##  NOTE: The probability raster output from 1.3.Opt1, i.e. prob, is utilized 
  ##  here under the same variable name. Therefore, 1.3.Opt.1 MUST BE RUN prior 
  ##  while still in the same R session:
 
  ##  For each population map:
  for(pmap in dataset_paths){
    ##  Bring in the population map:
    popmap <- brick(pmap)
    
    ##  Load the corresponding UGM land cover raster (or the urban land cover raster)
    ##  as well:
    landcover_ugm <- LC2
    
    ##  See if the given map is projected (i.e. pph) or not:
    prj_map <- grepl(".*pph.*", pmap)
    
    ##  See if the given map is supposed to be UN adjusted:
    un_adj <- grepl(".*UNadj.*", pmap)
    
    ##  If the pop map is projected:
    if (prj_map){
      ##  Project the landcover raster to match:
      landcover_ugm <- projectRaster(landcover_ugm, popmap)
    }
    
    ##  Get the total URBAN population of the 2010 raster (e.g. the sum of pop 
    ##  where the landcover is urban/built):
    pop2010 <- sum(popmap[which( values(landcover_ugm) == 1 )])
    
    ##  Calculate the estimated 2020 URBAN population:
    pop_2020 <- pop2010 * (exp( (GR_urb[2]/100) )) * (exp( (GR_urb[3]/100) ))
    
    ##  Get the average URBAN population density for 2010 from the pop. den. raster where 
    ##    it is spatially coincident with land cover matching urban:
    urb_dens_10 <- mean(pop2010[which( values(landcover_ugm) == 1 )])
    
    ##  Use that average urban pop. dens. for 2010 and the total pop for 2010 to
    ##  calculate the population density threshold to be used in determining new 
    ##  urban growth from 2010 to 2015:
    ##  Calculate the urban pop. dens. in 2020:
    urb_dens_20 <- urb_dens_10 - (urb_dens_10 * densdr * n_years)
    
    ##  Calculate the estimated urban pop.area in 2020 (i.e. # of pixels):
    urb_pop_20 <- pop_2020/urb_dens_20
    
    ##  Calculate the area (i.e. number of pixels) that will have new urban 
    ##  growth:
    urb_grow_area <- urb_pop_20 - length( which( values( landcover_ugm ) == 1 ) )
    
    ##  Give the treshold value of probability where cells are converted to 
    ##  urban:
    threshold.value <- sort(prob[], TRUE)[urb_grow_area + 1]
    
    ##  NOTE: We are assuming that half of the predicted growth occurs between 
    ##        2010 to 2015 and the 2nd half of the predicted growth occurs 
    ##        between 2015 and 2020. Therefore we are taking half the cells with 
    ##        the highest probability and converting them before applying pop 
    ##        growth rates and then doing the same for the second half to arrive 
    ##        at 2020 predictions.
    
    ##  Select all cells that are above that threshold:
    ##  NOTE:  This will be our 2020 landcover.
    new_growth10_20 <- which( values( prob ) > threshold.value )
    landcover_ugm_20 <- landcover_ugm
    landcover_ugm_20[new_growth10_20] <- 1
    
    ##  Create the landcover containing the urban growth "seen" between 2010 and
    ##  2015:
    ##  Select the median value of all the probabilities above the threshold:
    med_thresh <- median(values(prob) > threshold.value)
    new_growth10_15 <- which(values(prob) > med_thresh) 
    landcover_ugm_15 <- landcover_ugm
    landcover_ugm_15[new_growth10_15] <- 1
    
    
    ##  Write the 2015 and 2020 landcovers with the urban extents to the output 
    ##  folder: 
    writeRaster(landcover_ugm_15, 
                filename = paste0(tmp_path, country,"_lc_2015", if(prj_map){"_prj"},".tif"),
                method = "GTiff", overwrite = TRUE, datatype = "INT2U", 
                NAflag = 65535, options = c("COMPRESS=LZW"))
    
    writeRaster(landcover_ugm_20, 
                filename = paste0(tmp_path, country,"_lc_2020", if(prj_map){"_prj"},".tif"),
                method = "GTiff", overwrite = TRUE, datatype = "INT2U", 
                NAflag = 65535, options = c("COMPRESS=LZW"))
    
    ##  Apply urban and rural growth rates for 5 years of change to the 2010 pop.
    ##  dens. raster respective to the landcover_ugm for 2015:
    popmap_15 <- popmap
    popmap_15[which(values(landcover_ugm_15)==1)] <- popmap_15[which(values(landcover_ugm_15)==1)] * GR_urb[2]
    popmap_15[which(values(landcover_ugm_15)!=1)] <- popmap_15[which(values(landcover_ugm_15)!=1)] * GR_rur[2]
      
    ##  Write that raster to the TMP folder:
    write.raster(popmap_15, filename = paste0(tmp_path, country, if(un_adj){"_pph_v"}else{"_ppp_v"}, rf_version, "_2015_ugm.tif"),
                 method = "GTiff", overwrite = TRUE, datatype = "INT2U", 
                 NAflag = 65535, options = c("COMPRESS=LZW"))
    
    
    ##  Apply urban and rural growth rates for 5 years of change to the previously
    ##  calculated 2015 estimated population density:
    popmap_20 <- popmap_15
    popmap_20[which(values(landcover_ugm_20)==1)] <- popmap_20[which(values(landcover_ugm_20)==1)] * GR_urb[3]
    popmap_20[which(values(landcover_ugm_20)!=1)] <- popmap_20[which(values(landcover_ugm_20)!=1)] * GR_rur[3]
    
    ##  If the map is supposed to be UN adjusted:
    if (un_adj){
      ##  If the map is projected:
      if(prj_map){
        ##  Bring in the PPHa grid sum:
        grid_PPHaSum <- brick("C:/tmp/grid_PPHaSum.tif")
        
        ##  Calculate the constant:
        const <- grid_PPHaSum * 0 + UNPOP[3]
        
        ## Kludge: Fix the popmap_year extents
        popmap_20 <- crop(popmap_20, extent(landcover_ugm_20))
        const <- crop(const, extent(landcover_ugm_20))
        grid_PPHaSum <- crop(grid_PPHaSum, extent(landcover_ugm_20))
        
        for (var_name in c("popmap_20","const", "grid_PPHaSum")){
          print(paste("Fixing Extent: ", var_name, sep=""))
          eval(parse(text=paste("extent(", var_name, ") <- extent(landcover_ugm_20)", sep="")))
          flush.console()
        }
              
        if (round_counts){
          popmap_20_adj <- (popmap_20 * (const / grid_PPHaSum)) + 0.5         
        } else {
          popmap_20_adj <- popmap_20 * (const / grid_PPHaSum)
        }
      } else{
      ##  Bring in the grid PPPSum
      grid_PPPsum <- brick("C:/tmp/grid_PPPSum.tif")
      
      ##  Calculate the constant:
      const <- grid_PPPSum * 0 + UNPOP[3]
      
      ##  Adjust the population raster to the UN adjusted values:
      ##  Correct the extents of grid_PPPSum and const:
      ## Crop the popmap_year and gridz extents:
      grid_PPPSum <- crop(grid_PPPSum, extent(landcover_ugm_20))
      const <- crop(const, extent(landcover_ugm_20))
      
      ##  FRS: I added this as I believe this is a bug with the raster package.  This
      ##    shouldn't need to be run, and it will *not* fix problems in differing
      ##    rows and columns, if there's ever a problem where the crop() fails to
      ##    produce rasters of identical sizes (e.g. when there are very small diff.
      ##    in cell size for example).  This is a stop-gap hack and hopefully could
      ##    be removed if raster package works correctly in the future:
      for (var_name in c("grid_PPPSum", "const")) {
        print(paste("Fixing Extent: ", var_name, sep=""))
        eval(parse(text=paste("extent(", var_name, ") <- extent(landcover_ugm_20)", sep="")))
        flush.console()
      }
      
      if (round_counts){        
        popmap_20_adj <- (popmap_20 * (const / grid_PPPSum)) + 0.5 
      } else {
        popmap_20_adj <- popmap_20 * (const / grid_PPPSum)
      }
      
      }
      ##  Write this resulting raster to the output to obtain our 2020 predicted 
      ##  population density map:
      writeRaster(popmap_20_adj, filename = paste0(outpath, country, if(prj_map){"_pph_v"}else{"_ppp_v"}, rf_version, "_2020_UNadj.tif"),
                  method = "GTiff", overwrite = TRUE, datatype = "INT2U", 
                  NAflag = 65535, options = c("COMPRESS=LZW"))
       
    } else{
      ##  Write the non-UN adjusted popualtion map:
      writeRaster(popmap_20, filename = paste0(outpath, country, if(prj_map){"_pph_v"}else{"_ppp_v"}, rf_version, "_2020.tif")filename = paste0(outpath, country, if(prj_map){"_pph_v"}else{"_ppp_v"}, rf_version, "_2020_UNadj.tif"),
                  method = "GTiff", overwrite = TRUE, datatype = "INT2U", 
                  NAflag = 65535, options = c("COMPRESS=LZW"))
    }
  } 
  ##  END:  CREATION OF PREDICTED POPULATION USING THE UGM
  ######
} else{
  print("Use UGM option is currently OFF. Are you sure you are wanting to run
          1.4.Opt.1? If so, adjust the option in 1.0 - Configuration.py.r")
}

##  End the cluster:
endCluster()