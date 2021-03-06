##	This is the primary per-configuration file, parsed by each script,
##		in this folder, including both R and Python scripts.  There should
##		be fewer per-script configuration items now as these will control
##		the country-name and root path (path to the folder containing the RF
##		/src, /data, /output, etc. folders.

##	NOTE: Make sure that you don't use the R assignment operator <- as
##		this file is also parsed by Python, so use = instead:


##	Configure the country abbreviation and name:
country = "TZA"

##  Configure the UG model option:
use_UGM = 0
region = "S" 

##(N, S, W, C or MAD) #this will be needed for the travel time 
##calculation as the "cost" file ("costtt.tif") has been divided 
##by region to save space. 

##	The version of the scripts used to produce the mapping products, and
##		which will match the "_v" portion of the filename outputs:
rf_version = "2c"

##	This should be set to the folder *containing* the "RF" folder structure,
##		but make sure that you use forward slashes instead of back slashes or
##		double back slashes:
root_path = "D:/Users/jnieves/Research/Population/Data/RF"

