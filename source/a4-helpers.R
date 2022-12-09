library(tidyverse)

# Data access ----
#---------------------------------------------------------------------------#
# This function returns the incarceration data as a dataframe
# Note: The CSV file is stored on my local machine to speed load times
#---------------------------------------------------------------------------#
get_data <- function(num_records=-1) {
  df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
  return(df)
}

incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
# Processing places ----
# NOTE: For these functions to work, the dataframe `incarceration_df` must 
#       be initialized
#----------------------------------------------------------------------------#
# Return the list of states in a region.  The regions are: 
#    Midwest, Northeast, South, West
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# Return the list of divisions in a region. The regions are: 
# Midwest, Northeast, South, West
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Return the list of states in a region.  The divisions are: 
#    East North Central
#    East South Central
#    Middle Atlantic
#    Mountain
#    New England
#    Pacific
#    South Atlantic
#    West North Central
#    West South Central
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
# Returns TRUE if the place is a state 
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# Returns a list of states for region or division. If place is a state
# just return it.


# Useful queries ----
#----------------------------------------------------------------------------#
# Do some states have no jail populations? If so, which states?
#----------------------------------------------------------------------------#

# Basic info ----
#----------------------------------------------------------------------------#
# Format some region information (currently, only the divisions)
#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
# Show some basic information about the dataset
#----------------------------------------------------------------------------#
get_basic_info <- function(df) {
  t <- "Basic Info on `incarceration_trends`\n"
  t <- paste0(t, "  No. regions               ", length(unique(df$region)), "\n")
  t <- paste0(t, "  No. divisions             ", length(unique(df$division)), "\n")
  t <- paste0(t, "  No. states                ", length(unique(df$state)), "\n")
  t <- paste0(t, "  No. counties              ", length(unique(df$county_name)), "\n")
  t <- paste0(t, "  No. fips (county IDs)     ", length(unique(df$fips)), "\n")
  t <- paste0(t, "  No. different urbanicity  ", length(unique(df$urbanicity)), "\n")
  
  t <- paste0(t, "Divisions ...\n")
  t <- paste0(t, format_region_info("Midwest"), "\n")
  t <- paste0(t, format_region_info("Northeast"), "\n")
  t <- paste0(t, format_region_info("South"), "\n")
  t <- paste0(t, format_region_info("West"), "\n")
  
  cat(t) 
}

# Main ----
#----------------------------------------------------------------------------#
# Basic tests of the helper functions
# Comment or uncomment 
#----------------------------------------------------------------------------#
# ## Very important: You must initialize `incarceration_df`
incarceration_df <- get_data()
# 
# ## Demonstrate use of the functions
# ## Each of these functions returns a vector of states
# states_in_region("South")
# states_in_division("Pacific")
# states_in_region_or_division("South")        
# states_in_region_or_division("Mountain")  
# states_in_region_or_division("OR")          # returns c("OR")
# 
# ## Returns the divisions that makeup a region 
# divisions_in_region("West")
# 
# ## Returns basic information about the dataset
# get_basic_info(incarceration_df)
# 
# ## 
# states_with_no_jail_pop()
