# =============================================================================
#'
#' @title reformat output of GEOtop for further analyziz
#'
#'
#' @description Give output of GEOtop as one data.frame of a location and the function 
#'              reformat it and returns a  file containing time-, loc_name-, Depth- and tem.
#'              The output is a csv-file, saved in a own defined output directory.
#'
#'
#' @details 
#'
#' @param df       one dataframe of one location .
#'                 A dataframe needs at least the following columns:
#'                 Location name as loc_name;
#'                 Ground temperature record with hourly interval (column head as the depth)
#'                 Date as time (prefered to be in POSIXct and the following format: YYYY-MM-DD HH:MM:SS)
#' 
#' @param out.path path to directory where the csv file should be saved. e.g: "~/Desktop/"
#' 
#'
#'
#' @return a csv file saved in a own defined output directory. file contains:
#'         time-, loc_name-, Depth- and tem.
#'           
#' 
#' @export 
#' @examples
#' GEOtop_adj(df, "~/Desktop/")

#' @author Fereshteh Ghiami Shomami <fereshteh.ghiami.sh@@gmail.com>

# =============================================================================

#plot function for one site
GEOtop_adj <- function(df, out.path) {
  #load required packages
  require(pacman)
  p_load(dplyr, tidyr)
  dfname<-deparse(substitute(df))
  #Remove useless columns and row
  cols <-
    as.numeric((stringr::str_extract(colnames(df[, c(5:33)]), "[[:digit:]]+"))) /
    1000
  
  #Remove useless columns and rows
  df %>% slice(-1, ) %>% select (-c(2:4)) %>%
    `colnames<-` (c ("time", cols)) %>%
    
    #Reformat time & add location name column (takes from the df name)
    
    dplyr::mutate(
      time =  as.POSIXct(time, format = "%d/%m/%Y %H:%M"),
      loc_name = dfname
    ) %>%
    
    #Reformat data frame from wide to long
    tidyr::gather(Depth, value = tem, -(c("time", "loc_name")))%>%
  
    # write it to the csv file
    write.table(file=file.path("~/Desktop/",paste(dfname,".csv")),sep = ",", row.names = FALSE)
}







