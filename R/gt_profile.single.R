# =============================================================================
#'
#' @title Plot of ground temperature (GT) profile of a location for the available years in a period
#'
#'
#' @description Give one data.frame of a location or a list of data.frames of multiple 
#'              locations and the function returns a plot containing the minimum, maximum and mean annual GT.
#'              The output is a pdf-file, saved in a own defined output directory.
#'
#'
#' @details 
#'
#' @param obs_gt   one dataframe of one location or a list of dataframes of multiple locations.
#'                 A dataframe needs at least the following columns:
#'                 Location name as loc_name;
#'                 Ground temperature record with hourly interval (column head as the depth)
#'                 Date as time (prefered to be in POSIXct and the following format: YYYY-MM-DD HH:MM:SS)
#' 
#' @param out.path path to directory where the plot should be saved. e.g: "~/Desktop/"
#' 
#' @param n.col number of columns of plots in the pdf file
#' 
#' @param n.row number of rows of plots in the pdf file          
#'
#'
#' @return Pdf-file saved in R directory. Plot contains:
#'         min-, max-, mean- annual temperatures as well as the zero temperature line.
#'           
#' 
#' @export 
#' @examples
#' gt_profile(df, "~/Desktop/", n.col=  2, n.row=2)

#' @author Fereshteh Ghiami Shomami <fereshteh.ghiami.sh@@gmail.com>
# =============================================================================

#plot function for one site
gt_profile.single <- function (obs_gt, out.path, n.col, n.row) {
  
  #load required packages
  require(pacman)
  require (ggplot2)
  p_load(lubridate, dplyr, tidyr)
  
  #change the time format 
  obs_gt <- obs_gt %>%
    mutate(time = as.POSIXct(time, format = "%d/%m/%Y %H:%M"))
  
  #extract days and years with sufficient records
  day_cnt <- obs_gt %>%
    mutate(day = floor_date(time, "day")) %>% group_by(day) %>% dplyr::count(day)
  
  full_d <- day_cnt %>% filter (n >= 8)
  
  year_cnt <- day_cnt %>%
    mutate(year = floor_date(day, "year")) %>% group_by(year) %>% dplyr::count(year)
  
  full_y <-year_cnt %>% filter (n >= 365)
  
  # Restrict observations to the full annual record&calculate daily max, min and mean
  daily_gt_compt <-
    obs_gt %>% subset (year(obs_gt$time) %in% year(full_y$year)) %>% 
    gather(Depth, value = tem,-(c("time","loc_name"))) %>%
    mutate(day = floor_date(time, "day")) %>% 
    group_by(day, Depth, loc_name) %>% 
    dplyr::summarise(
      tem_avg = mean(tem),
      tem_min = min (tem),
      tem_max = max(tem),
      cnt = n()
    )
  
  # Restrict observations to the full daily record&calculate Annual max, min and mean
  Ann_gt_compt <- daily_gt_compt%>%filter(day %in% (full_d$day)) %>%
    mutate(year = (strftime(day, "%Y")) )%>%
    group_by(year, Depth) %>%
    dplyr::summarise(
      Tmean = mean(tem_avg),
      Tmin = min(tem_min),
      Tmax = max(tem_max)
    ) 
  
 
  
  #plot the gt profile
  myplot <- ggplot(
    Ann_gt_compt %>% mutate_if(is.character, as.numeric) %>%tidyr::gather(Temperature, value,-(c("Depth", "year"))),
    aes(Depth, value, colour = Temperature)
  ) +
    geom_point(size = 1) +
    geom_line(size = 0.5) +
    ylab(expression('Temperature (' *  ~ degree * C * ')')) +
    xlab("Depth (m)") +
    scale_y_continuous(expand = expansion(c(0, 0)), position =
                         "right") +
    scale_x_reverse(expand = expansion(c(0, 0))) +
    coord_flip() +
    geom_vline(
      xintercept = c(0, 0),
      linetype = "solid",
      size = 1
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dotted",
      size = 1
    ) +
    labs(title = paste(
      "Ground thermal regime", "(", obs_gt$loc_name, ")", sep = " "
    )) +
    theme(
      axis.title.x = element_text(size = 10, colour = "black"),
      axis.title.y = element_text(
        size = 10,
        colour = "black",
        vjust = 2
      ),
      axis.text.x = element_text(
        size = 8,
        face = "bold",
        colour = "black",
        angle = 0,
        hjust = -0.2
      ),
      axis.text.y = element_text(
        size = 8,
        face = "bold",
        colour = "black"
      ),
      panel.background = element_blank(),
      panel.grid.minor.y = element_line(size = 3),
      panel.grid.major = element_line(colour = "gray"),
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        size = 1
      ),
      plot.margin = unit(c(1, 1, 1.5, 1.2), "cm"),
      legend.box = "horizontal",
      legend.position = "bottom",
      plot.title.position = 'plot',
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_colour_manual(values = c(
      "Tmean" = "black",
      "Tmax" = "red",
      "Tmin" = "blue"
    )) 
  
  #export as pdf file  
  mypath <- file.path(out.path,paste(df$loc_name[1],".pdf",sep="") )
  pdf(file=mypath)
  pags<-ceiling((length(full_y$year))/(n.col*n.row))    
  for (i in 1:pags) {
    print(myplot +
            ggforce::facet_wrap_paginate(
              ~ year,
              ncol = n.col,
              nrow = n.row,
              page = i
            ))
  }
  dev.off()
  
}

    

  
  
  
  
 
  
  





