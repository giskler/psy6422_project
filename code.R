##Restore dependencies with renv --------------------------------------------------------------------------------------------------------------------
if (!require("renv")) {
  install.packages("renv");
}

library(renv)
renv::restore()

##Load required libraries --------------------------------------------------------------------------------------------------------------------------

library(circlize)
library(ComplexHeatmap)
library(cowplot)
library(data.table)
library(dplyr)
library(janitor)
library(lubridate)
library(plyr)
library(purrr)
library(readxl)
library(spiralize)
library(tidyverse)

##Read and prepare data ----------------------------------------------------------------------------------------------------------------------------

#Create a list of train operator codes (TOC) for passenger train operators to filter out the relevant observations
tocs <- c("EA","EB", "EC", "ED", "EE", "EF", "EH", "EJ", "EK", "EM", "ES", "ET", "EX", "HA", "HB", "HE", "HF", "HL", 
          "HM", "HO", "HT", "HU", "HX", "HY", "LD", "LN", "PF", "XB", "XC", "XE")

#Create a list of column names that need to be renamed. Most of these columns ended up not being used in the final product of this project, 
#but are included to allow for different types of analyses or graphs in any future adaptations of this code
rename_cols = c(toc_code = "operator_affected", event_type = "performance_event_code", react_reason = "reactionary_reason_code", stanox = "start_stanox")

#Read file names in the raw data folder
csvfiles <- c(list.files(path= "raw", pattern="\\.csv$", full.names=TRUE))

#Create a list to populate with data frames
train_frame <- list()

#Use a loop to read the files, this may take some time depending on the amount of files being read. At the time this project was completed
#a total of 124 files covering the time period between 2015-04-01 to 2024-07-20 were used
for(i in 1:length(csvfiles)) {
  
  #Create a csv reader function
  train_reader <- function(filelist) { 
    
    plyr::ldply(filelist, fread) %>% #Read the files using fread
      clean_names() %>% #Convert column names to snake case using janitor
      dplyr::rename(any_of(rename_cols)) %>% #Rename columns
      filter(event_type != "M" & event_type != "F" & event_type != "A" & 
               event_type != "O" & event_type != "S") %>% #Drop observations that contain delay data and planned cancellations
      filter(toc_code %in% tocs) %>% #Filter by passenger train operators
      mutate(departure_date = parse_date_time(origin_departure_date, c("dmy", "%d/%m/%Y %H%M")), 
             .before = origin_departure_date) %>% #Convert the date format using lubridate
      mutate(departure_date = as.Date(departure_date)) %>% #Change data type to date
      select(departure_date, toc_code, event_type, react_reason, stanox) #Keep only the columns we are interested
  } 
  
  train_frame[[i]] <- train_reader(csvfiles[i])
}

#Merge the data frames
master_df <- train_frame %>% reduce(full_join) #Reduce the large list to a single data frame using purrr

#Create an assert function for sanity checks
assert <- function(value, target) {
  #This function crashes the code if value does not equal target
  if (value == target) {
    print("Value equals target")
  } else {
    stop("Value does not equal target")
  }
}

#Count number of days between first and last observation
date_diff <- as.numeric(difftime(max(master_df$departure_date), min(master_df$departure_date), units = "days"))

#Sanity check
assert(length(unique(master_df$departure_date)), date_diff)

#Create a train operator data frame
operators <- data.frame(toc_code = c("EA","EB", "EC", "ED", "EE", "EF", "EH", "EJ", "EK", "EM", "ES", "ET", "EX", "HA", "HB", "HE", "HF", "HL", 
                                     "HM", "HO", "HT", "HU", "HX", "HY", "LD", "LN", "PF", "XB", "XC", "XE"),
                        operator = c("TransPennine Express", "Greater Anglia", "Grand Central", "Northern", "Heathrow Connect",
                                     "Great Western Railway", "CrossCountry", "West Midlands Railway", "London Overground", 
                                     "East Midlands Railway", "Caledonian Sleeper", "Govia Thameslink Railway", "Elizabeth line", "ScotRail", "LNER", 
                                     "Merseyrail", "Avanti West Coast", "Transport for Wales", "Heathrow Express", "Chiltern Railways", "c2c", 
                                     "Southeastern", "Thameslink", "South Western Railway", "Lumo", "London Northwestern Railway", 
                                     "Hull Trains", "LUL District Line - Wimbledon", "LUL Bakerloo Line", "LUL District Line - Richmond"))

#Merge train operator data frame with the master data frame
master_df <- master_df %>% left_join(operators, 
                                     by=c("toc_code"))

#Read description for cancellation codes
react_codes <- read_excel("data/glossary.xlsx", sheet="Reactionary Reason Code") %>%
  clean_names() %>% #Convert column names to snake case
  select(-3) %>% #Drop the reason name column
  dplyr::rename(react_reason = reactionary_reasons_reactionary_reason_code) #Rename columns

#Merge cancellation codes data frame with the master data frame
master_df <- master_df %>% left_join(react_codes, 
                                     by=c("react_reason"))

#Read description for event types
event_types <- read_excel("data/glossary.xlsx", sheet="Performance Event Code") %>%
  clean_names() %>% #Convert column names to snake case
  select(-2) %>% #Drop the performance event column
  dplyr::rename(event_type = performance_event_types_performance_event_code) #Rename columns

#Merge event types data frame with the master data frame
master_df <- master_df %>% left_join(event_types, 
                                     by=c("event_type"))

#Rearrange columns for readability
master_df <- master_df[,c(1,3,8,4,7,2,6,5)]

#Save master data frame as a csv file and compress it
write.csv(master_df, file=gzfile("data/masterfile.csv.gz"))

##Spiral graph -------------------------------------------------------------------------------------------------------------------------------------

#Count the number of cancellations by date
train_count <- master_df %>% group_by(departure_date) %>% tally()

#Read weather data
weather <- fread("data/weather.csv") %>%
  dplyr::rename(departure_date = date) %>% #Rename columns
  mutate(departure_date = as.Date(departure_date)) %>% #Change data type to date
  mutate(type = as.factor(type)) #Turn weather types into a factor, this is necessary in order to populate the weather track

#Merge data frames
train_count <- train_count %>% left_join(weather, 
                                         by=c("departure_date"))

#Sanity check, number of cancellations should equal number of observations in master data frame
assert(nrow(master_df), sum(train_count$n))

#Create a spiral graph function that requires the input of a data frame and a starting and ending date in the format "YYYY-MM-DD"
#Refer to the documentation for the spiralize library for further details on its functions
spiral_plot <- function(df, start, end){
  
  #We'll use a temporary data frame to store the filtered date range
  temp_df <- df %>% 
    filter(departure_date > start & departure_date < end) 
  
  #Initialize the spiral graph. As the spiral is 360 degrees, we have a choice to either normalize each year to 360 or to plot
  #each year as 365/366 days. Both options have their drawbacks and advantages
  spiral_initialize_by_time(xlim = range(temp_df$departure_date), verbose = FALSE, normalize_year = FALSE)
 
  #Load train track. We set the range of the y axis to be slightly higher than the maximum value
  spiral_track(height =  0.8, background = FALSE, ylim = c(0, 1.05*max(temp_df$n)))
  
  #Draw backgrounds for the breakpoints. TRACK_META reads the meta data of the current track
  bg_col = c("#F8F8F8", "#F0F0F0", "#E8E8E8", "#E0E0E0") 
  for(i in 1:4) {
    spiral_rect(TRACK_META$xlim[1], TRACK_META$ylim[1] + TRACK_META$yrange*(i-1)/4, 
                TRACK_META$xlim[2], TRACK_META$ylim[1] + TRACK_META$yrange*i/4, 
                gp = gpar(fill = bg_col[i], col = NA))
  } 
  
  #Draw bar plot
  spiral_bars(temp_df$departure_date, temp_df$n, gp = gpar(fill = 4, col = 4)) 
  
  #Create unit labels
  max = TRACK_META$ymax #Read the maximum value of the y axis
  at = grid.pretty(c(0, max)) #Set the start point as 0 and the end point as max
  at = at[at <= max ] #Create breakpoints between 0 and max
  labels = as.character(at) #Place the labels along the y axis
  labels[at >= 1000 & at < 1000000] = paste0(at[at >= 1000 & at < 1000000]/1000, "K") #Add a K label to indicate thousands
  spiral_yaxis(at = at, labels = labels, labels_gp = gpar(fontsize = 5)) #Draw it at the beginning and end of the spiral
  
  #Create month labels
  dd = max(temp_df$departure_date) #Read the final date in the range
  day(dd) = 15 #Set the day to the 15th as a midpoint
  dd = dd + months(1:12) #Add the months
  spiral_text(dd, y = 1.5, month.name[month(dd)], facing = "inside", nice_facing = TRUE) #Draw the labels
  
  #Add year labels
  years = as.character(unique(year(temp_df$departure_date))) #Create a vector with the years in the date range
  
  for (i in 1:length(years)){
    spiral_text(sprintf("%s-01-01", years[i]), TRACK_META$ycenter, years[i], gp = gpar(fontsize = 8)) #Place a year label on January 1st of each year
  } 

  #Create the title
  grid.text(sprintf("Train cancellations in Great Britain, %s-%s", first(years),last(years)), x = unit(0, "npc") + unit(0, "mm"), y = unit(1, "npc") - unit(0, "mm"),
            gp = gpar(fontsize = 14)) 
  
  #Add source
  grid.text("Source: NWR Historic Delay Attribution licensed under OGL v3.0.", x = unit(0, "npc") + unit(0, "mm"), y = unit(0, "npc") - unit(0, "mm"),
            gp = gpar(fontsize = 8)) 
}

#Draw the plots
spiral_plot(train_count, "2015-04-01", "2017-12-31")
spiral_plot(train_count, "2018-01-01", "2020-12-31")
spiral_plot(train_count, "2021-01-01", "2024-10-12")

#Save the plots. We're not going to use ggsave as the quality of the saved file is noticeably worse using ggsave
svg("figs/2015_2017.svg", width = 10, height = 6)
spiral_plot(train_count, "2015-04-01", "2017-12-31")
invisible(dev.off())

svg("figs/2018_2020.svg", width = 10, height = 6)
spiral_plot(train_count, "2018-01-01", "2020-12-31")
invisible(dev.off())

svg("figs/2021_2024.svg", width = 10, height = 6)
spiral_plot(train_count, "2021-01-01", "2024-10-12")
invisible(dev.off())

##Draw a grid of all years
grid_plot <- function(df, start, end){  
  
  y_maximum = max(df$n)
  
  #We'll use a temporary data frame to store the filtered date range
  temp_df <- train_count %>% 
    filter(departure_date > start & departure_date < end) 
  
  #Initialize the spiral graph. As the spiral is 360 degrees, we have a choice to either normalize each year to 360 or to plot
  #each year as 365/366 days. Both options have their drawbacks and advantages
  spiral_initialize_by_time(xlim = range(temp_df$departure_date), verbose = FALSE, normalize_year = FALSE)
 
  #Load train track. We set the range of the y axis to be slightly higher than the maximum value
  spiral_track(height =  0.8, background = FALSE, ylim = c(0, 1.05*max(y_maximum)))
  
  #Draw backgrounds for the breakpoints. TRACK_META reads the meta data of the current track
  bg_col = c("#F8F8F8", "#F0F0F0", "#E8E8E8", "#E0E0E0") 
  for(i in 1:4) {
    spiral_rect(TRACK_META$xlim[1], TRACK_META$ylim[1] + TRACK_META$yrange*(i-1)/4, 
                TRACK_META$xlim[2], TRACK_META$ylim[1] + TRACK_META$yrange*i/4, 
                gp = gpar(fill = bg_col[i], col = NA))
  } 
  
  #Draw bar plot
  spiral_bars(temp_df$departure_date, temp_df$n, gp = gpar(fill = 4, col = 4)) 
  
  #Create unit labels
  max = TRACK_META$ymax #Read the maximum value of the y axis
  at = grid.pretty(c(0, max)) #Set the start point as 0 and the end point as max
  at = at[at <= max ] #Create breakpoints between 0 and max
  labels = as.character(at) #Place the labels along the y axis
  labels[at >= 1000 & at < 1000000] = paste0(at[at >= 1000 & at < 1000000]/1000, "K") #Add a K label to indicate thousands
  spiral_yaxis(at = at, labels = labels, labels_gp = gpar(fontsize = 5)) #Draw it at the beginning and end of the spiral
  
  #Create month labels
  dd = max(temp_df$departure_date) #Read the final date in the range
  day(dd) = 15 #Set the day to the 15th as a midpoint
  dd = dd + months(1:12) #Add the months
  spiral_text(dd, y = 1.5, month.name[month(dd)], facing = "inside", nice_facing = TRUE, gp = gpar(fontsize = 8)) #Draw the labels
  
  #Add year labels
  years = as.character(unique(year(temp_df$departure_date))) #Create a vector with the years in the date range
  
  #Create the title
  grid.text(sprintf("%s", years), 
            x = unit(0, "npc") + unit(0, "mm"), y = unit(1, "npc") - unit(0, "mm"),
            gp = gpar(fontsize = 14)) 
  
  #Add number of cancellations
  grid.text(sprintf("Total cancellations: %s", format(sum(temp_df$n), big.mark=",")), 
            x = unit(0, "npc") + unit(-6, "mm"), y = unit(1, "npc") - unit(5, "mm"), just = "left",
            gp = gpar(fontsize = 8)) 
}

#Count the number of years and create lists of start and end dates
yrs = as.character(unique(year(train_count$departure_date)))
start = c("2015-04-01", "2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01")
end = c("2015-12-31", "2016-12-31", "2017-12-31", "2018-12-31", "2019-12-31", "2020-12-31", "2021-12-31", "2022-12-31", "2023-12-31", "2024-10-12")

#Create a list to be populated
pl = list() 

#Run a loop to populate the list
for (i in 1:length(yrs)){
  
  pl[[i]] <- grid.grabExpr({
    grid_plot(train_count, start[i], end[i])
  })
}

#Create the title
title <- ggdraw() + 
  draw_label(sprintf("Train cancellations in Great Britain, %s-%s", first(yrs),last(yrs)),
             fontface = 'bold', size = 20, x = 0, hjust = 0) +
  #Add margin on the left of the drawing canvas, so title is aligned with left edge of first plot
  theme(plot.margin = margin(0, 0, 0, 7))

#Plot the grid of years
plot_row <- plot_grid(plotlist = pl, ncol = 4)

#Combine title with grid of years
plot_grid(title, plot_row, ncol = 1, rel_heights = c(0.1, 1))
grid.text("Source: NWR Historic Delay Attribution licensed under the Open Government Licence v3.0.\nCC-BY-SA Kim Idar Giske", 
          x = unit(0.5, "npc") + unit(5, "mm"), y = unit(0.05, "npc") - unit(0, "mm"), just = "left",
          gp = gpar(fontsize = 8)) 

#Save the grid plot
svg("figs/all_years.svg", width = 20, height = 15)
plot_grid(title, plot_row, ncol = 1, rel_heights = c(0.1, 1))
grid.text("Source: NWR Historic Delay Attribution licensed under the Open Government Licence v3.0.\nCC-BY-SA Kim Idar Giske", 
          x = unit(0.5, "npc") + unit(5, "mm"), y = unit(0.05, "npc") - unit(0, "mm"), just = "left",
          gp = gpar(fontsize = 8)) 
invisible(dev.off())