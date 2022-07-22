rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load("doBy", "dplyr", "gdata", "reshape2", "stringr", "ggplot2", "readxl", "data.table", "Rmisc")


where_are_the_files = "./raw_data/"  
d.list <- list.files(path = where_are_the_files)
files <- paste0(where_are_the_files, d.list)
id <- 1
df <- data.frame()

for (file in files) {
  
  d <- read.csv(file = file) #read each file
  
  # replace whitespaces for NAN and then all NAN for previous value
  d <- mutate_all(d, list(~na_if(.,"")))
  d <- d %>% tidyr::fill(everything())
  
  
  
  # select columns
  d <- d[, c("sender", "timestamp", "gain_color", "loss_color", "left", "right",
             "choice", "not_choice", "diff_net_value", 
             "duration", "responded", "response", "points", 
             "pointsBlock", "points1Block", 
             "points2Block", "points3Block",  "points4Block", 
             "points5Block", "points6Block",
             "response_action", "picture_left", "picture_right")]
  
  
  d$subject_id <- id #create an id
  d$duration <- shift(d$duration)
  d$file <- file
  id <- id + 1
  
  d <- d[d$sender %in% c("choice"), ] #select rows
  
  d$sender <- NULL #remove sender column
  
  rownames(d) <- NULL  #reset index
  
  print(nrow(d))
  
  d$block <- 0
  
  d[25:69, "block"] <- 1
  d[70:114, "block"] <- 2
  d[115:159, "block"] <- 3
  d[160:204, "block"] <- 4
  d[205:249, "block"] <- 5
  d[250:nrow(d), "block"] <- 6
  
  # Lets create a correcte columns, where choice and not choice represent
  # proportion of losses.
  d$choice_corrected <- ifelse(d$gain_color == "blue",
                                 1-d$choice,
                                 d$choice)
  d$not_choice_corrected<- ifelse(d$gain_color == "blue",
                                    1-d$not_choice,
                                    d$not_choice)
  
  
  d$choice_corrected <- ifelse(d$choice_corrected == 1,
                               0,
                               d$choice_corrected)
  d$not_choice_corrected <- ifelse(d$not_choice_corrected == 1,
                                   0,
                                   d$not_choice_corrected)
  
  d$maximized_option <- ifelse(d$choice_corrected < d$not_choice_corrected, 1, 0)
  
  # combine all participants
  df <- rbind(df, d)
  
}


write.csv(df,"cleaned_df.csv", row.names = FALSE)

