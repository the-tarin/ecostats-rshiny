library(tidyverse)
library(lubridate)
setwd("Documents/software-ds/ecostats-rshiny/")

# generate random coordinates, bearings and datetime object timestamps
generate_data <- function(num_samples) {
  # Create an empty dataframe
  survey_df <- data.frame()
  
  # Generate random data
  for(i in 1:num_samples){
    # Generate a random coordinate
    longitude <- runif(1, min = -180, max = 180)
    latitude <- runif(1, min = -90, max = 90)
    
    # Generate a random bearing
    bearing <- runif(1, min = 0, max = 360)
    
    # Generate a random datetime object timestamp
    timestamp <- Sys.time() + runif(1, min = -31536000, max = 31536000)
    
    # Create a dataframe from the generated data
    temp_df <- data.frame(
      "longitude" = longitude,
      "latitude" = latitude,
      "bearing" = bearing,
      "timestamp" = timestamp
    )
    
    # Append the temp_data to the data dataframe
    survey_df <- rbind(survey_df, temp_df)
  }
  
  return(survey_df)
}


fake_survey_df <- generate_data(50)
save(fake_survey_df, file = "dev/data/fake_survey.RData")
write.csv(fake_survey_df, file = "dev/data/fake_survey.csv")

