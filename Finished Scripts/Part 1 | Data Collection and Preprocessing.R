# Jonathan Schlosser
# August 30, 2020
# Final Ancient Aliens Project
# Data Collection and Pre-Processing
# Part 1 in Series


#### LOADING LIBRARIES ####
library(rvest)
library(tidyverse)
library(stopwords)


#### WEBSCRAPING DATA ####

# Establishing the website root. 
root <- "https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=ancient-aliens"


# Creating a dataframe to hold the scraped results. 
scripts = data.frame(stringsAsFactors = FALSE)


# Defining a function to properly iterate through the web root
for (s in 01:15){
  for (e in 01:22){
    if ((s<=9) & (e<=9)){
      aliens <- paste(root, "&episode=s0", s, "e0", e, sep = "")
      print(aliens)
    }
    else if ((s<=9) & (e>9)){
      aliens <- paste(root, "&episode=s0", s, "e", e, sep = "")
      print(aliens)
    }
    else if ((s>9) & (e<=9)){
      aliens <- paste(root, "&episode=s", s, "e0", e, sep = "")
      print(aliens)
    }
    else{
      aliens <- paste(root, "&episode=s", s, "e", e, sep = "")
      print(aliens)
    }
    
    tryCatch({
      aliens_webpage = read_html(aliens)
      
      
      episode_text = aliens_webpage %>%
        html_node("div.scrolling-script-container") %>%
        html_text()
      
      episode_title = aliens_webpage %>%
        html_node("h3") %>%
        html_text()
    }, 
    error=function(e){})
    
    
    temp <- matrix(data = NA, nrow = 1, ncol = 4)
    temp[,1] <- s
    temp[,2] <- e
    temp[,3] <- episode_title
    temp[,4] <- episode_text
    scripts <- rbind(scripts, temp)
  }
}

#### LOADING IN AIR DATE DATA ####

# Collected via Wikipedia (https://en.wikipedia.org/wiki/Ancient_Aliens#Season_2_(2010)) on Aug 28, 2020
Air_Dates <- read.csv('./Data/Air Dates/Original Air Dates.csv')


#### INITIAL PREPROCESSING ####

# Cleaning the raw webscraped data, removing NAs and duplicates
colnames(scripts) <- c("Season", "Episode", "EpisodeTitle", "ShowText")
scripts$WorkingText <- as.character(scripts$ShowText)
scripts_clean = filter(scripts, WorkingText != 'NA')
scripts_clean = filter(scripts_clean, duplicated(scripts_clean$EpisodeTitle) == FALSE)

# Saving and writing CSV
write.csv(scripts_clean, "Ancient Aliens Scripts.csv")

Ancient_Aliens_Scripts <-  read.csv("./Data/Ancient Aliens Scripts.csv")


#### FINALIZING DATA FRAME ####

# Joining the data sets
Ancient_Aliens_Scripts_Final <- Ancient_Aliens_Scripts %>% 
  inner_join(Air_Dates, by = c("Season", "Episode"))
# Removed 3 episodes from season 15 - this is the most recent season and the whole season was not available for collection. 

# Renaming the final columns
colnames(Ancient_Aliens_Scripts_Final) <- c("ID", "Season", "Episode", 
                                            "EpisodeTitle", "ShowText", 
                                            "WorkingText", "AirDate" )

# Setting the WorkingText to char datatype
Ancient_Aliens_Scripts_Final$WorkingText <- as.character(Ancient_Aliens_Scripts_Final$WorkingText)

Ancient_Aliens_Scripts_Final$AirDate <- as.Date(Ancient_Aliens_Scripts_Final$AirDate, "%m/%d/%y")

class(Ancient_Aliens_Scripts_Final$AirDate)

# Saving the final data
write.csv(Ancient_Aliens_Scripts_Final, "./Data/Ancient_Aliens_Scripts_Final.csv")

####  ==== END OF CODE ==== ####
