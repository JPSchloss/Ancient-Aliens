# Jonathan Schlosser
# August 30, 2020
# Final Ancient Aliens Project
# Tidying Data and Text Pre-processing
# Part 2 in Series

#### PRELIMINARY STEPS ####
#Loading in libraries
library(tidyverse)
library(tidytext)
library(SnowballC)

#Loading in data
Ancient_Aliens <-  read_csv("./Data/Ancient_Aliens_Scripts_Final.csv")


#### TEXT PROCESSING ####

# Removing numbers, punctuation, and other odd 
remove_regex <- c("[[:upper:]]{3,}", 
                  "[[:punct:]]", 
                  "[[:digit:]]", 
                  "[^\x01-\x7F]", 
                  "^[0-9]\\w+")

for (i in remove_regex) {
  tryCatch({
    Ancient_Aliens$WorkingText = gsub(i, " ", Ancient_Aliens$WorkingText)
  }, error=function(e){})
}

# Creating a list of additional terms to remove from words lists 
remove_terms_list <- as.vector(c('narrator', 'human', 'people', 'peoples', 'earth',
                                 'world', 'exist', 'suggest', 'extraterrestrial',
                                 'ancient', 'god', 'time', 'found', 'astronaut',
                                 'alien', 'â', 'ð', 'uh', 've', 'theorists', 'theory',
                                 'yeah', 'gods', 'extraterrestrials', 'life', 'evidence',
                                 'planet', 'technology', 'space', 'called', 'humans', 'thousands'))

# Unnesting Words and Creating a Tidy Dataset and removing stopwords
Ancient_Aliens_Tidy <- Ancient_Aliens %>%
  unnest_tokens(word, WorkingText) %>%
  anti_join(stop_words) %>%
  filter(!word %in% remove_terms_list)%>%
  mutate(WordStem = wordStem(word))

# Saving the Tidy File
write.csv(Ancient_Aliens_Tidy, "./Data/Ancient Aliens Tidy.csv")

####  ==== END OF CODE ==== ####
