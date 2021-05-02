# Jonathan Schlosser
# August 30, 2020
# Final Ancient Aliens Project
# Overall Analyses
# Part 3 in Series

#### PRELIMINARY STEPS ####
#Loading in libraries
library(tidyverse)
library(tidytext)
library(gridExtra)

#Loading in data
Ancient_Aliens_Tidy <-  read_csv("./Data/Ancient Aliens Tidy.csv")

#### FREQUENCY ANALYSES ####
# Top 50 Word Count
Word_Counts <- Ancient_Aliens_Tidy %>%
  count (word, sort=TRUE)

Top_50_Count <- ggplot(Word_Counts[1:50,], aes(x=reorder(word, n), y=n)) +
  geom_bar(stat="identity", aes(fill = n))+
  coord_flip()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 50 Words", y = "Term Frequency",
       title = "Count of Terms in Ancient Aliens Scripts")+
  guides(fill=FALSE)


png("./Plots/Top50Count.png", width = 600, height = 800)
Top_50_Count
dev.off()

# Top 50 Word Count Stemmed

Stem_Word_Counts <- Ancient_Aliens_Tidy %>%
  count(WordStem, sort=TRUE)

Stem_Top_50_Count <- ggplot(Stem_Word_Counts[1:50,], aes(x=reorder(WordStem, n), y=n)) +
  geom_bar(stat="identity", aes(fill = n))+
  coord_flip()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 50 Stemmed Words", y = "Term Frequency",
       title = "Count of Stemmed Terms in Ancient Aliens Scripts")+
  guides(fill=FALSE)


png("./Plots/StemTop50Count.png", width = 600, height = 800)
Stem_Top_50_Count
dev.off()


#### FREQUENCY SUMMARY STATISTICS ####
# Not Stemmed
Overall_Count_Stats <- Word_Counts %>% summarize(
  mean = mean(n),
  median = median(n),
  sd = sd(n),
  min = min(n),
  max = max(n))  

png("./Tables/OverallStats.png", width = 480, height = 480, bg = "white")
grid.table(Overall_Count_Stats)
dev.off()

# Stemmed
Stem_Overall_Count_Stats <- Stem_Word_Counts %>% summarize(
  mean = mean(n),
  median = median(n),
  sd = sd(n),
  min = min(n),
  max = max(n))  

png("./Tables/StemOverallStats.png", width = 480, height = 480, bg = "white")
grid.table(Stem_Overall_Count_Stats)
dev.off()

#### PROPORTION ANALYSES ####
# Top 50 Word Prop

Word_Props  <-  Ancient_Aliens_Tidy %>%
  count(word) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))

Top_50_Props <- ggplot(Word_Props[1:50,], aes(x=reorder(word, prop), y=prop)) +
  geom_bar(stat="identity", aes(fill = n))+
  coord_flip()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 50 Words", y = "Term Proportion",
       title = "Proportion of Times Word Appears in \nAncient Aliens")+
  guides(fill=FALSE)


png("./Plots/Top50Proportions.png", width = 600, height = 800)
Top_50_Props
dev.off()

# Top 50 Word Prop

Stem_Word_Props  <-  Ancient_Aliens_Tidy %>%
  count(WordStem) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))

Stem_Top_50_Props <- ggplot(Stem_Word_Props[1:50,], aes(x=reorder(WordStem, prop), y=prop)) +
  geom_bar(stat="identity", aes(fill = n))+
  coord_flip()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 50 Stemmed Words", y = "Term Proportion",
       title = "Proportion of Times Stemmed Word Appears in \nAncient Aliens")+
  guides(fill=FALSE)


png("./Plots/StemTop50Proportions.png", width = 600, height = 800)
Stem_Top_50_Props
dev.off()


#### PROPORTION SUMMARY STATISTICS ####
# Not Stemmed
Overall_Prop_Stats <- Word_Props %>% summarize(
  mean = mean(prop),
  median = median(prop),
  sd = sd(prop),
  min = min(prop),
  max = max(prop)) 

png("./Tables/OverallPropStats.png", width = 840, height = 480, bg = "white")
grid.table(Overall_Prop_Stats)
dev.off()

# Stemmed
Stem_Overall_Prop_Stats <- Stem_Word_Props %>% summarize(
  mean = mean(prop),
  median = median(prop),
  sd = sd(prop),
  min = min(prop),
  max = max(prop)) 

png("./Tables/StemOverallPropStats.png", width = 840, height = 480, bg = "white")
grid.table(Stem_Overall_Prop_Stats)
dev.off()

# Preparing and Saving Files for Application
colnames(Stem_Word_Props) <- c("StemmedWord",  "Count", "Proportion")
write_csv(Stem_Word_Props, "Data/Stem_Word_Props.csv")

####  ==== END OF CODE ==== ####
