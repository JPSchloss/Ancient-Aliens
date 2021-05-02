# Jonathan Schlosser
# August 30, 2020
# Final Ancient Aliens Project
# Episode Analyses
# Part 5 in Series

#### PRELIMINARY STEPS ####
#Loading in libraries
library(tidyverse)
library(tidytext)
library(gridExtra)

#Loading in data
Ancient_Aliens_Tidy <-  read_csv("./Data/Ancient Aliens Tidy.csv")

# Factoring By Episode
Ancient_Aliens_Tidy_Episode = Ancient_Aliens_Tidy %>% 
  mutate(EpisodeFactor = factor(EpisodeTitle, levels = unique(EpisodeTitle)))


#### FREQUENCY ANALYSES ####
# Top 15 Word Count By Episode

Episode_Word_Counts <- Ancient_Aliens_Tidy_Episode %>%
  group_by(EpisodeFactor) %>%
  count(word) 

Top_15_Words_Episode <- Episode_Word_Counts %>% 
  arrange(EpisodeFactor, -n) %>%
  top_n(15)

Top_15_Words_Episode_Plot <- ggplot(Top_15_Words_Episode, aes(x= reorder_within(word, n, EpisodeFactor), y=n, fill = n))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ EpisodeFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Words", y = "Term Frequency",
       title = "Number of Times Word Appears in \nAncient Aliens By Episode")+
  guides(fill=FALSE)

png("Plots/Top15CountByEpisode.png", width = 1400, height = 1000)
Top_15_Words_Episode_Plot
dev.off()

# Top 15 Stemmed Word Count By Episode

Stem_Episode_Word_Counts <- Ancient_Aliens_Tidy_Episode %>%
  group_by(EpisodeFactor) %>%
  count(WordStem) 

Top_15_Stem_Words_Episode <- Stem_Episode_Word_Counts %>% 
  arrange(EpisodeFactor, -n) %>%
  top_n(15)

Top_15_Stem_Words_Episode_Plot <- ggplot(Top_15_Stem_Words_Episode, aes(x= reorder_within(WordStem, n, EpisodeFactor), y=n, fill = n))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ EpisodeFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Stemmed Words", y = "Term Frequency",
       title = "Number of Times Stemmed Word Appears in \nAncient Aliens By Episode")+
  guides(fill=FALSE)

png("Plots/StemTop15CountByEpisode.png", width = 1400, height = 1000)
Top_15_Stem_Words_Episode_Plot
dev.off()


#### FREQUENCY SUMMARY STATISTICS ####
# Not Stemmed
Episode_Count_Stats <- Episode_Word_Counts %>% summarize(
  mean = mean(n),
  median = median(n),
  sd = sd(n),
  min = min(n),
  max = max(n))  

png("Tables/EpisodeCountStats.png", width = 1000, height = 4000, bg = "white")
grid.table(Episode_Count_Stats)
dev.off()

# Stemmed

Stem_Episode_Count_Stats <- Stem_Episode_Word_Counts %>% summarize(
  mean = mean(n),
  median = median(n),
  sd = sd(n),
  min = min(n),
  max = max(n))  

png("Tables/StemEpisodeCountStats.png", width = 1000, height = 4000, bg = "white")
grid.table(Stem_Episode_Count_Stats)
dev.off()

#### PROPORTION ANALYSES ####
# Top 15 Word Prop By Episode

Episode_Word_Props  <-  Ancient_Aliens_Tidy_Episode %>%
  group_by(EpisodeFactor) %>%
  count(word) %>%
  mutate(prop = n / sum(n))

Top_15_Words_Props_Episode <- Episode_Word_Props %>% 
  arrange(EpisodeFactor, -prop) %>%
  top_n(15)

Top_15_Words_Props_Episode_Plot <- ggplot(Top_15_Words_Props_Episode, aes(x= reorder_within(word, prop, EpisodeFactor), y=prop, fill = prop))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ EpisodeFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Words", y = "Term Proportion",
       title = "Proportion of Times Word Appears in \nAncient Aliens By Episode")+
  guides(fill=FALSE)


png("Plots/Top15PropByEpisode.png", width = 1400, height = 1000)
Top_15_Words_Props_Episode_Plot
dev.off()

# Top 15 Stemmed Word Prop By Episode

Stem_Episode_Word_Props  <-  Ancient_Aliens_Tidy_Episode %>%
  group_by(EpisodeFactor) %>%
  count(WordStem) %>%
  mutate(prop = n / sum(n))

Stem_Top_15_Words_Props_Episode <- Stem_Episode_Word_Props %>% 
  arrange(EpisodeFactor, -prop) %>%
  top_n(15)

Stem_Top_15_Words_Props_Episode_Plot <- ggplot(Stem_Top_15_Words_Props_Episode, aes(x= reorder_within(WordStem, prop, EpisodeFactor), y=prop, fill = prop))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ EpisodeFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Stemmed Words", y = "Term Proportion",
       title = "Proportion of Times Stemmed Word Appears in \nAncient Aliens By Episode")+
  guides(fill=FALSE)

png("Plots/StemTop15PropByEpisode.png", width = 1400, height = 1000)
Stem_Top_15_Words_Props_Episode_Plot
dev.off()

#### PROPORTION SUMMARY STATISTICS ####
Episode_Prop_Stats <- Episode_Word_Props %>% summarize(
  mean = mean(prop),
  median = median(prop),
  sd = sd(prop),
  min = min(prop),
  max = max(prop))

png("Tables/EpisodePropStats.png", width = 1000, height = 4000, bg = "white")
grid.table(Episode_Prop_Stats)
dev.off()


Stem_Episode_Prop_Stats <- Stem_Episode_Word_Props %>% summarize(
  mean = mean(prop),
  median = median(prop),
  sd = sd(prop),
  min = min(prop),
  max = max(prop))

png("Tables/StemEpisodePropStats.png", width = 1000, height = 4000, bg = "white")
grid.table(Stem_Episode_Prop_Stats)
dev.off()

#### TFIDF ANALYSES ####
#Calculating TF-IDF By Episode For Non-Stemmed Words
Episode_tfidf <- Ancient_Aliens_Tidy_Episode %>%
  count(word, EpisodeFactor) %>%
  bind_tf_idf(word, EpisodeFactor, n) 

Top_15_Episode_TFIDF <- Episode_tfidf %>%
  group_by(EpisodeFactor) %>%
  arrange(EpisodeFactor, -tf_idf) %>%
  top_n(15)

Top_15_Words_Episode_TFIDF_Plot <- ggplot(Top_15_Episode_TFIDF, aes(x= reorder_within(word, tf_idf, EpisodeFactor), y=tf_idf, fill = tf_idf))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ EpisodeFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Words", y = "TF*IDF",
       title = "TFIDF in Ancient Aliens\n By Episode")+
  guides(fill=FALSE)

png("Plots/Top15TFIDFByEpisode.png", width = 1400, height = 1000)
Top_15_Words_Episode_TFIDF_Plot
dev.off()

#Calculating TF-IDF By Episode For Stemmed Words
Stem_Episode_tfidf <- Ancient_Aliens_Tidy_Episode %>%
  count(WordStem, EpisodeFactor) %>%
  bind_tf_idf(WordStem, EpisodeFactor, n) 

Stem_Top_15_Episode_TFIDF <- Stem_Episode_tfidf %>%
  group_by(EpisodeFactor) %>%
  arrange(EpisodeFactor, -tf_idf) %>%
  top_n(15)

Stem_Top_15_Words_Episode_TFIDF_Plot <- ggplot(Stem_Top_15_Episode_TFIDF, aes(x= reorder_within(WordStem, tf_idf, EpisodeFactor), y=tf_idf, fill = tf_idf))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ EpisodeFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Stemmed Words", y = "TF*IDF",
       title = "TFIDF in Ancient Aliens\n By Episode")+
  guides(fill=FALSE)

png("Plots/StemTop15TFIDFByEpisode.png", width = 1400, height = 1000)
Stem_Top_15_Words_Episode_TFIDF_Plot
dev.off()


#### STM TOPIC MODEL ####
library(quanteda)
library(topicmodels)
library(stm)

# Creating the DFM
Stem_Episode_DFM <- Ancient_Aliens_Tidy_Episode %>%
  count(EpisodeFactor, WordStem, sort = TRUE) %>%
  cast_dfm(EpisodeFactor, WordStem, n)

Stem_Episode_STM = convert(Stem_Episode_DFM, to = 'stm')

# Identifying Optimal K Value

Find_Episode_K_Values_Stem <- searchK(Stem_Episode_STM$documents, Stem_Episode_STM$vocab, K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
                              data = Stem_Episode_STM$meta, seed = 374075)

png("Plots/StemEpisodeTopicDiagnostics.png", width = 1000, height = 600)
plot(Find_Episode_K_Values_Stem)
dev.off()

Find_Episode_K_Values_Stem_Narrowed <- searchK(Stem_Episode_STM$documents, Stem_Episode_STM$vocab, K =
                                        c(25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35),
                                      data = Stem_Episode_STM$meta, seed = 374075)

png("Plots/StemEpisodeTopicDiagnosticsNarrowed.png", width = 1000, height = 600)
plot(Find_Episode_K_Values_Stem_Narrowed)
dev.off()


# Running STM Topic Model - 26 Topics 
Stem_Episode_Topic_Model <- stm(Stem_Episode_DFM, K = 26, 
                               verbose = FALSE, init.type = "Spectral", seed = 374075)

# Setting Topic Levels Variable For Cleaner Graphics
topic_levels = c('Topic 1', 'Topic 2', 'Topic 3', 'Topic 4', 'Topic 5', 'Topic 6', 'Topic 7', 'Topic 8',
                 'Topic 9', 'Topic 10', 'Topic 11', 'Topic 12', 'Topic 13', 'Topic 14', 'Topic 15', 
                 'Topic 16', 'Topic 17', 'Topic 18', 'Topic 19', 'Topic 20', 'Topic 21', 'Topic 22',
                 'Topic 23', 'Topic 24', 'Topic 25', 'Topic 26')

# Identifying Top Words In Each Topic From STM Model
Stem_Tidy_Episode_Topics <- tidy(Stem_Episode_Topic_Model)

Stem_Tidy_Episode_Topics_Plot <- Stem_Tidy_Episode_Topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  mutate(topic = factor(topic, levels = topic_levels)) %>%
  ggplot(aes(term, beta, fill = beta)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip()+
  facet_wrap(~ topic, scales = "free_y") +
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 10 Stemmed Words", y = "Proportion in Each Topic",
       title = "Stemmed Words In Each Topic \nin Ancient Aliens By Episode (K=26)",
       subtitle = 'STM Model')+
  guides(fill=FALSE)


png("Plots/StemEpisodeTopicModel.png", width = 1000, height = 600)
Stem_Tidy_Episode_Topics_Plot
dev.off()


# Identifying Top Episodes in Each Topic From STM Model
Stem_Tidy_Episode_Gamma <- tidy(Stem_Episode_Topic_Model, matrix = "gamma",                    
                               document_names = rownames(Stem_Episode_DFM))


Stem_Tidy_Episode_Gamma_Plot <- Stem_Tidy_Episode_Gamma %>%
  filter(gamma >= 0.1) %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic) +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "Episodes in Each Topic",
       title = "Distribution of Episodes within Each Topic - Stemmed Terms",
       subtitle = 'STM Model')+
  guides(fill=FALSE)


png("Plots/StemEpisodeTopicModelGamma.png", width = 1000, height = 600)
Stem_Tidy_Episode_Gamma_Plot
dev.off()

#### LDA GIBBS TOPIC MODEL ####
# Running LDA Gibbs Topic Model - 26 Topics 
StemLDA = LDA(convert(Stem_Episode_DFM, to="topicmodels"),
              k=26,
              method="Gibbs",
              control = list(seed = 374075)) 

# Identifying Top Words In Each Topic From Gibbs Model
Stem_LDA_Tidy_Episode_Topics <- tidy(StemLDA)

Stem_LDA_Tidy_Episode_Topics_Plot <- Stem_LDA_Tidy_Episode_Topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  mutate(topic = factor(topic, levels = topic_levels)) %>%
  ggplot(aes(term, beta, fill = beta)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip()+
  facet_wrap(~ topic, scales = "free_y") +
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 10 Stemmed Words", y = "Proportion in Each Topic",
       title = "Stemmed Words In Each Topic \nin Ancient Aliens By Episode (K=26)",
       subtitle = 'LDA Gibbs Model')+
  guides(fill=FALSE)


png("Plots/StemLDAEpisodeTopicModel.png", width = 1000, height = 600)
Stem_LDA_Tidy_Episode_Topics_Plot
dev.off()

# Identifying Top Episodes in Each Topic From Gibbs Model
Stem_LDA_Tidy_Episode_Gamma <- tidy(StemLDA, matrix = "gamma",                    
                                   document_names = rownames(Stem_Episode_DFM))


Stem_LDA_Tidy_Episode_Gamma_Plot <- Stem_LDA_Tidy_Episode_Gamma %>%
  filter(gamma >= 0.1) %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic) +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "Episodes in Each Topic",
       title = "Distribution of Episodes within Each Topic - Stemmed Terms",
       subtitle = 'LDA Gibbs Model')+
  guides(fill=FALSE)


png("Plots/StemLDAEpisodeTopicModelGamma.png", width = 1000, height = 600)
Stem_LDA_Tidy_Episode_Gamma_Plot
dev.off()

#### LDA VEM TOPIC MODEL ####
# Running LDA VEM Topic Model - 26 Topics 
StemLDA2 = LDA(convert(Stem_Episode_DFM, to="topicmodels"),
               k=26,
               method="VEM",
               control = list(seed = 374075)) 

# Identifying Top Words In Each Topic From VEM Model
Stem_LDA2_Tidy_Episode_Topics <- tidy(StemLDA2)

Stem_LDA2_Tidy_Episode_Topics_Plot <- Stem_LDA2_Tidy_Episode_Topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  mutate(topic = factor(topic, levels = topic_levels)) %>%
  ggplot(aes(term, beta, fill = beta)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip()+
  facet_wrap(~ topic, scales = "free_y") +
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 10 Stemmed Words", y = "Proportion in Each Topic",
       title = "Stemmed Words In Each Topic \nin Ancient Aliens By Episode (K=26)",
       subtitle = 'LDA VEM Model')+
  guides(fill=FALSE)


png("Plots/StemLDA2EpisodeTopicModel.png", width = 1000, height = 600)
Stem_LDA2_Tidy_Episode_Topics_Plot
dev.off()

# Identifying Top Episodes in Each Topic From STM Model
Stem_LDA2_Tidy_Episode_Gamma <- tidy(StemLDA2, matrix = "gamma",                    
                                    document_names = rownames(Stem_Episode_DFM))


Stem_LDA2_Tidy_Episode_Gamma_Plot <- Stem_LDA2_Tidy_Episode_Gamma %>%
  filter(gamma >= 0.1) %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic) +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "Episodes in Each Topic",
       title = "Distribution of Episodes within Each Topic - Stemmed Terms",
       subtitle = 'LDA VEM Model')+
  guides(fill=FALSE)


png("Plots/StemLDA2EpisodeTopicModelGamma.png", width = 1000, height = 600)
Stem_LDA2_Tidy_Episode_Gamma_Plot
dev.off()


#### TIME ANALYSES ####
# Creating Air Date and Episode Dataframe
AirDates <- Ancient_Aliens_Tidy %>%
  select(EpisodeTitle, AirDate) %>%
  distinct()

# Creating stem word and time dataframe
Stemmmed_Words_Time <- Ancient_Aliens_Tidy %>%
  select(WordStem, AirDate)

write_csv(Stemmmed_Words_Time, "Data/Stemmmed_Words_Time.csv")

# Creating working dataframe for stemmed words. 
STM_Episodes_Dates <-  Stem_Tidy_Episode_Gamma %>%
  left_join(AirDates, by = c('document' = 'EpisodeTitle'))
colnames(STM_Episodes_Dates) <- c("Episode Title", "Topic", "Gamma", "Date")
STM_Episodes_Dates$Model <- 'STM'

Gibbs_Episodes_Dates <-  Stem_LDA_Tidy_Episode_Gamma %>%
  left_join(AirDates, by = c('document' = 'EpisodeTitle'))
colnames(Gibbs_Episodes_Dates) <- c("Episode Title", "Topic", "Gamma", "Date")
Gibbs_Episodes_Dates$Model <- 'Gibbs'

VEM_Episodes_Dates <-  Stem_LDA2_Tidy_Episode_Gamma %>%
  left_join(AirDates, by = c('document' = 'EpisodeTitle'))
colnames(VEM_Episodes_Dates) <- c("Episode Title", "Topic", "Gamma", "Date")
VEM_Episodes_Dates$Model <- 'VEM'

Episode_Dates <- bind_rows(STM_Episodes_Dates, Gibbs_Episodes_Dates, VEM_Episodes_Dates)

write_csv(Episode_Dates, "Data/Episode_Dates.csv")

# Timeline Graphs - Will need to come back to this after exploration


#### SAVING FILES FOR APPLICATION ####

# Saving Word Count Dataset
colnames(Stem_Episode_Word_Props) <- c("EpisodeTitle", "StemmedWord",  "Count", "Proportion")
write_csv(Stem_Episode_Word_Props, "Data/Stem_Episode_Word_Props.csv")

# Saving TF-IDF Dataset
colnames(Stem_Episode_tfidf) <- c("StemmedWord", "EpisodeTitle", "Count", "TF", "IDF", "TF-IDF")
write_csv(Stem_Episode_tfidf, "Data/Stem_Episode_tfidf.csv")


# Saving Topic and Gamma Datasets for Each Model
colnames(Stem_Tidy_Episode_Topics) <- c("Topic", "Term", "Beta")
Stem_Tidy_Episode_Topics$Model <- 'STM'

colnames(Stem_Tidy_Episode_Gamma) <- c("EpisodeTitle", "Topic", "Gamma")
Stem_Tidy_Episode_Gamma$Model <- 'STM'

colnames(Stem_LDA_Tidy_Episode_Topics) <- c("Topic", "Term", "Beta")
Stem_LDA_Tidy_Episode_Topics$Model <- 'Gibbs'

colnames(Stem_LDA_Tidy_Episode_Gamma) <- c("EpisodeTitle", "Topic", "Gamma")
Stem_LDA_Tidy_Episode_Gamma$Model <- 'Gibbs'

colnames(Stem_LDA2_Tidy_Episode_Topics) <- c("Topic", "Term", "Beta")
Stem_LDA2_Tidy_Episode_Topics$Model <- 'VEM'

colnames(Stem_LDA2_Tidy_Episode_Gamma) <- c("EpisodeTitle", "Topic", "Gamma")
Stem_LDA2_Tidy_Episode_Gamma$Model <- 'VEM'

Stem_Topics <- bind_rows(Stem_Tidy_Episode_Topics, Stem_LDA_Tidy_Episode_Topics, Stem_LDA2_Tidy_Episode_Topics)
Stem_Gamma <- bind_rows(Stem_Tidy_Episode_Gamma, Stem_LDA_Tidy_Episode_Gamma, Stem_LDA2_Tidy_Episode_Gamma)

write_csv(Stem_Topics, "Data/Stem_Topics.csv")
write_csv(Stem_Gamma, "Data/Stem_Gamma.csv")


####  ==== END OF CODE ==== ####

