# Jonathan Schlosser
# August 30, 2020
# Final Ancient Aliens Project
# Season Analyses
# Part 4 in Series

#### PRELIMINARY STEPS ####
#Loading in libraries
library(tidyverse)
library(tidytext)
library(gridExtra)

#Loading in data
Ancient_Aliens_Tidy <-  read_csv("./Data/Ancient Aliens Tidy.csv")

# Factoring By Season
Ancient_Aliens_Tidy_Season = Ancient_Aliens_Tidy %>% 
  mutate(SeasonFactor = factor(Season, levels = unique(Season)))


#### FREQUENCY ANALYSES ####
# Top 15 Word Count By Season

Season_Word_Counts <- Ancient_Aliens_Tidy_Season %>%
  group_by(SeasonFactor) %>%
  count(word) 

Top_15_Words_Season <- Season_Word_Counts %>% 
  arrange(SeasonFactor, -n) %>%
  top_n(15)

Top_15_Words_Season_Plot <- ggplot(Top_15_Words_Season, aes(x= reorder_within(word, n, SeasonFactor), y=n, fill = n))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ SeasonFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Words", y = "Term Frequency",
       title = "Number of Times Word Appears in \nAncient Aliens By Season")+
  guides(fill=FALSE)

png("Plots/Top15CountBySeason.png", width = 1400, height = 1000)
Top_15_Words_Season_Plot
dev.off()

# Top 15 Stemmed Word Count By Season

Stem_Season_Word_Counts <- Ancient_Aliens_Tidy_Season %>%
  group_by(SeasonFactor) %>%
  count(WordStem) 

Top_15_Stem_Words_Season <- Stem_Season_Word_Counts %>% 
  arrange(SeasonFactor, -n) %>%
  top_n(15)

Top_15_Stem_Words_Season_Plot <- ggplot(Top_15_Stem_Words_Season, aes(x= reorder_within(WordStem, n, SeasonFactor), y=n, fill = n))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ SeasonFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Stemmed Words", y = "Term Frequency",
       title = "Number of Times Stemmed Word Appears in \nAncient Aliens By Season")+
  guides(fill=FALSE)

png("Plots/StemTop15CountBySeason.png", width = 1400, height = 1000)
Top_15_Stem_Words_Season_Plot
dev.off()


#### FREQUENCY SUMMARY STATISTICS ####
# Not Stemmed
Season_Count_Stats <- Season_Word_Counts %>% summarize(
  mean = mean(n),
  median = median(n),
  sd = sd(n),
  min = min(n),
  max = max(n))  

png("Tables/SeasonCountStats.png", width = 1000, height = 1400, bg = "white")
grid.table(Season_Count_Stats)
dev.off()

# Stemmed

Stem_Season_Count_Stats <- Stem_Season_Word_Counts %>% summarize(
  mean = mean(n),
  median = median(n),
  sd = sd(n),
  min = min(n),
  max = max(n))  

png("Tables/StemSeasonCountStats.png", width = 1000, height = 1400, bg = "white")
grid.table(Stem_Season_Count_Stats)
dev.off()

#### PROPORTION ANALYSES ####
# Top 15 Word Prop By Season

Season_Word_Props  <-  Ancient_Aliens_Tidy_Season %>%
  group_by(SeasonFactor) %>%
  count(word) %>%
  mutate(prop = n / sum(n))

Top_15_Words_Props_Season <- Season_Word_Props %>% 
  arrange(SeasonFactor, -prop) %>%
  top_n(15)

Top_15_Words_Props_Season_Plot <- ggplot(Top_15_Words_Props_Season, aes(x= reorder_within(word, prop, SeasonFactor), y=prop, fill = prop))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ SeasonFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Words", y = "Term Proportion",
       title = "Proportion of Times Word Appears in \nAncient Aliens By Season")+
  guides(fill=FALSE)


png("Plots/Top15PropBySeason.png", width = 1400, height = 1000)
Top_15_Words_Props_Season_Plot
dev.off()

# Top 15 Stemmed Word Prop By Season

Stem_Season_Word_Props  <-  Ancient_Aliens_Tidy_Season %>%
  group_by(SeasonFactor) %>%
  count(WordStem) %>%
  mutate(prop = n / sum(n))

Stem_Top_15_Words_Props_Season <- Stem_Season_Word_Props %>% 
  arrange(SeasonFactor, -prop) %>%
  top_n(15)

Stem_Top_15_Words_Props_Season_Plot <- ggplot(Stem_Top_15_Words_Props_Season, aes(x= reorder_within(WordStem, prop, SeasonFactor), y=prop, fill = prop))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ SeasonFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Stemmed Words", y = "Term Proportion",
       title = "Proportion of Times Stemmed Word Appears in \nAncient Aliens By Season")+
  guides(fill=FALSE)

png("Plots/StemTop15PropBySeason.png", width = 1400, height = 1000)
Stem_Top_15_Words_Props_Season_Plot
dev.off()

#### PROPORTION SUMMARY STATISTICS ####
Season_Prop_Stats <- Season_Word_Props %>% summarize(
  mean = mean(prop),
  median = median(prop),
  sd = sd(prop),
  min = min(prop),
  max = max(prop))

png("Tables/SeasonPropStats.png", width = 1000, height = 1400, bg = "white")
grid.table(Season_Prop_Stats)
dev.off()


Stem_Season_Prop_Stats <- Stem_Season_Word_Props %>% summarize(
  mean = mean(prop),
  median = median(prop),
  sd = sd(prop),
  min = min(prop),
  max = max(prop))

png("Tables/StemSeasonPropStats.png", width = 1000, height = 1400, bg = "white")
grid.table(Stem_Season_Prop_Stats)
dev.off()

#### TFIDF ANALYSES ####
#Calculating TF-IDF By Season For Non-Stemmed Words
Season_tfidf <- Ancient_Aliens_Tidy_Season %>%
  count(word, SeasonFactor) %>%
  bind_tf_idf(word, SeasonFactor, n) 

Top_15_Season_TFIDF <- Season_tfidf %>%
  group_by(SeasonFactor) %>%
  arrange(SeasonFactor, -tf_idf) %>%
  top_n(15)

Top_15_Words_Season_TFIDF_Plot <- ggplot(Top_15_Season_TFIDF, aes(x= reorder_within(word, tf_idf, SeasonFactor), y=tf_idf, fill = tf_idf))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ SeasonFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Words", y = "TF*IDF",
       title = "TFIDF in Ancient Aliens\n By Season")+
  guides(fill=FALSE)

png("Plots/Top15TFIDFBySeason.png", width = 1400, height = 1000)
Top_15_Words_Season_TFIDF_Plot
dev.off()

#Calculating TF-IDF By Season For Stemmed Words
Stem_Season_tfidf <- Ancient_Aliens_Tidy_Season %>%
  count(WordStem, SeasonFactor) %>%
  bind_tf_idf(WordStem, SeasonFactor, n) 

Stem_Top_15_Season_TFIDF <- Stem_Season_tfidf %>%
  group_by(SeasonFactor) %>%
  arrange(SeasonFactor, -tf_idf) %>%
  top_n(15)

Stem_Top_15_Words_Season_TFIDF_Plot <- ggplot(Stem_Top_15_Season_TFIDF, aes(x= reorder_within(WordStem, tf_idf, SeasonFactor), y=tf_idf, fill = tf_idf))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ SeasonFactor, scales = "free_y")+
  scale_x_reordered()+
  scale_fill_gradient(high = 'black', low = 'chartreuse1')+
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Top 15 Stemmed Words", y = "TF*IDF",
       title = "TFIDF in Ancient Aliens\n By Season")+
  guides(fill=FALSE)

png("Plots/StemTop15TFIDFBySeason.png", width = 1400, height = 1000)
Stem_Top_15_Words_Season_TFIDF_Plot
dev.off()


#### STM TOPIC MODEL ####
library(quanteda)
library(topicmodels)
library(stm)

# Creating the DFM
Stem_Season_DFM <- Ancient_Aliens_Tidy_Season %>%
  count(SeasonFactor, WordStem, sort = TRUE) %>%
  cast_dfm(SeasonFactor, WordStem, n)

Stem_Season_STM = convert(Stem_Season_DFM, to = 'stm')

# Identifying Optimal K Value
Find_K_Values_Stem <- searchK(Stem_Season_STM$documents, Stem_Season_STM$vocab, K = c(2,3,4,5,6,7,8,
                                                                                      9,10,11,12,13,14),
                              data = Stem_Season_STM$meta, seed = 374075)

png("Plots/StemSeasonTopicDiagnostics.png", width = 1000, height = 600)
plot(Find_K_Values_Stem)
dev.off()

# Running STM Topic Model - 6 Topics 
Stem_Season_Topic_Model <- stm(Stem_Season_DFM, K = 6, 
                               verbose = FALSE, init.type = "Spectral", seed = 374075)

# Identifying Top Words In Each Topic From STM Model
Stem_Tidy_Season_Topics <- tidy(Stem_Season_Topic_Model)

Stem_Tidy_Season_Topics_Plot <- Stem_Tidy_Season_Topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
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
       title = "Stemmed Words In Each Topic \nin Ancient Aliens By Season (K=6)",
       subtitle = 'STM Model')+
  guides(fill=FALSE)


png("Plots/StemSeasonTopicModel.png", width = 1000, height = 600)
Stem_Tidy_Season_Topics_Plot
dev.off()


# Identifying Top Seasons in Each Topic From STM Model
Stem_Tidy_Season_Gamma <- tidy(Stem_Season_Topic_Model, matrix = "gamma",                    
                               document_names = rownames(Stem_Season_DFM))


Stem_Tidy_Season_Gamma_Plot <- Stem_Tidy_Season_Gamma %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic) +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "Seasons in Each Topic",
       title = "Distribution of Seasons within Each Topic - Stemmed Terms",
       subtitle = 'STM Model')+
  guides(fill=FALSE)


png("Plots/StemSeasonTopicModelGamma.png", width = 1000, height = 600)
Stem_Tidy_Season_Gamma_Plot
dev.off()

#### LDA GIBBS TOPIC MODEL ####
# Running LDA Gibbs Topic Model - 6 Topics 
StemLDA = LDA(convert(Stem_Season_DFM, to="topicmodels"),
           k=6,
           method="Gibbs",
           control = list(seed = 374075)) 

Stem_LDA_Tidy_Season_Topics <- tidy(StemLDA)

Stem_LDA_Tidy_Season_Topics_Plot <- Stem_LDA_Tidy_Season_Topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
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
       title = "Stemmed Words In Each Topic \nin Ancient Aliens By Season (K=6)",
       subtitle = 'LDA Gibbs Model')+
  guides(fill=FALSE)


png("Plots/StemLDASeasonTopicModel.png", width = 1000, height = 600)
Stem_LDA_Tidy_Season_Topics_Plot
dev.off()

# Identifying Top Seasons in Each Topic From Gibbs Model
Stem_LDA_Tidy_Season_Gamma <- tidy(StemLDA, matrix = "gamma",                    
                               document_names = rownames(Stem_Season_DFM))


Stem_LDA_Tidy_Season_Gamma_Plot <- Stem_LDA_Tidy_Season_Gamma %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic) +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "Seasons in Each Topic",
       title = "Distribution of Seasons within Each Topic - Stemmed Terms",
       subtitle = 'LDA Gibbs Model')+
  guides(fill=FALSE)


png("Plots/StemLDASeasonTopicModelGamma.png", width = 1000, height = 600)
Stem_LDA_Tidy_Season_Gamma_Plot
dev.off()

#### LDA VEM TOPIC MODEL ####
# Running LDA VEM Topic Model - 6 Topics 
StemLDA2 = LDA(convert(Stem_Season_DFM, to="topicmodels"),
              k=6,
              method="VEM",
              control = list(seed = 374075)) 

Stem_LDA2_Tidy_Season_Topics <- tidy(StemLDA2)

Stem_LDA2_Tidy_Season_Topics_Plot <- Stem_LDA2_Tidy_Season_Topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
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
       title = "Stemmed Words In Each Topic \nin Ancient Aliens By Season (K=6)",
       subtitle = 'LDA VEM Model')+
  guides(fill=FALSE)


png("Plots/StemLDA2SeasonTopicModel.png", width = 1000, height = 600)
Stem_LDA2_Tidy_Season_Topics_Plot
dev.off()

# Identifying Top Seasons in Each Topic From STM Model
Stem_LDA2_Tidy_Season_Gamma <- tidy(StemLDA2, matrix = "gamma",                    
                                   document_names = rownames(Stem_Season_DFM))


Stem_LDA2_Tidy_Season_Gamma_Plot <- Stem_LDA2_Tidy_Season_Gamma %>%
  ggplot(aes(gamma, fill = gamma)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic) +
  theme_minimal()+
  theme(axis.text.x = element_text(hjust = 0.5, face = 'bold'), 
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))+
  labs(x = "Topic", y = "Seasons in Each Topic",
       title = "Distribution of Seasons within Each Topic - Stemmed Terms",
       subtitle = 'LDA VEM Model')+
  guides(fill=FALSE)


png("Plots/StemLDA2SeasonTopicModelGamma.png", width = 1000, height = 600)
Stem_LDA2_Tidy_Season_Gamma_Plot
dev.off()

####  ==== END OF CODE ==== ####

