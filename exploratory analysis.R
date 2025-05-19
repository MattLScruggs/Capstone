#### setting up libraries and data file locations #### 
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
data("stop_words")
data_loc ='F:/Science/Capstone/Data sets/cord-19_2022-06-02.tar/cord-19_2022-06-02/2022-06-02/'


#### reading in the data ####
meta_data_sample<-read.csv(paste0(data_loc,"metadata.csv"), nrows=10)
colnames(meta_data_sample)
all_cols<-as.list(apply(meta_data_sample, 2, class))
cols_needed <- c("cord_uid","abstract","journal","publish_time")
all_cols[!names(all_cols) %in% cols_needed] = list(NULL)

meta_data<- read.csv(paste0(data_loc, "metadata.csv"), header=TRUE, colClasses = all_cols)

#### Identifying and visualizing bigrams ####

abs_df<-meta_data[,c('cord_uid','abstract')]%>%
  unnest_tokens(bigram, abstract, token = 'ngrams', n=2)%>%
  filter(!is.na(bigram))


bigram_sep<-abs_df%>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts<-bigram_sep%>%
  count(word1, word2, sort =T)



bigram_graph<-bigram_counts%>%
  filter(n>=50)%>%
  graph_from_data_frame()

filtered_graph<-bigram_graph%>%delete_vertices(V(bigram_graph)[degree(bigram_graph)<10])


ggraph(filtered_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#### identifying bigrams with the word "transmission" ####


R0_bigrams<-bigram_counts%>%
  filter(word1 == 'transmission' | word2 == 'transmission')%>%
  filter(n>50)

R0_graph<-R0_bigrams%>%
  graph_from_data_frame()


ggraph(R0_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#### split 

