library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
data("stop_words")
data_loc ='Q:/Projects/Capstone/'

selected_data<-read.csv(paste0(data_loc,"selected_metadata.csv"))

#### Tokenize the methods and results ####


results_list<-strsplit(selected_data$results_text,split =" ")%>%unlist()
methods_list<-strsplit(selected_data$methods_text,split =" ")%>%unlist()
full_list<-append(results_list, methods_list)

word_counts<-table(full_list)%>%
  as.data.frame()%>%
  arrange(desc(Freq))