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

meta_data$publish_time<-as.Date(meta_data$publish_time)

early_articles<-meta_data%>%
  filter(publish_time< '2021-01-01')

#what years are represented here?

early_articles$year <- format(early_articles$publish_time, "%Y")

prop.table(table(as.integer(early_articles$year)))


#find abstracts with colons and uppercase letters like INTRODUCTION: 

pattern = "[A-Z]+:\\s+"

structured_abs<-early_articles%>%
  filter(grepl(pattern,abstract, perl=TRUE))

headers<-structured_abs$abstract%>%
  str_extract_all(pattern)

structured_abs$headers<-headers




