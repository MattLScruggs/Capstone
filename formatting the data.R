#### setting up libraries and data file locations #### 
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
data("stop_words")
data_loc ='Q:/Projects/Capstone/'


#### reading in the data ####
meta_data_sample<-read.csv(paste0(data_loc,"metadata.csv"), nrows=10)
colnames(meta_data_sample)
all_cols<-as.list(apply(meta_data_sample, 2, class))
cols_needed <- c("cord_uid","abstract","journal","publish_time")
all_cols[!names(all_cols) %in% cols_needed] = list(NULL)

meta_data<- read.csv(paste0(data_loc, "metadata.csv"), header=TRUE, colClasses = all_cols)

meta_data$publish_time<-as.Date(meta_data$publish_time)





#### find abstracts with colons and uppercase letters like INTRODUCTION:####

pattern = "[A-Z]+:\\s+"

structured_abs<-meta_data%>%
  filter(grepl(pattern,abstract, perl=TRUE))

#headers<-structured_abs$abstract%>%
#  str_extract_all(pattern)%>%
#  unlist()

#header_counts<-table(headers)%>%
#  as.data.frame()%>%
#  arrange(desc(Freq))

structured_abs$year <- format(structured_abs$publish_time, "%Y")

methods_pattern<-"METHODS: (.*?) [A-Z]+:"

structured_abs$method_text<- str_extract(structured_abs$abstract, methods_pattern)%>%
  str_replace(pattern = "METHODS: ",replacement ="")

results_pattern<-"RESULTS: (.*?) [A-Z]+:"

structured_abs$results_text<- str_extract(structured_abs$abstract, results_pattern)%>%
  str_replace(pattern = "RESULTS: ",replacement ="")

structured_abs<-structured_abs%>%
  drop_na()

epidemiology_studies<-structured_abs%>%
  filter(grepl('epidemiological study', abstract, perl=TRUE))

epidemiology_studies$abstract[1]
epidemiology_studies$abstract[2]
epidemiology_studies$abstract[6]
epidemiology_studies$abstract[41]
epidemiology_studies$abstract[31]

es_1<-epidemiology_studies$cord_uid[1]
es_2<-epidemiology_studies$cord_uid[2]
es_3<-epidemiology_studies$cord_uid[6]
es_4<-epidemiology_studies$cord_uid[41]
es_5<-epidemiology_studies$cord_uid[31]

case_reports<-structured_abs%>%
  filter(grepl('a case report of', abstract, perl=TRUE))


case_reports$abstract[3]
cr_1<-case_reports$cord_uid[3]
case_reports$abstract[34]
cr_2<-case_reports$cord_uid[34]
case_reports$abstract[116]
cr_3<-case_reports$cord_uid[116]
cr_4<-case_reports$cord_uid[3]
cr_5<-case_reports$cord_uid[4]
case_reports$abstract[6600]
case_reports$abstract[6672]

cord_id_list<-c(cr_1,cr_2,cr_3,cr_4,cr_5,es_1,es_2,es_3,es_4,es_5)

selected_df<-structured_abs%>%filter(cord_uid %in% cord_id_list)

write.csv(selected_df,file=paste0(data_loc,"selected_metadata.csv"))
write.csv(structured_abs, file=paste0(data_loc, "structured_abstracts.csv"))

