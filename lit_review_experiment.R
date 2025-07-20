### set up data source and functions 
setwd('Q:/Projects/Capstone/R Code')
source('lit_review_functions_experiment.R')

### import study files
pdfs_to_text_csv(pdf_folder='pdf_docs', file_out = 'study_text.csv')


study_db<-create_vector_db(path_to_file = 'study_text.csv', text_col_name = 'Text', doc_col_name = 'Doc', 
                           abstraction_level = 0.2,
                           block_length = 10)

results_db<-ask_query(existing_vector_db=study_db, pathogen = "C. Auris")

View(study_db$sentence_df)

View(study_db$abstract_tf_idf)

View(study_db$tfidf_matrix)

View(study_db$embedding$pc_df)


### define possible variables

pathogen_list<-c("COVID_19", "Rocky mountain spotted fever","Influenza", "Candida Auris")

block_list<-c(1,5,7,10)

abs_list<-c(0.2,0.5,0.8,0.99)

hyper_matrix<-expand.grid(pathogen_list, block_list, abs_list)

### run all outputs


epi_db<-data.frame()

for (i in 1:nrow(hyper_matrix)){

path_name<-hyper_matrix[i,1]
block<-hyper_matrix[i,2] 
var<-hyper_matrix[i,3]

study_db<-create_vector_db(path_to_file = 'study_text.csv', text_col_name = 'Text', doc_col_name = 'Doc', 
                           abstraction_level = var,
                           block_length = block)

tmp_db<-ask_query(existing_vector_db=study_db, pathogen = path_name)

tmp_db$pathogen<-path_name
tmp_db$block_size<-block
tmp_db$var_lvl<-var

epi_db<-rbind(epi_db, tmp_db)

}

### analyze results

#write.csv(epi_db, "Experimental results.csv", row.names=FALSE)

epi_db<-read.csv("Experimental results.csv")

epi_db%>%group_by(block_size, pathogen)%>%
  summarize(mean_score=mean(Best_Score))%>%
  ggplot(aes(x=block_size, y=mean_score, colour = pathogen))+geom_line()

epi_db%>%group_by(var_lvl, pathogen)%>%
  summarize(mean_score=mean(Best_Score))%>%
  ggplot(aes(x=var_lvl, y=mean_score, colour = pathogen))+geom_line()

epi_db%>%group_by(Domain)%>%
  ggplot(aes(x=Best_Score, fill=Domain))+geom_histogram()+facet_wrap(~Domain)

epi_db%>%group_by(pathogen)%>%
  ggplot(aes(x=Best_Score, fill=pathogen))+geom_histogram()+facet_wrap(~pathogen)

epi_db%>%group_by(block_size, var_lvl, pathogen)%>%
  summarize(mean_score=mean(Best_Score))%>%
  ggplot(aes(x=factor(block_size), y=factor(var_lvl), size=mean_score, colour = pathogen))+geom_point()+facet_wrap(~pathogen)

epi_db%>%group_by(block_size, var_lvl, Domain)%>%
  summarize(mean_score=mean(Best_Score))%>%
  ggplot(aes(x=factor(block_size), y=factor(var_lvl), size=mean_score, colour = Domain))+geom_point()+facet_wrap(~Domain)

epi_db%>%filter(block_size == 10)%>%
  filter(var_lvl == 0.2)%>%
  ggplot(aes(x=pathogen, y=Domain, colour = pathogen, size=Best_Score))+geom_point()
