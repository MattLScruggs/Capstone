library(tidyverse)
source('lit_review_functions_final.R')

study_db<-create_vector_db(path_to_file = 'study_text.csv', text_col_name = 'Text', doc_col_name = 'Doc', abstraction_level = 0.3, block_length = 1)

ask_query(study_db)


