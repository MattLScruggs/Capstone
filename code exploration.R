library(tidyverse)
source('lit_review_functions_experiment.R')

pdfs_to_text_csv()

full_data<-read.csv('study_text.csv')

sentence_df<-split_sentences(full_data$Text,id_col=full_data$Doc,block_size = 10)

word_df<-split_words(sentence_df$sentence, doc_id_col = sentence_df$doc, line_id_col = sentence_df$block)

tfidf_df<-calc_baseline_tf_idf(word_df, word_col="word",line_col="block")

hist(tfidf_df$tf_idf)

hist(tfidf_df$count)

hist(tfidf_df$total)

hist(tfidf_df$line_count)

plot(tfidf_df$line_count, tfidf_df$tf_idf, type="p")
     
plot(tfidf_df$total, tfidf_df$tf_idf, type="p")


plot(tfidf_df$count, tfidf_df$tf_idf, type="p")
tfidf_df$ratio <- tfidf_df$count/tfidf_df$total
 


plot(tfidf_df$ratio, tfidf_df$tf_idf, type="p")

plot(1/tfidf_df$line_count, tfidf_df$tf_idf, type="p")

median(tfidf_df$tf_idf)
mean(tfidf_df$tf_idf)  


tfidf_df%>%
  group_by(line)%>%
  summarize(mean=mean(tf_idf))%>%
  ggplot(aes(x=mean))+geom_histogram()

new_tfidf_df<-tfidf_df[(tfidf_df$line_count>1 & tfidf_df$count>1 & tfidf_df$total>1),]

new_tfidf_df%>%
  group_by(line)%>%
  summarize(mean=mean(tf_idf))%>%
  ggplot(aes(x=mean))+geom_histogram()

hist(tfidf_df$tf_idf)
hist(new_tfidf_df$tf_idf)

## Remove outliers with a 1 in any of the count variables. These are probably not thematically appropriate terms.


test_tf_df<-study_db$abstract_tf_idf

top_sentences<-test_tf_df%>%
  group_by(line)%>%
  summarize(score=sum(tf_idf))

sentences<-study_db$sentence_df%>%
  left_join(top_sentences, by=c("block"="line"))%>%
  drop_na()%>%
  arrange(desc(score))

sentences$sentence[1:5]

### Must must must clean the input text data better.
