#Author: Jake Brulato
#Date: 4/4/2023
#Title: Assignment 4 Text Analytics

install.packages("data.table")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tm") # text mining 
install.packages("topicmodels") 
install.packages("slam")
install.packages("SnowballC")
install.packages("reshape2")


library(data.table)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(tm)
library(topicmodels)
library(slam)
library(tidyr)
library(tidytext)
library(SnowballC)

get_sentiments("bing")

#get_sentiments("bing")

# 3.Read the file: data <- fread("psychcentral_data.csv", sep=",", header=T, 
#strip.white = T, na.strings = c("NA","NaN","","?"))

data <- read.csv("psychcentral_data.csv", sep=",", header=T, 
              strip.white = T, na.strings = c("NA","NaN","","?"))

data$id <- seq.int(nrow(data))

# 3.1.	(1 point) What are the column names in the data? 
colnames(data)
# "row", "q_subject", "q_content", "answers", "id"

# 3.2.	(1 point) How many rows does this data have? 

nrow(data)

#8360

#4.Use libraries “dplyr” and “tidytext” to tokenize column q_content. 
#Then remove the stop-words. The count the number of tokens. 

qcontent_text <- data %>%
  unnest_tokens(word, q_content)

data("stop_words")

qcontent_stopwords <- qcontent_text %>% 
  anti_join(stop_words, by = "word")

qcontent_counted <- qcontent_stopwords %>% 
  count(id, word, sort = TRUE) # count the number of the word in the document

#4.1.(2 points) What are the top five tokens returned? 
qcontent_stopwords %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 5)

# im, dont, feel, time, life

#4.2.(2 points) Use library “ggplot2” to create a visualization that shows the 
#frequency of the tokens that appeared for at least 2000 times. 
#(Hint: Change n in argument filter to 2000). Paste the visualization below:

qcontent_stopwords %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 2000) %>% # top words (fre > 2000)
  mutate(word = reorder(word, n)) %>% # reorder word based on fre
  ggplot(aes(word, n)) + 
  geom_bar(stat =  "identity") + 
  xlab(NULL) + 
  coord_flip()

#4.3.(2 points) Based on the results in 4.2., would you suggest stemming on this text? 
#Why? Bring one example from the visualization above that shows stemming should be done 
#on this text?  

# the best example I can find is many z words with the "zon" prefix, there seem to be many words that can
# be condensed to stems in the 513,425 observations

#4.4.	Install “SnowballC” package using 
#install.packages("SnowballC", repos = "https://cran.r-project.org"). 
#Use library “SnowballC” to stem q_content. 
qcontent_stemmed <- qcontent_text %>% 
  mutate(word2 = wordStem(word))

n_distinct(unique(qcontent_stemmed$word))
n_distinct(unique(qcontent_stemmed$word2))

#4.4.1.(2 points) Then remove the stop-words. Now what are the top five tokens after stemming?
stemmed_text <- qcontent_stemmed %>% 
  anti_join(stop_words, by = "word")

stemmed_counted <- stemmed_text %>% 
  count(id, word2, sort = TRUE)

stemmed_counted %>%
  count(word2, sort = TRUE) %>%
  slice_head(n = 5)

#dont, feel, im, time, friend

#4.4.2.(2 points) Use library “ggplot2” to create a visualization that shows the 
#frequency of the tokens that appeared for at least 4000 times. 
#(Hint: Change n in argument filter to 4000). Paste the visualization below:

stemmed_text %>% 
  count(word2, sort = TRUE) %>% 
  filter(n > 4000) %>% # top words (freq > 4000)
  mutate(word2 = reorder(word2, n)) %>% # reorder word based on freq
  ggplot(aes(word2, n)) + 
  geom_bar(stat =  "identity") + 
  xlab(NULL) + 
  coord_flip()

#4.4.3.(3 points) Use library “wordcloud” to create a word cloud with the 200 most used tokens. 
#Paste the visualization below:

stemmed_text%>% 
  count(word2, sort = T)%>%
  with(wordcloud(word2, n, max.words = 200))
  
#4.4.4.(5 points) Create a color-coded word cloud based on sentiment. Use the most 
#frequent 100 tokens for positive and negative words. Paste the word cloud in the space below:

stemmed_text%>%
  count(word2, sort = T)%>%
  with(wordcloud(word2, n, max.words = 100, colors = brewer.pal(8,"Dark2")))

stemmed_text%>%
  inner_join(get_sentiments("bing"))%>%
  count(word2, sentiment, sort = T)%>%
  acast(word2 ~ sentiment, value.var = "n", fill = 0)%>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)


#5.Repeat all the steps in question 4 but this time for column answers.

answers_text <- data %>%
  unnest_tokens(word, answers)

data("stop_words")

answers_stopwords <- answers_text %>% 
  anti_join(stop_words, by = "word")

answers_counted <- answers_stopwords %>% 
  count(id, word, sort = TRUE) 

#5.1.(2 points) What are the top five tokens returned?
answers_stopwords %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 5)

# dont, feel, people, youre, time

#5.2.	(2 points) Use library “ggplot2” to create a visualization that shows the frequency 
#of the tokens that appeared for at least 4000 times. 
#(Hint: Change n in argument filter to 4000). Paste the visualization below:

answers_stopwords %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 4000) %>% # top words (fre > 4000)
  mutate(word = reorder(word, n)) %>% # reorder word based on fre
  ggplot(aes(word, n)) + 
  geom_bar(stat =  "identity") + 
  xlab(NULL) + 
  coord_flip()
  
#5.3.	Install “SnowballC” package using 
#install.packages("SnowballC", repos = "https://cran.r-project.org"). 
#Use library “SnowballC” to stem answers. 

answers_stemmed <- answers_text %>% 
  mutate(word2 = wordStem(word))

n_distinct(unique(answers_stemmed$word))
n_distinct(unique(answers_stemmed$word2))


#5.3.1.(2 points) Then remove the stop-words. Now what are the top five tokens after stemming?

answers_stemmed_stopwords <- answers_stemmed %>% 
  anti_join(stop_words, by = "word")

answers_stemmed_stopwords %>%
  count(word2, sort = TRUE) %>%
  slice_head(n = 5)

# feel, dont, time, peopl, your
#5.3.2.(2 points) Use library “ggplot2” to create a visualization that shows the 
#frequency of the tokens that appeared for at least 6000 times. 
#(Hint: Change n in argument filter to 6000). Paste the visualization below:

answers_stemmed_stopwords %>% 
  count(word2, sort = TRUE) %>% 
  filter(n > 6000) %>% # top words (fre > 6000)
  mutate(word2 = reorder(word2, n)) %>% # reorder word based on fre
  ggplot(aes(word2, n)) + 
  geom_bar(stat =  "identity") + 
  xlab(NULL) + 
  coord_flip()
  
#5.3.3.(6 points) Use library “wordcloud” to create a word cloud with the 200 most used tokens. 
#Paste the visualization below:

answers_stemmed_stopwords%>% 
  count(word2, sort = T)%>%
  with(wordcloud(word2, n, max.words = 200))

  
#5.3.4.(6 points) Create a color-coded word cloud based on sentiment. 
#Use the most frequent 100 tokens for positive and negative words. 
#Paste the word cloud in the space below:
  
answers_stemmed_stopwords%>%
  count(word2, sort = T)%>%
  with(wordcloud(word2, n, max.words = 100, colors = brewer.pal(8,"Dark2")))

answers_stemmed_stopwords%>%
  inner_join(get_sentiments("bing"))%>%
  count(word2, sentiment, sort = T)%>%
  acast(word2 ~ sentiment, value.var = "n", fill = 0)%>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)

#6.	Use the following code to perform topic-modeling on q_content:
library(RTextTools)
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$q_content), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 5) # k is the number of topics to be found.

#6.1.(5 points) The code above will create the beta scores for each document per topic (k = 5). 
#Then create bar plots (similar to what we created in class) for each topic for 10 tokens 
#(top_n(10, beta)). Paste the visualization below. 


qlda_td <- tidy(lda)

qk5_terms <- qlda_td %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

qk5_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#6.2.	(5 points) Based on the visualization in 6.1., what can you say about k? Would you try a larger k or a smaller k? 

# I would do a larger k as there is a lot more not shown in the plot, having it across more across the set beta will give a better scope

#6.3.(10 points) Repeat 6.1. with the following ks:

#6.3.1.	 K = 2. Paste your visualization in the space below:
qlda_2<- LDA(dtm.new, k = 2)

qlda_td2 <- tidy(qlda_2)

qk2_terms <- qlda_td2 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

qk2_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()
  
#6.3.2.	 K = 3. Paste your visualization in the space below:

qlda_3<- LDA(dtm.new, k = 3)

qlda_td3 <- tidy(qlda_3)

qk3_terms <- qlda_td3 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

qk3_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()
  
#6.3.3.	 K = 4. Paste your visualization in the space below:
  
qlda_4<- LDA(dtm.new, k = 4)

qlda_td4 <- tidy(qlda_4)

qk4_terms <- qlda_td4 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

qk4_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#6.3.4.	 K = 10. Paste your visualization in the space below:
qlda_10<- LDA(dtm.new, k = 10)

qlda_td10 <- tidy(qlda_10)

qk10_terms <- qlda_td10 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

qk10_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#6.3.5.	 Based on the results recommend the number of topics that would be appropriate 
#for this corpus. 

#I'd say from all the visualizations listed, 10 would be the best as it states all the topics for each instance

#7.	Create a LDA model with k = 10 
#(use the same group of codes from question 6 and modify the topic number) 
#and answer the following answers: 
corpus2 <- Corpus(VectorSource(data$answers), readerControl=list(language="en"))
dtm2 <- DocumentTermMatrix(corpus2, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals2 <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm.new2   <- dtm2[rowTotals2> 0, ] #remove all docs without words

#7.1.(5 points) The LDA model will create the beta scores for each document per topic (k = 10). 
#Then create bar plots (similar to what we created in class) for each topic for 10 tokens (top_n(10, beta)). 
#Paste the visualization below. 
alda_10<- LDA(dtm.new2, k = 10)
alda_td10 <- tidy(alda_10)

ak10_terms <- alda_td10 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

ak10_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#7.2.(5 points) Based on the visualization in 6.1., are the tokens in all topics similar? 
#Then what can you say about k? Would you try a larger k or a smaller k? 

#I would do a smaller K as there are a lot of similar values among the five so I would make it smaller

#7.3.(10 points) Repeat 6.1. with the following ks:

#7.3.1.	 K = 2. Paste your visualization in the space below:
  
alda_2<- LDA(dtm.new2, k = 2)
alda_td2 <- tidy(alda_2)

ak2_terms <- alda_td2 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

ak2_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#7.3.2.	 K = 8. Paste your visualization in the space below:
  
alda_8<- LDA(dtm.new2, k = 8)
alda_td8 <- tidy(alda_8)

ak8_terms <- alda_td8 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

ak8_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#7.3.3.	 K = 11. Paste your visualization in the space below:
alda_11 <- LDA(dtm.new2, k = 11)
alda_td11 <- tidy(alda_11)

ak11_terms <- alda_td11 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

ak11_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#7.3.4.	 K = 14. Paste your visualization in the space below:
  
alda_14 <- LDA(dtm.new2, k = 14)
alda_td14 <- tidy(alda_14)

ak14_terms <- alda_td14 %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(topic, -beta)

ak14_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") + 
  scale_y_reordered()

#7.3.5.	 Based on the results recommend the number of topics that would be appropriate for 
#this corpus. 

#Personally I think 4 is the best of them all but from the ones of that were listed, 10 would be the middle point
#It doesn't describe too little like k = 2 does and has less than the other K's listed

#8.(20 points) Suppose that you are a researcher who works for National Institutes of Health (NIH).
#You are working on a project that aims to identify the most important reasons for 
#mental disorders. Based on your analysis above, can we propose any hypothesis about the 
#reasons for mental disorders in the society? Please explain. 

#We can see from the terms that there are certain words that are said over and over from people with mental disorders
#People that cab see that help, think, don't, and feel are top words on multiple LDA's give more of negative sentiment.
#This gives cause that specific words correlate to a potential mental disorder that could be affecting their way of speech.
#The occurrence of these words are too frequent, where it is possible that in the subtle way of they speak could indicate
#some form of a mental disorder.

