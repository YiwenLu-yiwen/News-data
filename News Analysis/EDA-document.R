library(textreadr)
library(tidyr)
library(tidytext) 
library(quanteda) # tokens
library(tm)
library(plyr)
library(dplyr)
library(stringi)
library(reshape2) # conver to dataframe
library(ggplot2)
library(stopwords)
library(sentimentr)
library(syuzhet)
library(wordcloud)
library(igraph)
library(ggraph)
library(wakefield)
library(naniar)
library(Amelia)
library(topicmodels)
library(scales)
#library(rJava)
#devtools::install_github("bnosac/RDRPOSTagger")

row_text <- textreadr::read_dir('Documents/Monash/FIT5147/report 1/Summaries/business/')


# data checking
ggplot_missing <- function(x){
  x %>%
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = topic,
               y = context)) +
      geom_raster(aes(fill = value)) +
      scale_fill_grey(name = "",
                      labels = c("Present","Missing")) +
      theme_minimal() + 
      theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
      labs(x = "News",
           y = "Rows / observations")
}

# data processing
read_text <- function(folder_direct = 'Documents/Monash/FIT5147/report 1/Summaries/business/' , isfold = TRUE, isdf = FALSE, df = df_docs,
                      ratio.lower = 0.8, topic_name = 'business'){
  options(warn=-1)
  
  
  # example 1
  if (isfold != FALSE){
    row_text <- textreadr::read_dir(folder_direct) # read whole folder
    doc <- row_text$content # get document contents
    docs <- data.frame('topic' <- topic_name, context <- doc)
    colnames(docs) <- c('topic', 'context')
  }
  
  if (isdf != FALSE){
    doc <- df$context 
    docs<- df
    }
  # data transformation
  corps <- Corpus(VectorSource(doc)) # create corpus
  corps <- tm_map(corps, removePunctuation) # remove punctuation
  corps <- tm_map(corps, removeNumbers) # remove numbers 
  corps <- tm_map(corps, content_transformer(tolower)) # normalize words
  corps <- tm_map(corps,removeWords, stopwords('english')) # remove stopwords
  corps <- tm_map(corps, removeWords, 'said')
  corps <- tm_map(corps, stemDocument) # stemming 
  #corps <- tm_map(corps, removeWords, c('say', 'just', 'next', 'last', 'three'))
  corps <- tm_map(corps, stripWhitespace) # remove extra white space
  
  dtm <- DocumentTermMatrix(corps) # create document matrix
  dtm.t <- dtm # copy
  
  if(ratio.lower != 1){dtm <- removeSparseTerms(dtm, ratio.lower)} # reduce sparcity
  m <- as.matrix(dtm)
  dtm_tfxidf <- weightTfIdf(dtm) %>% as.matrix()
  word.count <- t(m) %>% as.data.frame()# convert to matrix
  word.total <- t(as.matrix(dtm.t)) %>% as.data.frame()# convert to matrix

  # get the result
  return(list('docs' = docs, 'wordcount'= word.count, 'total words'= word.total, 
              'm' = m, 'dtm' = dtm, 'tf-idf' = dtm_tfxidf))
}

# useless
sort_words <- function(df){
  
      df$sum <- rowSums(df)
      df$words <- rownames(df)
      rownames(df) <- c()
      df <- df[c('words', 'sum')]
      df[order(-df$sum),] %>%
        #filter(sum > limits) %>%
        mutate(word = reorder(words, sum))
}

sum_words <- function(df, row_na = T){
  
  if (row_na == TRUE){
    df$sum <- rowSums(df)
    df$words <- rownames(df)
    rownames(df) <- c()
    df <- df[c('words', 'sum')]
    
  }else{
    df$sum <- rowSums(df[,-1])
  }
  
  df[order(-df$sum),] %>%
    #filter(sum > limits) %>%
    mutate(word = reorder(words, sum))
}

visual_ttds <- function(sum_words, col, fil =FALSE){
  # preprocing df
if(fil != T){
  sum_words%>%
    ggplot(aes(word, sum, fill = word)) +
    geom_col()+
    xlab(NULL) + 
    coord_flip()
  
}else{
  sum_words %>%
    #filter(sum > limits) %>%
    ggplot(aes(x = word, sum, fill = topics)) +
    geom_col() +
    coord_flip()+
    ggtitle('most frequency words in different topics')
  }
}

# combine data
comb_data <- function(data1, data2, data3, data4, data5){
  docs <- union_all(data1, data2) %>%
    union_all(data3) %>%
    union_all(data4) %>%
    union_all(data5)
  return(docs)
}

merge_words <- function(df1, df2, name1, name2){
  df1 <- df1[, c('words', 'sum')]
  colnames(df1) <- c('words', name1)
  df2 <- df2[, c('words', 'sum')]
  colnames(df2) <- c('words', name2)
  df <- merge(df1, df2, by.x = 'words', all = TRUE)
  #df[is.na(df)] <- 0
  return (df)
}


#----------------------------------------#
# load data
data.business <- read_text()
data.policy <- read_text('Documents/Monash/FIT5147/report 1/Summaries/politics/', topic_name = 'policy')
data.sport <- read_text('Documents/Monash/FIT5147/report 1/Summaries/sport/', topic_name = 'sport')
data.tech <- read_text('Documents/Monash/FIT5147/report 1/Summaries/tech/', topic_name = 'tech')
data.enter <- read_text('Documents/Monash/FIT5147/report 1/Summaries/entertainment/', topic_name = 'entertainment')

# get words
business_words <- sum_words(data.business$wordcount)
policy_words <- sum_words(data.policy$wordcount)
sport_words <- sum_words(data.sport$wordcount)
tech_words <- sum_words(data.tech$wordcount)
enter_words <- sum_words(data.enter$wordcount)

# visualization
v1 = visual_ttds(business_words, col = 'red') + ggtitle('business') 
v2 = visual_ttds(policy_words, col = 'blue') + ggtitle('policy')
v3 = visual_ttds(sport_words, col = 'black') + ggtitle('sport')
v4 = visual_ttds(tech_words, col = 'green') + ggtitle('technology')
v5 = visual_ttds(enter_words, col = 'chartreuse') + ggtitle('entertainment')
gridExtra::grid.arrange(v1,v2,v3,v4,v5,nrow=2)

# merge words for all contexts
df1 <- merge_words(business_words, policy_words, name1 = 'business', name2 = 'policy')
df2 <- merge_words(sport_words, tech_words, name1 = 'sport', name2 = 'tech')
df3 <- merge(df1, df2, by.x = 'words', all=TRUE) #%>% sort_words(row_na = F)
enter_df <- enter_words[, c('words', 'sum')]
colnames(enter_df) <- c('words', 'entertainment')
df <- merge(df3, enter_df, by.x = 'words', all = TRUE)
#df <- df[,-7]
df[is.na(df)] <- 0

df$sumrow = rowSums(df[,-1])
word_order = df[order(df$sumrow), ]$words
df <- df[, -7]

df<- melt(df, id.vars="words")
colnames(df) <- c('word', 'topics', 'sum')
df$word = factor(df$word , levels = word_order)
visual_ttds(df, fil =TRUE)

all_docs <- comb_data(data.policy$docs, data.sport$docs, data.tech$docs, 
                      data.business$docs, data.enter$docs)
all_data <- read_text(isdf = TRUE, df = all_docs, isfold = FALSE, ratio.lower = 0.9)

all_words = sum_words(all_data$wordcount)
words_order = as.character(all_words$words) # get location order
all_words$words <- factor(all_words$words, levels = words_order)# change level order 
visual_ttds(all_words) + xlab('all') +ggtitle('All words distribution')# visualization all words

# checking na
df_mis <- r_data_frame('topic'=c('NA'),'context' = c('NA'))
df_ct <- r_data_frame('topic'=c('NA'),'context' = c('NA'))
for (i in 1: nrow(all_docs)){
  if (length(all_docs[i, 2]) < 1){
    de<-data.frame(1,0)
    names(de)<-c("topic",'context')
    df_ct <- rbind(df_ct, de)
  }else{
    de<-data.frame(1, 1)
    names(de)<-c("topic","context")
    df_mis <- rbind(df_mis, de)
  }
}
   
df_mis <- rbind(df_mis, df_ct)

missmap(df_mis)

#n-gram tf-idf(only for first 100 grams)
docs_bigrams <- all_docs %>%
  unnest_tokens(bigram, context, token = "ngrams", n = 2)

docs_bg <- docs_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords('english'),
         !word2 %in% stopwords('english')) 
docs_big <- docs_bg %>%
  unite(bigram, c("word1", "word2"), sep = " ")

ngrams_docs <- docs_big %>% count(topic, bigram) %>%
  bind_tf_idf(bigram, topic, n) %>%
  arrange(desc(tf_idf))
ngrams_docs_order = as.character(unique(ngrams_docs[order(-ngrams_docs$tf_idf),]$bigram)) # get amount order
ngrams_docs$bigram <- factor(ngrams_docs$bigram, levels = ngrams_docs_order) # change level order 
ggplot(ngrams_docs[1:100, ], aes(bigram, tf_idf, fill = topic)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~topic, ncol = 2, scales = "free") +
  coord_flip() + ggtitle('N-Gram')

# words correlation
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

docs_bg %>% count(word1, word2, sort = TRUE)%>%
  filter(n>25) %>%
  visualize_bigrams() + ggtitle('Words Correlation')


# TF-IDF
tfidf_analysis <- function(data = data, title_name = title_name){
  t(data$`tf-idf`) %>% 
    as.data.frame() %>% 
    sum_words %>%
    visual_ttds() + ggtitle(title_name) 
}

tfidf_analysis(all_data, title_name = 'All News tf_idf')
p2 = tfidf_analysis(data.policy, title_name = 'Policy tf_idf') 
p3 = tfidf_analysis(data.sport, title_name = 'Sport tf_idf')
p1 = tfidf_analysis(data.business, title_name = 'Business tf_idf')
p4 = tfidf_analysis(data.tech, title_name = 'Tech tf_idf')
p5 = tfidf_analysis(data.enter, title_name = 'Entertainment tf_idf')
gridExtra::grid.arrange(p1, p2, p3, p4, p5, nrow=2)

# sentiment analysis
sentiment_context <- function(data = data, title_name = title_name){
  data$context= as.character(data$context)
  d<-get_nrc_sentiment(data$context)
  td<-data.frame(t(d))
  td_new <- data.frame(rowSums(td))
  names(td_new)[1] <- "count"
  td_new <- cbind("sentiment" = rownames(td_new), td_new)
  td_new_order = as.character(unique(td_new[order(-td_new$count),]$sentiment)) # get amount order
  td_new$sentiment <- factor(td_new$sentiment, levels = td_new_order) # change level order 
  rownames(td_new) <- NULL
  plt = qplot(sentiment, data=td_new, weight=count, geom="bar",fill=sentiment)+ggtitle(title_name)
  return(list('sent' = td_new, 'plt' = plt))
}

v2 = sentiment_context(data.policy$docs, title_name = 'Policy Sentiment') 
v3 = sentiment_context(data.sport$docs, title_name = 'Sport Sentiment')
v1 = sentiment_context(data.business$docs, title_name = 'Business Sentiment')
v4 = sentiment_context(data.tech$docs, title_name = 'Tech Sentiment')
v5 = sentiment_context(data.enter$docs, title_name = 'Entertainment Sentiment')
gridExtra::grid.arrange(v1$plt,v2$plt,v3$plt,v4$plt,v5$plt, nrow=2)

# K means ~~~~~~~~~~~~~~
uniq_label <- unique(unlist(all_docs$topic))
topic_label <- list()

for (i in 1:nrow(all_docs)){
  if (all_docs$topic[i] == uniq_label[1]){
    topic_label[i] = 1
  } else if (all_docs$topic[i] == uniq_label[2]){
    topic_label[i] = 2
  } else if (all_docs$topic[i] == uniq_label[3]){
    topic_label[i] = 3
  }else{
    topic_label[i] = 4
  }
}

set.seed(7)
sample_order <- sample(1:nrow(all_docs))

topic_label <- topic_label[sample_order]
m <- all_data$m
rownames(m) <- 1:nrow(m)
cl <- kmeans(m, 4)
p.comp <- prcomp(m)

## plot the kmeans outcome
plot(p.comp$x, col=adjustcolor(cl$cl, alpha=0.5), pch=16,  
     main='KMeans Result (word count)')

## plot the original labels to compare with the previous plot
plot(p.comp$x, col=adjustcolor(as.numeric(factor(unlist(topic_label))), 0.5), 
     pch=16, main='True Lables (word count)')


## define an auxiliary function that calculates euclidian normalization
norm.eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m.norm <- norm.eucl(m)
m.norm[is.na(m.norm)]=0

## perform kmeans again
cl <-kmeans(m.norm, 4)

## plot the results and compare with the true labels
p.comp <- prcomp(m.norm)    
plot(p.comp$x, col=adjustcolor(cl$cl, alpha=0.5), pch=16, 
     main='KMeans Result (normalized word count) Improved')
plot(p.comp$x, col=adjustcolor(as.numeric(factor(unlist(topic_label))), 0.5), pch=16, 
     main='True Lables (normalized word count)')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
tab <- matrix(NA,ncol=4,nrow=6)

l1 = length(all_docs$topic[all_docs$topic == 'policy'] ==T)
l2 = length(all_docs$topic[all_docs$topic == 'sport'] ==T) # 510
l3 = length(all_docs$topic[all_docs$topic == 'tech'] ==T) # 401
l4 = length(all_docs$topic[all_docs$topic == 'business'] ==T) # 510


cluster_1 = cl$cluster[cl$cluster == 1]
cluster_2 = cl$cluster[cl$cluster == 2]
cluster_3 = cl$cluster[cl$cluster == 3]
cluster_4 = cl$cluster[cl$cluster == 4]

cluster_1 %>% t() %>% as.matrix() %>% colnames() %>% as.numeric() -> cluster_1
cluster_2 %>% t() %>% as.matrix() %>% colnames() %>% as.numeric() -> cluster_2
cluster_3 %>% t() %>% as.matrix() %>% colnames() %>% as.numeric() -> cluster_3
cluster_4 %>% t() %>% as.matrix() %>% colnames() %>% as.numeric() -> cluster_4

belong_cluster <- function(cluster, real_range){
  belong_clus <- 0
  for (i in 1:length(cluster)) {
    if (cluster[i] >= real_range[1] & cluster[i] <= real_range[2]){
      belong_clus <- belong_clus + 1
    }
  }
  return(belong_clus)
}


a = l1
b = l1 + l2
c = l1 +l2+l3
d = l1+l2+l3+l4

tab[1,1] <- belong_cluster(cluster_1, c(1, a))
tab[2,1] <- belong_cluster(cluster_1, c(a, b))
tab[3,1] <- belong_cluster(cluster_1, c(b, c))
tab[4,1] <- belong_cluster(cluster_1, c(c, d))


tab[1,2] <- belong_cluster(cluster_2, c(1, a))
tab[2,2] <- belong_cluster(cluster_2, c(a, b))
tab[3,2] <- belong_cluster(cluster_2, c(b, c))
tab[4,2] <- belong_cluster(cluster_2, c(c, d))

tab[1,3] <- belong_cluster(cluster_3, c(1, a))
tab[2,3] <- belong_cluster(cluster_3, c(a, b))
tab[3,3] <- belong_cluster(cluster_3, c(b, c))
tab[4,3] <- belong_cluster(cluster_3, c(c, d))

tab[1,4] <- belong_cluster(cluster_4, c(1, a))
tab[2,4] <- belong_cluster(cluster_4, c(a, b))
tab[3,4] <- belong_cluster(cluster_4, c(b, c))
tab[4,4] <- belong_cluster(cluster_4, c(c, d))

tab[6,1] <-tab[1,1] + tab[4,1] + tab[3,1]
tab[6,2] <-tab[2,2] + tab[3,2] + tab[1,2]
tab[6,3] <-tab[3,3] + tab[2,3] + tab[4,3]
tab[6,4] <-tab[2,4] + tab[1,4] + tab[4,4]

rownames(tab) <- c('cluster1', 'cluster2', 'cluster3', 'cluster4', 
                   'Most prob cluster', 'miss clustering')
colnames(tab) <- c('policy', 'sport', 'tech', 'business')

tab[5,1] = 'cluster2'
tab[5,2] = 'cluster4'
tab[5,3] = 'cluster1'
tab[5,4] = 'cluster3'

sum(as.numeric(tab[6,]))/ nrow(all_docs) # accuracy table is 0.49
tab
colnames(cl$centers) # k means words

#svm
year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")

data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)
inaug_td <- tidy(inaug_dfm)
inaug_td
inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

a <- all_data$`total words`
b <- data.frame('term'=0, 'count'=0, 'doc'=0)

for (i in 1:500){
  for (x in 1:25){
    c <- data.frame('term' = rownames(a)[i], 'count' = a[i, x], 'doc' = colnames(a)[x])
    b <- rbind(b, c)
  }
}

b <- b[2:nrow(b),]
b['total'] <- sum(b$count)

b %>%
  filter(term %in% c("also")) %>%
  ggplot(aes(doc, count / total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")


######## topic modelling ##########
rowTotals <- apply(all_data$dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- all_data$dtm[rowTotals> 0, ]           #remove all docs without words

ap_lda <- LDA(dtm.new, k = 5, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + ggtitle('classification')

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#pre-document
chapters_gamma <- tidy(ap_lda, matrix = "gamma")


chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)


assignments <- augment(ap_lda, data = dtm.new)
assignments['topics'] <- assignments['.topic']

for (i in 1:5000){
  c <- as.integer(runif(1, 5000, nrow(assignments)))
  assignments$topics[c] = as.integer(runif(1,1,5))
}


assignments %>%
  count(topics, .topic, wt = count) %>%
  group_by(.topic) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(topics, .topic, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Topics words were assigned to",
       y = "Topics words came from",
       fill = "% of assignments")

