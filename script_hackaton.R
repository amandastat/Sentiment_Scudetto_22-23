library(syuzhet)
library(dplyr)
post_fb2 <- post_fb %>%
  filter(`Post Created Date`=="2023-03-23"|`Post Created Date`=="2023-03-22"|`Post Created Date`=="2023-03-21"|
         `Post Created Date`=="2023-03-20"|`Post Created Date`=="2023-03-19"|`Post Created Date`=="2023-03-18"|
         `Post Created Date`=="2023-03-17"|`Post Created Date`=="2023-03-16"|`Post Created Date`=="2023-03-15"|
         `Post Created Date`=="2023-03-14")
get_sentiment_dictionary(dictionary = "syuzhet", language = "italian")
post_fb3 <- post_fb2 %>% arrange(`Post Created`)
forzanapoli_sentiment_fb=get_nrc_sentiment(post_fb3$Message, language = "italian", lowercase = TRUE)
rt2 <- rt %>% arrange(created_at)
forzanapoli_sentiment=get_nrc_sentiment(rt2$text, language = "italian", lowercase = TRUE)
sum(forzanapoli_sentiment$positive)
sum(forzanapoli_sentiment$negative)
mean(forzanapoli_sentiment$positive)
mean(forzanapoli_sentiment$negative)
summary(forzanapoli_sentiment_fb)
sum(forzanapoli_sentimen_fbt$positive)
sum(forzanapoli_sentiment_fb$negative)
mean(forzanapoli_sentiment_fb$positive)
mean(forzanapoli_sentiment_fb$negative)
summary(forzanapoli_sentiment_fb)

s_v <- get_sentences(rt2$text)
s_v_sentiment <- get_sentiment(s_v)
plot(
  s_v_sentiment, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)
s_v_fb <- get_sentences(post_fb3$Message)
s_v_sentiment_fb <- get_sentiment(s_v_fb)
percent_vals_fb <- get_percentage_values(s_v_sentiment_fb, bins = 10)
plot(
  percent_vals_fb, 
  type="l", 
  main="Grafico a linee del sentiment (keyword Scudetto)", 
  xlab = "Blocchi temporali", 
  ylab= "Valenza emozionale", 
  col="red"
)
percent_vals <- get_percentage_values(s_v_sentiment, bins = 20)
plot(
  percent_vals, 
  type="l", 
  main="Grafico a linee del sentiment (keyword Scudetto)", 
  xlab = "Blocchi temporali", 
  ylab= "Valenza emozionale", 
  col="red"
)
dct_values <- get_dct_transform(
  s_v_sentiment, 
  low_pass_size = 5, 
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T
)
plot(
  dct_values, 
  type ="l", 
  main ="Grafico a linee del sentiment (keyword Scudetto)", 
  xlab = "Blocchi temporali", 
  ylab = "Valenza emozionale", 
  col = "red"
)

pwdw <- round(length(s_v_sentiment)*.1)
poa_rolled <- zoo::rollmean(s_v_sentiment, k=pwdw)
bwdw <- round(length(s_v_sentiment_fb)*.1)
bov_rolled <- zoo::rollmean(s_v_sentiment_fb, k=bwdw)
poa_list <- rescale_x_2(poa_rolled)
bov_list <- rescale_x_2(bov_rolled)
plot(poa_list$x, 
     poa_list$z, 
     type="l", 
     col="blue", 
     xlab="Narrative Time", 
     ylab="Emotional Valence")
lines(bov_list$x, bov_list$z, col="red")
### Percentuale di ogni emozione per dataset
forzanapoli_sentiment2 <- forzanapoli_sentiment %>%
  select(- positive, - negative)
barplot(
  sort(colSums(prop.table(forzanapoli_sentiment2[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emozioni presenti nel dataset Scudetto", xlab="Percentuali"
)
forzanapoli_sentiment_fb2 <- forzanapoli_sentiment_fb %>%
  select(- positive, - negative)
barplot(
  sort(colSums(prop.table(forzanapoli_sentiment_fb[, 1:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emozioni presenti nel dataset Scudetto", xlab="Percentuali"
)

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(tidyverse)
library("quanteda")
library(factoextra)
library(FactoMineR)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(stopwords)
library(tm)
library(RColorBrewer)
library(stringr)
rt2=cop27[grepl("^(?!RT)", cop27$text, perl = TRUE),]
rt2$text=as.character(rt2$text)
rt2$text=gsub(pattern = "'",replacement =  " ", rt2$text)
rt2$text=gsub(pattern = "'",replacement =  " ", rt2$text)
rt2$text=gsub(pattern = "'",replacement =  " ", rt2$text)
rt2$text=str_replace_all(rt2$text,"#[a-z,A-Z]*","")
rt2$text=str_replace_all(rt2$text,"@[a-z,A-Z]*","")
rt2$text=str_replace_all(rt2$text,"_[a-z,A-Z]*","")
rt2$text=str_replace_all(rt2$text,'[^[:ascii:]àèéìòùáíóú]',"")
rt2$text=str_replace_all(rt2$text,'\\w*[0-9]+\\w*\\s*',"")
rt2$text=tolower(rt2$text)
rt2$text=str_remove(rt2$text, "httpstco")
rt2$text=str_remove(rt2$text, "the")
rt2$text=str_remove(rt2$text, "for")
rt2$text=str_remove(rt2$text, "and")
rt2$text=str_remove(rt2$text, "that")
rt2$text=str_remove(rt2$text, "può")
rt2$text=str_remove(rt2$text, "win")
rt2$text=str_remove(rt2$text, "quindi")
rt2$text=str_remove(rt2$text, "mai")
rt2$text=str_remove(rt2$text, "que")
rt2$text=str_remove(rt2$text, "solo")
rt2$text=str_remove(rt2$text, "così")
rt2$text=str_remove(rt2$text, "già")
rt2$text=str_remove(rt2$text, "poi")
rt2$text=str_remove(rt2$text, "però")
rt2$text=str_remove(rt2$text, "cosa")











corpus=corpus(rt2$text, docnames=rt2$id,
              docvars=as.data.frame(rt2$source))
token=tokens(corpus, what = "word", remove_punct = TRUE, remove_symbols = TRUE,
             remove_numbers = TRUE, remove_url = TRUE, remove_separators = TRUE,
             split_hyphens = T, include_docvars = TRUE, padding = FALSE)
token <- tokens_select(token, pattern = stopwords('it'), selection = 'remove')
dfm=dfm(token)
top=textstat_frequency(dfm)
dfm=dfm(token)
dfm_trim=dfm_trim(dfm,min_termfreq = 15)
top=(dfm_trim)
textplot_wordcloud(dfm_trim, comparison = F,min_count = 1, adjust=T,min_size = 1, max_size = 4,
max_words = 100)


docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("italian"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

df <- df[-c(3,4,13,15,16,18,20,22,24,27),]
df <- df[-c(19,20,22,23,24),]
df <- df[-c(29,31,40),]



set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud2(data=df, size=2, color='random-dark')

write.csv(rt, "tweet_scudetto.csv")



