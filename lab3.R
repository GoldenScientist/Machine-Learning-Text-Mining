#from https://en.wikipedia.org/wiki/Big_data
txt = c(
"Growth of and digitization of global",
"information-storage capacity",
"Big data refers to data sets",
"that are too large or complex",
"for traditional data-processing")


library(dplyr)
text_d = tibble(line=1:5, text= txt)





#install.packages('tidytext')
library(tidytext)
text_d %>%  unnest_tokens(word, text) %>% head(5)

#
data(stop_words)
stop_words %>% head(100)

text_d %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#install.packages('SnowballC')
#library(SnowballC)
text_d %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  mutate(stem=wordStem(word)) %>%
  head(5)

install.packages('textstem')
library(textstem)
test_a = text_d %>%
  unnest_tokens(word, text) %>%
   anti_join(stop_words) %>%
   mutate(lemma = lemmatize_words(word),
   	stem=wordStem(word))
test_a %>% head(5)

correction = function(x,txt=txt) 
	names(which.min(sapply(
		test_a$word,adist,x)))


correction("growt")



############
text_d %>% unnest_tokens(ngram,
 text, token = "ngrams", n = 3)


####
install.packages("gutenbergr")
library(gutenbergr)
shakespeare = gutenberg_works(author == "Shakespeare, William")
works = gutenberg_download(shakespeare$gutenberg_id)
options(dplyr.print_max = 1e3)
#works = gutenberg_download(shakespeare$gutenberg_id)

################
options(dplyr.width = Inf)
works %>% sample_n(10)

works %>%
unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  head(200)

allcounts = works %>%
unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
plot(allcounts$n,log="xy")

allcounts[200:300,]

####
cts = works %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(word %in% allcounts$word[200:220]) %>%
  count(gutenberg_id, word, sort = TRUE)

cts = cts %>% left_join(shakespeare %>% select(
	gutenberg_id,title)) %>% 
	select(-gutenberg_id) %>%
	spread(title, n)
cts[is.na(cts)]=0
cts 


###
cts = works %>%
  unnest_tokens(word, text) %>%
  filter(word %in% c('fool','battle')) %>%
  count(gutenberg_id, word, sort = TRUE) %>%
  spread(word,n) 
cts

plot(cts$battle,cts$fool, type="n")
text(cts$battle,cts$fool, cts$gutenberg_id)

works %>%
  unnest_tokens(word, text) %>%
  filter(word %in% c('fool','battle')) %>%
  count(gutenberg_id, word, sort = TRUE) %>% 
  cast_dfm(gutenberg_id, word, n)  %>%
  as.data.frame
#############################
###
tfidf = works %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(gutenberg_id, word, sort = TRUE) %>%
  bind_tf_idf(word, gutenberg_id,n)

tfidf %>% arrange(desc(tf_idf)) %>% head(5)

library(lsa)
foolbattle = tfidf %>% 
	filter(word %in% c('fool','battle',
		'feere','poore')) %>%
	cast_dfm(gutenberg_id, word, tf_idf) %>%
	as.data.frame()

similarity = dist(t(as.matrix(foolbattle)) )
cluster = kmeans(1- similarity, 4)


plot(foolbattle[,1:2], col=cluster$cluster)
text(foolbattle[,1:2], cts$gutenberg_id,
	col=cluster$cluster)

###


###


trigs = works %>%
 unnest_tokens(trigram, text,
  token = "ngrams", n = 3) %>% 
	count(trigram) %>% arrange(desc(n))

library(tidyr)
trigs = trigs %>% separate(trigram,c("a", "b","c"),
	extra = "drop",sep = " ") 

trigs %>% filter(a=="farewell" & b=="and")

next_word = function (x,y){
	df1 = trigs %>% filter(a==x & b==y)
	index = sample.int(nrow(df1), 
		1, prob = df1$n)
	return(df1$c[index])
}

set.seed(21329)

next_word("farewell","and")

create_phrase = function(x,y,n=10){
	phrase = paste(x,y)
	for (i in 1:(n-2)){
		z = next_word(x,y)
		x = y
		y = z
		phrase = paste(phrase,z)
	}
	return(phrase)
}

set.seed(213243)

create_phrase("farewell","and",n=10)




###

gutenberg_works(author == "Marlowe, Christopher")