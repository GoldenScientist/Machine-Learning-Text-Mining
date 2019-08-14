library(tidytext)
library(tm)
setwd("C:\\Users\\lebedevd\\Documents\\Teaching\\1819\\ST2ML2\\labs\\datasets\\stanfordSentimentTreebank")


txt = read.table("dictionary.txt", sep="|",as.is=T,
	comment.char = "",blank.lines.skip=F,quote='',
	allowEscapes = T,
	fill=T)

colnames(txt)=c('text','phrase.ids')

target= read.table("sentiment_labels.txt", sep="|",as.is=T,
	comment.char = "",blank.lines.skip=F,quote='',
	allowEscapes = T,
	fill=T,header=T)


library(tidytext)
library(textstem)

prepare_dtm = function(txt,f=cast_dfm){
	data(stop_words)
	test_a = txt %>%
	  unnest_tokens(word, text) %>%
	   anti_join(stop_words) %>%
	   mutate(lemma = lemmatize_words(word))

	return(test_a %>%
  		count(phrase.ids, lemma, sort = TRUE) %>% 
  		f(phrase.ids, lemma, n) )
}
set.seed(21312)
n80 = floor(nrow(txt)*0.8)
train = sample.int(nrow(txt),n80)
train_data = prepare_dtm(txt[train,])

library(dplyr)
train_target= data.frame(
	phrase.ids = as.numeric(rownames(train_data
		)))%>%
	left_join(target)
train_target = train_target$sentiment.values > 0.7

test_data = prepare_dtm(txt[-train,])
test_target= data.frame(
	phrase.ids = as.numeric(rownames(test_data
		)))%>%
	left_join(target)
test_target = test_target$sentiment.values>0.7



library(quanteda)

tmod_nb <- textmodel_nb(train_data,train_target)

dfmat_matched <- dfm_match(test_data, 
	features = featnames(train_data))

predicted_class <- predict(tmod_nb, 
	newdata = dfmat_matched )
table(test_target, predicted_class)




library(xgboost)

bst <- xgboost(data=train_data,
			label=train_target,
			max_depth = 20,
            eta = 1, nthread = 3, 
            nrounds = 100,objective = "binary:logistic")
# plot all the trees
boost_prediction = predict(bst, newdata=dfmat_matched)
#table(boost_prediction>0.5,test_target)
rc = roc(predictor =boost_prediction,
	response = test_target,
	levels = c("TRUE","FALSE") )
plot(rc,add=T,col="red")

###################################
library(text2vec)
tokens = space_tokenizer(txt[train,]$text)
it = itoken(tokens,
	ids = txt[train,]$phrase.ids,
 progressbar = FALSE)
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 5L)
vectorizer = vocab_vectorizer(vocab)
# use window of 5 for context words
tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(word_vectors_size = 100, vocabulary = vocab, x_max = 10)
# `glove` object will be modified by `fit_transform()` call !
wv_main = fit_transform(tcm, glove, n_iter = 20)
wv_context = glove$components
dim(wv_context)
word_vectors = wv_main + t(wv_context)

dtm =it %>% create_dtm(vectorizer)
dtm = normalize(dtm)

library(Matrix)
document_vecs = dtm %*% word_vectors


train_target= data.frame(
	phrase.ids = as.numeric(rownames(dtm
		)))%>%
	left_join(target)
	
train_target = train_target$sentiment.values > 0.7


classifier <- naiveBayes(as.matrix(document_vecs),
	train_target)



test_tokens = space_tokenizer(txt[-train,]$text)
test_it = itoken(test_tokens,
	ids = txt[-train,]$phrase.ids,
 progressbar = FALSE)

test_dtm =test_it %>% create_dtm(vectorizer)
test_dtm = normalize(test_dtm)

test_target= data.frame(
	phrase.ids = as.numeric(rownames(test_dtm
		)))%>%
	left_join(target)
	
test_target = test_target$sentiment.values > 0.7

test_vecs = test_dtm %*% word_vectors

tst = predict(classifier,newdata= 
	as.matrix(test_vecs))

table(tst, test_target)
