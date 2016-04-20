## best 0.48702
setwd("/Users/QinqingLiu/Documents/2016 Spring/6494 Advanced Stat Computing/Project/code")

library(stringr)
# library(RTextTools)
library(SnowballC)
library(xgboost)
library(plyr)
library(tau)
library(foreign)
library(dplyr)
library(randomForest)
library(data.table)
# library(SparkR)
# library(rPython)

rm(list = ls())
Sys.setlocale(locale="C")


cat("Reading data\n")
train <- read.csv('../input/train.csv', encoding="UTF-8"
                  # ,fileEncoding="UCS-2LE"
                  )
test <- read.csv('../input/test.csv', encoding="UTF-8"
                 # ,fileEncoding="UCS-2LE"
                 )
desc <- read.csv('../input/product_descriptions.csv', encoding="UTF-8")
attr <- read.csv('../input/attributes.csv', encoding="UTF-8")



common_word <- function(str1, str2){
  count <- 0
  str1 <- as.character(str1)
  str2 <- as.character(str2)
  words <- unlist(strsplit(str1, split = " "))
  for( i in 1:length(words)){
    # pattern <- words[i]
    pattern <- paste("(^| )",words[i],"($| )",sep="")
    count <- count + grepl(pattern, str2, perl=TRUE,ignore.case=TRUE)
  }
  return(count)
}

common_word2 <- function(search, str){
  count <- 0
  search <- as.character(search)
  str <- as.character(str)
  words <- unlist(strsplit(search, split = " "))
  for( i in 1:length(words)){
    pattern <- words[i]
    # pattern <- paste("(^| )",words[i],"($| )",sep="")
    count <- count + grepl(pattern, str,ignore.case=TRUE)
  }
  return(count)
}

word_stem <- function(str){
  i <- 1
  str <- as.character(str)
  words <- unlist(strsplit(str, split = " "))
  nwords <- length(words)
  pattern <- wordStem(words[i], language = "porter")
  for(i in 2:length(words)){
    pattern <- paste(pattern, wordStem(words[i], 
                                       language = "porter"), 
                     sep=" ")
  }
  return(pattern)
}

instr <- function(str1,str2,startpos=1,n=1,perl=TRUE){
  aa=unlist(strsplit(substring(str1,startpos),str2,perl=TRUE))
  if(length(aa) < n+1 ) return(-1);
  return(sum(nchar(aa[1:n])) + startpos+(n-1)*nchar(str2) )
}

str_whole_word <- function(str1, str2, i){
  count <- 0
  if (is.na(str1)) return(count)
  while(i < nchar(str2)){
    # i_ <- regexpr(str1, str2)
    # i_ <- str_locate(str1, str2)
    i <- instr(str2, str1, i)
    # i_ = str2.find(str1, i_) python
    if(i == -1) {
      return(count)
    }
    else{
      count <- count + 1
      i <- i + nchar(str1)
    }
  }
  return(count)
}

trans_ms <- function(str){
  str <- as.character(str)
  str <- gsub("/", " / ", str, fixed = TRUE)
  str <- gsub("-", " ", str, fixed = TRUE)
  str <- str_to_lower(str)
  return(str)
}

trans <- function(str){
  
  # s = re.sub(r"(\w)\.([A-Z])", r"\1 \2", s) 
  ##'desgruda' palavras que est??o juntas
  
  str <- as.character(str)
  
  str <- str_to_lower(str)
  
  # group$group.no.e <- str_replace_all(group$group, "e", "")
  
  str <- gsub(" x ", " xby ", str, fixed = TRUE)
  str <- gsub(" * ", " xby ", str, fixed = TRUE)
  str <- gsub(" by ", " xby ", str, fixed = TRUE)
  
  str <- gsub(" zero ", " 0 ", str, fixed = TRUE)
  str <- gsub(" one ", " 1 ", str, fixed = TRUE)
  str <- gsub(" two ", " 2 ", str, fixed = TRUE)
  str <- gsub(" three ", " 3 ", str, fixed = TRUE)
  str <- gsub(" four ", " 4 ", str, fixed = TRUE)
  str <- gsub(" five ", " 5 ", str, fixed = TRUE)
  str <- gsub(" six ", " 6 ", str, fixed = TRUE)
  str <- gsub(" seven ", " 7 ", str, fixed = TRUE)
  str <- gsub(" eight ", " 8 ", str, fixed = TRUE)
  str <- gsub(" nine ", " 9 ", str, fixed = TRUE)
  
  
  str <- gsub(" x0 ", " xby 0 ", str, fixed = TRUE)
  str <- gsub(" x1 ", " xby 1 ", str, fixed = TRUE)
  str <- gsub(" x2 ", " xby 2 ", str, fixed = TRUE)
  str <- gsub(" x3 ", " xby 3 ", str, fixed = TRUE)
  str <- gsub(" x4 ", " xby 4 ", str, fixed = TRUE)
  str <- gsub(" x5 ", " xby 5 ", str, fixed = TRUE)
  str <- gsub(" x6 ", " xby 6 ", str, fixed = TRUE)
  str <- gsub(" x7 ", " xby 7 ", str, fixed = TRUE)
  str <- gsub(" x8 ", " xby 8 ", str, fixed = TRUE)
  str <- gsub(" x9 ", " xby 9 ", str, fixed = TRUE)
  str <- gsub(" 0x ", " 0 xby ", str, fixed = TRUE)
  str <- gsub(" 1x ", " 1 xby ", str, fixed = TRUE)
  str <- gsub(" 2x ", " 2 xby ", str, fixed = TRUE)
  str <- gsub(" 3x ", " 3 xby ", str, fixed = TRUE)
  str <- gsub(" 4x ", " 4 xby ", str, fixed = TRUE)
  str <- gsub(" 5x ", " 5 xby ", str, fixed = TRUE)
  str <- gsub(" 6x ", " 6 xby ", str, fixed = TRUE)
  str <- gsub(" 7x ", " 7 xby ", str, fixed = TRUE)
  str <- gsub(" 8x ", " 8 xby ", str, fixed = TRUE)
  str <- gsub(" 9x ", " 9 xby ", str, fixed = TRUE)
  
  str <- gsub(" xby|for|and|in|the|on|sku|with|what|from|that ",
              " ", str, fixed = TRUE)
  
  str <- gsub("(|)", " ", str, fixed = TRUE)
  

  
  
  str <- gsub("([0-9]+)( *)(inches|inch|in|in.|')", "\\1in. ", str)
  str <- gsub("([0-9]+)( *)(foot|feet|ft|ft.|'')", "\\1ft. ", str)
  str <- gsub("([0-9]+)( *)(pounds|pound|lbs|lb|lb.)", "\\1lb. ", str)
  
  str <- gsub("([0-9]+)( *)(square|sq|sq.)([ ])(feet|foot|ft|ft.)", "\\1sq.ft. ", str)
  str <- gsub("([0-9]+)( *)(cu|cubic|cu.)([ ])(feet|foot|ft|ft.)", "\\1cu.ft. ", str)
  str <- gsub("([0-9]+)( *)(gallons|gallon|gal|gal.)", "\\1gal. ", str)
  str <- gsub("([0-9]+)( *)(ounces|ounce|oz|oz.)", "\\1oz. ", str)
  str <- gsub("([0-9]+)( *)(centimeters|cm|cm.)", "\\1cm. ", str)
  str <- gsub("([0-9]+)( *)(milimeters|mm|mm.)", "\\1mm. ", str)
  str <- gsub("([0-9]+)( *)(degrees|degree|deg.)", "\\1deg. ", str)
  str <- gsub("([0-9]+)( *)(volts|volt|volt.)", "\\1volt. ", str)
  str <- gsub("([0-9]+)( *)(watts|watt|watt.)", "\\1watt. ", str)
  str <- gsub("([0-9]+)( *)(amperes|ampere|amps|amp|amp.)", "\\1amp. ", str)
  
  str <- gsub(",", "", str)
  
  str <- gsub("whirpool", "whirlpool", str)
  str <- gsub("whirpoolga", "whirlpool", str)
  str <- gsub("whirpoolstainless", "whirlpool stainless", str)
  str <- gsub("BagsAt", "bags at", str)
  str <- gsub(" vinal ", " vinyl ", str, fixed = TRUE)
  str <- gsub(" vynal ", " vinyl ", str, fixed = TRUE)
  str <- gsub(" skil ", " skill ", str, fixed = TRUE)
  
  str <- gsub("  ", " ", str)
  
  # s = (" ").join([stemmer.stem(z) for z in s.split(" ")])
  
  return(str)
}


# Combine the train data and test data
train$type <- "train"
test$type <- "test"
test$relevance <- 1
data_all <- rbind(train, test)

# extract some features from attribute
brand <- attr[which(attr$name == "MFG Brand Name"),]
material <- attr[which(grepl("(Material|material)", attr$name)>0 ) ,]
# material <- ddply(material, .(product_uid), summarize, value = paste(value))
shape <- attr[which(grepl("(Shape|shape)", attr$name)>0 ) ,]
# shape <- ddply(shape, .(product_uid), summarize, value = paste(value))

material$value <- as.character(material$value)
shape$value <- as.character(shape$value)
### dplyr
# material <- material %>%
#   group_by(product_uid) %>%
#   summarise(value=paste(value, collapse=' '))
# 
# shape %>%
#   group_by(product_uid) %>%
#   summarise(value=paste(value, collapse=' '))

### data.table
material <- setDT(material)[, list(value=paste(value, collapse=' ')),
                by = product_uid]
shape <- setDT(shape)[, list(value=paste(value, collapse=' ')),
                            by = product_uid]

### base
# aggregate(value~product_uid, material, FUN= paste, collapse=' ') 



cat("Merge description, brand, material, 
    shape with train and test data \n")

data_all <- merge(data_all, desc, by.x = "product_uid", 
               by.y = "product_uid", all.x = TRUE, all.y = FALSE)
data_all <- merge(data_all, brand, by.x = "product_uid", 
               by.y = "product_uid", all.x = TRUE, all.y = FALSE)
data_all <- merge(data_all, material, by.x = "product_uid", 
               by.y = "product_uid", all.x = TRUE, all.y = FALSE)
data_all <- merge(data_all, shape, by.x = "product_uid", 
               by.y = "product_uid", all.x = TRUE, all.y = FALSE)


names(data_all)[names(data_all)=="value.x"] <- "brand"
names(data_all)[names(data_all)=="value.y"] <- "material"
names(data_all)[names(data_all)=="value"] <- "shape"

data_all$name <- NULL

# so far so good

# maybe need revise
data_all[is.na(data_all)] <- 000/000

data_save <- data_all # to simplify the reload process
data_all <- data_save

# str_count Count the number of matches in a string.

# data_all$product_title <- sapply(as.character(data_all$product_title), enc2utf8)
data_all$product_title <- str_replace_all(data_all$product_title,"[^[:graph:]]", " ") 
# levels(data_all$product_title) <- iconv(levels(data_all$product_title), to = "UTF-8" )
# levels(data_save$product_title) <- iconv(levels(data_save$product_title), to = "UTF-8")


data_all$search_term <- sapply(data_all$search_term, trans)
data_all$product_description <- sapply(data_all$product_description, trans)
data_all$product_title <- sapply(data_all$product_title, trans)
data_all$brand <- sapply(data_all$brand, trans_ms)
data_all$material <- sapply(data_all$material, trans_ms)
data_all$shape <- sapply(data_all$shape, trans_ms)

data_all_search_term_stem <- sapply(data_all$search_term, word_stem)
data_all_product_title_stem <- sapply(data_all$product_title,word_stem)
data_all_product_des_stem <- sapply(data_all$product_description,word_stem)

data_save2 <- data_all
data_all <- data_save2

#df_all['product_info'] = df_all['search_term']+"\t"+df_all['product_title'] +"\t"+df_all['product_description']
# data_all_product_info <- str_c(as.character(data_all$search_term), 
#                                as.character(data_all$product_title),
#                                as.character(data_all$product_description),"\t")

# data_all$query_in_title <- mapply(str_whole_word, str2=strsplit(data_all_product_info,"\t")[1] ,str1=strsplit(data_all_product_info,"\t")[2] ,i=0)
# data_all$query_in_des <- mapply(str_whole_word, str2=strsplit(data_all_product_info,"\t")[1], str1 =strsplit(data_all_product_info,"\t")[3],i=0)

  # map equal to apply
# for (k in (1:dim(data_all)[1])){
#   possibleError <- tryCatch(
#     (
#       data_all$query_in_title[k] <- str_whole_word(as.character(data_all$product_title[k]),
#                                                    as.character(data_all$search_term[k]), i=0)
#       data_all$query_in_des[k] <- str_whole_word(as.character(data_all$product_description[k]),
#                                                  as.character(data_all$search_term[k]), i=0 )
#     )
#   )
#   if(inherits(possibleError, "error")) next
#   data_all$query_in_title[k] <- str_whole_word(as.character(data_all$product_title[k]),
#                                       as.character(data_all$search_term[k]), i=0 )
#   data_all$query_in_des[k] <- str_whole_word(as.character(data_all$product_description[k]),
#                                                as.character(data_all$search_term[k]), i=0 )
# }

for (k in (1:dim(data_all)[1])){
  data_all$query_in_title[k] <- str_whole_word(as.character(data_all_search_term_stem[k]),
                                               as.character(data_all_product_title_stem[k]),
                                                    i=0)
  data_all$query_in_des[k] <- str_whole_word(as.character(data_all_search_term_stem[k]),
                                             as.character(data_all_product_des_stem[k]),
                                            i=0 )
}

# python.load("relavent 2.py")
# query_in_title <- python.get("df_all['query_in_title']")
# query_in_des <- python.get("df_all['query_in_description']")


# df_all['query_in_title'] = df_all['product_info'].map(lambda x:str_whole_word(x.split('\t')[0],x.split('\t')[1],0))
# df_all['query_in_description'] = df_all['product_info'].map(lambda x:str_whole_word(x.split('\t')[0],x.split('\t')[2],0))

# data_all$query_in_title <- str_whole_word(data$data_all_product_title_stem)

######## wrong
######## title got na value 

cat("Get number of words and word matching title in train\n")
data_all$title_match <- mapply(common_word,
                               data_all$search_term,
                               data_all$product_title)
data_all$des_match <- mapply(common_word, 
                             data_all$search_term, 
                             data_all$product_description)
data_all$brand_match <- mapply(common_word, 
                               data_all$search_term, 
                               data_all$brand)
data_all$material_match <- mapply(common_word, 
                                  data_all$search_term, 
                                  data_all$material)
data_all$shape_match <- mapply(common_word, 
                               data_all$search_term, 
                               data_all$shape)

# sapply(gregexpr("\\W+", str1), length) + 1
# sapply(gregexpr("\\W+", str1), function(x) sum(x>0) ) + 1 

data_all$len_search <- sapply(gregexpr("\\W+",data_all$search_term ), 
                              function(x) sum(x>0) ) + 1 
data_all$len_title <- sapply(gregexpr("\\W+",data_all$product_title ), 
                             function(x) sum(x>0) ) + 1 
data_all$len_des <- sapply(gregexpr("\\W+",data_all$product_description ), 
                           function(x) sum(x>0) ) + 1 
data_all$len_brand <- sapply(gregexpr("\\W+",data_all$brand ), 
                             function(x) sum(x>0) ) + 1 


data_save3 <- data_all
data_all <- data_save3

cat("Get number of words and word matching title in train with porter stem\n")



data_all_title_match_stem <- mapply(common_word2,
                                    data_all_search_term_stem,
                                    data_all_product_title_stem)
data_all_des_match_stem <- mapply(common_word2, 
                                  data_all_search_term_stem, 
                                  data_all_product_des_stem)
data_all$title_match_stem <- data_all_title_match_stem
data_all$des_match_stem <- data_all_des_match_stem

data_all$ratio_title <- data_all$title_match/data_all$len_search
data_all$ratio_des <- data_all$des_match/data_all$len_search
data_all$ratio_brand <- data_all$brand_match/data_all$len_search
data_all$ratio_title_stem <- data_all$title_match_stem/data_all$len_search
data_all$ratio_des_stem <- data_all$des_match_stem/data_all$len_search
data_all$ratio_material <- data_all$material_match/data_all$len_search
data_all$ratio_shape <- data_all$shape_match/data_all$len_search

data_save4 <- data_all
data_all <- data_save4
# 
# data_all$query_in_title <- 
# 
# 
# df_all['query_in_title'] = df_all['product_info'].map(lambda x:str_whole_word(x.split('\t')[0],x.split('\t')[1],0))
# df_all['query_in_description'] = df_all['product_info'].map(lambda x:str_whole_word(x.split('\t')[0],x.split('\t')[2],0))
# 
# 

# data_all$len_des < NULL
# data_all$len_title <- NULL
# data_all$len_brand <- NULL


train <- data_all %>%
  filter(type == "train")

test <- data_all %>%
  filter(type == "test")



cat("Feature Engineering Done\n")
str(train)
str(test)


train[is.na(train)] <- 0
train <- train[complete.cases(train),]

test[is.na(test)] <- 0

train_save <- train
test_save <- test

test <- test_save

# write.csv(train[,c(5,11:28)],"train.csv", row.names = FALSE)
# write.csv(test[,c(5,11:28)],"test.csv", row.names = FALSE)

train <- train[,c(5,11:30)]
test <- test[,c(2,11:30)]

# train$relevance_t <-log(log(train$relevance+1)+1)

cat("A linear model on number of words and number of words that match\n")
#######
# linear generate non-normal residuals, need to be revised 
test_rel <- matrix(0,nrow =dim(test)[1], ncol=22)
for (i in 1:22){
  h<-sample(nrow(train),10000)
  dval<-xgb.DMatrix(data=data.matrix(train[h,c(2:21)]),
                    label=train[h,1], missing = "NAN")
  dtrain<-xgb.DMatrix(data=data.matrix(train[-h,c(2:21)]),
                      label=train[-h,1], missing = "NAN")
  watchlist<-list(val=dval,train=dtrain)
  
  param <- list(  
    objective           = "reg:linear",
    booster             = "gbtree",
    eta                 = 0.05,
    max_depth           = 6,
    subsample           = 0.7,
    colsample_bytree    = 0.7,
    eval_metric         = "rmse",
    min_child_weight    = 6
    
  )
  
  clf <- xgb.train(data = dtrain,
                   params               = param,
                   nrounds              = 1000, #300
                   verbose              = 1,
                   watchlist            = watchlist,
                   early.stop.round     = 200,
                   print.every.n        = 50
  )
  clf$bestScore

  test_relevance <- predict(clf,data.matrix(test[,c(2:21)]),
                            ntreelimit =clf$bestInd
                            , missing="NAN"
  )
  # test_predict <- exp(exp(test_relevance_t)-1)-1
  summary(test_relevance)
  
  test_rel[,i] <- test_relevance

}

test_relevance <- rowMeans(test_rel)
test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)


h<-sample(nrow(train),5000)
dval<-xgb.DMatrix(data=data.matrix(train[h,c(2:21)]),
                  label=train[h,1], missing = "NAN")
dtrain<-xgb.DMatrix(data=data.matrix(train[-h,c(2:21)]),
                    label=train[-h,1], missing = "NAN")
watchlist<-list(val=dval,train=dtrain)

param <- list(  
                objective           = "reg:linear",
                booster             = "gbtree",
                eta                 = 0.05,
                max_depth           = 6,
                subsample           = 0.7,
                colsample_bytree    = 0.5,
                eval_metric         = "rmse",
                min_child_weight    = 6
                
)

clf <- xgb.train(data = dtrain,
                 params               = param,
                 nrounds              = 1000, #300
                 verbose              = 1,
                 watchlist            = watchlist,
                 early.stop.round     = 200,
                 print.every.n        = 50
)
clf$bestScore

# library(e1071)
# svm.model <- svm(relevance ~ title_match + des_match + brand_match
#                  + material_match + shape_match + len_search +
#                    len_title + len_des + len_brand + title_match_stem
#                  + des_match_stem + ratio_title + ratio_des + 
#                    ratio_brand + ratio_title_stem + ratio_des_stem 
#                  + ratio_material + ratio_shape, 
#                  data = train, type = "eps-regression", 
#                  kernel = )
# train_svm_pre <- predict(svm.model, train)
# library(Metrics)
# rmse(train$relevance, train_svm_pre)

# test$relevance <- predict(svm.model, test)


library(gbm)
# gbm_model <- gbm.fit(train[-h,10:25],train$relevance[-h],
#                      distribution = "gaussian",
#                      interaction.depth = 2,
#                      shrinkage=0.05,n.trees=500)
# train_predict <- predict(gbm_model,train[h,10:25],n.trees=600)
# train_predict <- predict(clf,data.matrix(train[,c(2:19)]),
#                          ntreelimit =clf$bestInd, missing = "NAN" )

# train_predict_t <- predict(clf,data.matrix(train[,c(11:28)]),
#                          ntreelimit =clf$bestInd, missing = "NAN" )
# train_predict <- exp(exp(train_predict_t)-1)-1

# library(Metrics)
# rmse(train$relevance, train_predict)
# 
# test$relevance <- predict(svm.model, test)
# test_relevance <- test$relevance


xgb.importance(colnames(train[c(2:21)]), model = clf)

# plot(train$relevance, train_predict)
# plot(sample(train$relevance-train_predict,1000))
# qqnorm(train$relevance_t-train_predict_t)
# 
# qqnorm(train$relevance-train_predict)
# hist(train$relevance-train_predict)




cat("Submit file\n")
test_relevance <- predict(clf,data.matrix(test[,c(2:21)]),
                          ntreelimit =clf$bestInd
                          , missing="NAN"
                          )
# test_predict <- exp(exp(test_relevance_t)-1)-1
summary(test_relevance)

test_relevance <- ifelse(test_relevance>3,3,test_relevance)
test_relevance <- ifelse(test_relevance<1,1,test_relevance)

submission <- data.frame(id=test$id,relevance=test_relevance)
subm <- submission[order(submission$id),]
# subm <- subm[!duplicated(subm),]
# subm <- unique(subm[,1:2])
write.csv(subm,"xgb_sub_1.csv", row.names = FALSE)

# print(Sys.time()-t)


