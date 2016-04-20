library(dplyr)
library(ggplot2)
library(e1071)
library(SparseM)
library(tm)

### Part 1 - Read a CSV file into memory

data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"),
                 header = FALSE, 
                 col.names = c("age", "workclass", 
                               "fnlwgt", "education", "education-num", 
                               "marital-status","occupation", 
                               "relationship", "race", "sex", 
                               "capital-gain", "capital-loss", 
                               "hours-per-week", "native-county", 
                               "income"),
                 skip = 10)
dim(data)
data[1:5,]

### Part 2 - Filtering
df1 <- data %>%
  filter(education != " Preschool")

df2 <- df1 %>%
  filter(education.num >=  quantile(df1$education.num, 0.25) 
         & education.num <=  quantile(df1$education.num, 0.75) )

percentage_descrease <- (dim(data)[1] - dim(df2)[1])/dim(data)[1]
sprintf("Percentage descrease in Filtering Step is: %s%%",
        100*percentage_descrease)

### Part 3 - Grouping and visualization
ggplot(data = df2, aes(x=occupation, y=age, fill = occupation)) +
  # geom_bar(stat="identity")
  stat_summary(fun.y="mean", geom="bar")

### Part 4 - Naiive Bayes Classification
traindata <- read.csv(url('https://s3-us-west-1.amazonaws.com/amirziai-accessfuel/nb_train.csv'))
testdata <- read.csv(url('https://s3-us-west-1.amazonaws.com/amirziai-accessfuel/nb_train.csv'))

trainvector <- as.vector(traindata$Sentence)
testvector <- as.vector(testdata$Sentence)

# CREATE SOURCE FOR VECTORS
trainsource <- VectorSource(trainvector)
testsource <- VectorSource(testvector)

# CREATE CORPUS FOR DATA
traincorpus <- Corpus(trainsource)
testcorpus <- Corpus(testsource)

# PERFORMING THE VARIOUS TRANSFORMATION on "traincorpus" and "testcorpus" DATASETS #SUCH AS TRIM WHITESPACE, REMOVE PUNCTUATION, REMOVE STOPWORDS.
traincorpus <- tm_map(traincorpus,stripWhitespace)
traincorpus <- tm_map(traincorpus,tolower)
traincorpus <- tm_map(traincorpus, removeWords,stopwords("english"))
traincorpus<- tm_map(traincorpus,removePunctuation)
traincorpus <- tm_map(traincorpus, PlainTextDocument)
testcorpus <- tm_map(testcorpus,stripWhitespace)
testcorpus <- tm_map(testcorpus,tolower)
testcorpus <- tm_map(testcorpus, removeWords,stopwords("english"))
testcorpus<- tm_map(testcorpus,removePunctuation)
testcorpus <- tm_map(testcorpus, PlainTextDocument)

# CREATE TERM DOCUMENT MATRIX
trainmatrix <- t(TermDocumentMatrix(traincorpus))
testmatrix <- t(TermDocumentMatrix(testcorpus))

# TRAIN NAIVE BAYES MODEL USING trainmatrix DATA AND traindate$Journal_group CLASS VECTOR
model <- naiveBayes(as.matrix(trainmatrix),as.factor(traindata$Tag))

# PREDICTION
results <- predict(model,as.matrix(testmatrix))

