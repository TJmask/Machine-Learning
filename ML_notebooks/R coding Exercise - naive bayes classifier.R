rm(list=ls());
gc();

# install.packages('e1071')
# install.packages('naivebayes')
# install.packages('caret')
library(caret) 
library(e1071)
library(naivebayes)
library(data.table)
set.seed(123456)

# Example: Classifying Student decission using NB                   # #######
mydata      = fread('https://raw.githubusercontent.com/rlowrance/re/master/hsbdemo.csv');
head(mydata)
mydata[, V1:= NULL]
mydata[, id:= NULL]
mydata[, cid:= NULL]
mydata$ses = ifelse(mydata$ses=="low", 0, ifelse(mydata$ses=="middle",1,2))
mydata$schtyp = 1*(mydata$schtyp=="private")
mydata$honors = 1*(mydata$honors=="enrolled")
mydata$female = 1*(mydata$female=="female")

summary(mydata)
# Prep the data:(i) normalizing and (ii) test vs non-test data sep  # --------
# standarize the data

inx_train    = caret::createDataPartition(mydata$prog, p=0.7)$Resample1 
inx_valid    = NULL
inx_test     = (1:nrow(mydata))[! (1:nrow(mydata) %in% inx_train)]

dta_train    = mydata[ inx_train, ]
dta_valid    = mydata[ inx_valid, ]
dta_test     = mydata[-inx_train, ]

# Training a model on the data                                      # #######
NBclassfied  = e1071::naiveBayes(prog~ses+science., data=dta_train)
predict(NBclassfied,newdata = dta_test,type="class")
names(NBclassfied)
NBclassfied$apriori
NBclassfied$tables
NBclassfied$call
head(mydata)

mydata = as.data.frame(mydata)


# Training a model on the data AGAIN                                # #######
NBclassifier=naivebayes::naive_bayes(formula      = prog~ses+science+socst+read,
                                        usekernel = T,
                                        data      = dta_train)
predict(NBclassifier,newdata = dta_train)


# Evaluating model performance using a confusion matrix             # #######
fitted_data          = data.table( cbind(test_data = dta_test[,prog],
                                         pred_data = paste(predict(NBclassifier,newdata = dta_test)))
                                   )
fitted_data$is_equal = fitted_data$test_data==fitted_data$pred_data
confuss_mat          = fitted_data[,
                                   {
                                     tmp1=sum(is_equal);
                                     tmp2=sum(!is_equal);
                                     list(corrects=tmp1,wrongs = tmp2)
                                     },keyby=.(test_data,pred_data)]
confusionMatrix(as.factor(fitted_data$pred_data), as.factor(dta_test$prog))
sum(confuss_mat[,3:4])
dim(fitted_data)








# 1. txt
## nerual network NLP predicting.
## classifier. 

# 2. A, B progress. 












