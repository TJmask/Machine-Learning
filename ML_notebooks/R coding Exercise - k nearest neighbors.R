rm(list=ls());
gc();
library(class)
library(gmodels)
setwd('C:/Users/xvidalberastain/Google Drive/xavi_teaching/bus_111a_business_analytics_fall_2019/')
# Example: Classifying Cancer Samples using KNN                   - ------------------
wbcd         = read.csv('/Users/tjmask/Desktop/Courses/code of R/ML/data_wisc_bc.csv', stringsAsFactors = FALSE)
head(wbcd)
length(wbcd)
# dropping useless variables: id
wbcd = wbcd[-1]
nrow(wbcd)
dim(wbcd)
table(wbcd$diagnosis)


# Renaming diagnosis as a factor with proper labels
wbcd$diagnosis = factor(x      = wbcd$diagnosis,
                        levels = c("B",      "M"),
                        labels = c("Benign", "Malignant"))

# Initial exploratory analysis
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)


# Prep the data:(i) normalizing and (ii)test vs non-test data sep - --------------

# Normalizing function and normalizing the wbcd data                                                        
normalize = function(x){return ((x - min(x)) / (max(x) - min(x)))}
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
wbcd_n    = as.data.frame(lapply(wbcd[,2:31], normalize))
summary(wbcd_n$area_mean)
head(wbcd_n)


# create training and test data
wbcd_test_n  = wbcd_n[470:569, ]
wbcd_train_n = wbcd_n[1:400  , ]
wbcd_valid_n = wbcd_n[401:569, ]
wbcd_test    = wbcd[  470:569, ]
wbcd_train   = wbcd[  1:400  , ]
wbcd_valid   = wbcd[401:569  , ]


# create labels for training and test data
wbcd_train_labels = wbcd_train[, 1]
wbcd_valid_labels = wbcd_valid[, 1]
wbcd_test_labels  = wbcd_test[ , 1]


# Training model on dta_training                                  - --------------
wbcd_valid_pred = class::knn(train = wbcd_train_n, 
                             cl    = wbcd_train_labels,
                             test  = wbcd_valid_n,
                             k     = 1)

nrow(wbcd_train_n)
length(wbcd_train_labels)
nrow(wbcd_valid_n)
# Evaluating performance on dta_test                              - --------------
k1_conf_mat  =   gmodels::CrossTable(x          = wbcd_valid_labels, 
                                     y          = wbcd_valid_pred,
                                     prop.chisq = TRUE)

# re-classify test cases
wbcd_valid_pred = class::knn(train = wbcd_train_n, 
                             cl    = wbcd_train_labels,
                             test  = wbcd_valid_n,
                             k     = 5)

k5_conf_mat     = gmodels::CrossTable(x          = wbcd_valid_labels, 
                                      y          = wbcd_valid_pred,
                                      prop.chisq = TRUE)

k5_conf_mat$t
k5_conf_mat$prop.row
k5_conf_mat$prop.col

