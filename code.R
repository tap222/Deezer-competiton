rm(list = ls())
gc()
setwd('/home/tapas/Documents/deezer')
dir()

suppressMessages(library(lubridate))
suppressMessages(library(caret))
suppressMessages(library(gbm))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(h2o))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(corrplot))
library(anytime)

tr<-data.table(read.csv("train.csv"))
te<-data.table(read.csv("test.csv"))
tr<-data.table(tr[1:1000000,])
te<-data.table(te[1:199918,])
str(tr)
str(te)

#train
tr$release_date <- ymd(tr$release_date)
tr$year<-substring(tr$release_date,1,4)
#tr$year<- year(tr$release_date)
#tr$ts_listen_date<-anydate(tr$ts_listen)
View(tr)
#tr$ts_listen<- anytime(tr$ts_listen)
#tr$ts_listen_time<-format(as.POSIXlt(tr$ts_listen),format = '%T')
tr$user_age<-as.factor(tr$user_age)
#r$year<-as.factor(tr$year)

#test
te$release_date<-ymd(te$release_date)
#te$year<- year(te$release_date)
te$ts_listen_date<-anydate(te$ts_listen)
View(te)
#te$ts_listen<- anytime(te$ts_listen)
#te$ts_listen_time<-format(as.POSIXlt(te$ts_listen),format = '%T')
te$user_age<-as.factor(te$user_age)
#te$year<-as.factor(te$year)

#te[,ts_listen_time:=NULL]
te$is_listen<-""

tr$context_type<- as.factor(tr$context_type)
tr$platform_name<-as.factor(tr$platform_name)
tr$user_gender<- as.factor(tr$user_gender)
tr$platform_name <- as.factor(tr$platform_name)
tr$platform_family<- as.factor(tr$platform_family)
tr$listen_type<-as.factor(tr$listen_type)
tr$artist_id<-as.factor(tr$artist_id)
tr$is_listened<-as.factor(tr$is_listened)
tr$genre_id<-as.factor(tr$genre_id)
tr$media_id<-as.factor(tr$media_id)


te$context_type<- as.factor(te$context_type)
te$platform_name<-as.factor(te$platform_name)
te$user_gender<- as.factor(te$user_gender)
te$platform_name <- as.factor(te$platform_name)
te$platform_family<- as.factor(te$platform_family)
te$listen_type<-as.factor(te$listen_type)
te$artist_id<-as.factor(te$artist_id)
te$genre_id<-as.factor(te$genre_id)
te$album_id<-as.factor(te$album_id)


str(tr)
str(te)

se <- colnames(tr)[sapply(tr, is.numeric)]
#se <- se[!(se %in% c("context_type","platform_family","listen_type","artist_id","genre_id","album_id","year","month","day"))))]

#train
ggplot(tr,aes(media_duration))+geom_density(fill='lightblue',color='black')
skew <- sapply(tr[,se,with=F], function(x) skewness(x,na.rm = T))
skew
skew <- skew[skew > 2] #filter variables with skewness > 2
tr[,(names(skew)) := lapply(.SD, function(x) log(x + 15)), .SDcols = names(skew)]
ggplot(tr,aes(media_duration))+geom_density(fill='lightblue',color='black')

#test
ggplot(te,aes(media_duration))+geom_density(fill='lightblue',color='black')
se <- colnames(te)[sapply(te, is.numeric)]
skew_t <- sapply(te[,se,with=F], function(x) skewness(x,na.rm = T))
skew_t
skew_t <- skew_t[skew_t > 2]
te[,(names(skew_t)) := lapply(.SD, function(x) log(x + 10)), .SDcols = names(skew_t)]
ggplot(te,aes(media_duration))+geom_density(fill='lightblue',color='black')


# One Hot Encoding --------------------------------------------------------

tr_mod <- tr[,.(platform_name,user_gender,context_type,platform_family,listen_type,user_age)]
te_mod <- te[,.(platform_name,user_gender,context_type,platform_family,listen_type,user_age)]

tr_ex <- model.matrix(~.+0, data = tr_mod)
te_ex <- model.matrix(~.+0, data = te_mod)


tr_ex <- as.data.table(tr_ex)
te_ex <- as.data.table(te_ex)

dg <- setdiff(colnames(tr_ex), colnames(te_ex))
  
tr_ex <- tr_ex[,-dg,with=F]

new_tr <- cbind(tr, tr_ex)
new_te <- cbind(te, te_ex)


new_tr[,c("platform_name","user_gender","context_type","platform_family","listen_type","artist_id","genre_id","user_age") := NULL]
new_te[,c("platform_name","user_gender","context_type","platform_family","listen_type","artist_id","genre_id","user_age") := NULL]

co_data <- rbind(new_tr,new_te,fill=TRUE)

F_tr <- co_data[!(is.na(is_listened))]
F_te <- co_data[!is.na(is_listened)]


rm(co_data,new_tr,new_te,tr_ex,te_ex)
gc()


# Feature Engineering -----------------------------------------------------

#I think upto some extent


F_tr[,new_var_2 := as.Date(as.character(tr$ts_listen_date), format="%Y-%m-%d")-
       as.Date(as.character(tr$release_date), format="%Y-%m-%d")]
F_tr$new_var_2<-as.integer(F_tr$new_var_2)

F_te[,new_var_2 := as.Date(as.character(te$release_date), format="%Y/%m/%d")-
       as.Date(as.character(te$ts_listen_date), format="%Y/%m/%d")]
F_te$new_var_2<-as.integer(F_te$new_var_2)

tr[,new_var_3 := mean(media_duration),genre_id]
F_tr[,new_var_3 := tr$new_var_3]

xkm <- tr[,mean(media_duration),genre_id]

te <- xkm[te, on="genre_id"]
F_te[,new_var_3 := te$V1]

tr[,new_var_4 := mean(media_duration),user_gender]
F_tr[,new_var_4 := tr$new_var_4]

akm <- tr[,mean(media_duration),user_gender]

te <- akm[te, on="user_gender"]
F_te[,new_var_4 := te$i.V1]

tr[,new_var_5 := mean(media_duration),context_type]
F_tr[,new_var_5 := tr$new_var_5]

bkm <- tr[,mean(media_duration),context_type]

te <- bkm[te, on="context_type"]
F_te[,new_var_5 := te$i.V1.1]

tr[,new_var_6 := mean(media_duration),platform_family]
F_tr[,new_var_6 := tr$new_var_6]

ckm <- tr[,mean(media_duration),platform_family]

te <- ckm[te, on="platform_family"]
F_te[,new_var_6 := te$i.V1.2]

tr[,new_var_7 := mean(media_duration),platform_name]
F_tr[,new_var_7 := tr$new_var_7]

dkm <- tr[,mean(media_duration),platform_name]

te <- dkm[te, on="platform_name"]
F_te[,new_var_7 := te$i.V1.3]

tr[,new_var_8 := mean(media_duration),listen_type]
F_tr[,new_var_8 := tr$new_var_8]

ekm <- tr[,mean(media_duration),listen_type]

te <- ekm[te, on="listen_type"]
F_te[,new_var_8 := te$i.V1.4]

tr[,new_var_9 := mean(media_duration),artist_id]
F_tr[,new_var_9 := tr$new_var_9]

fkm <- tr[,mean(media_duration),artist_id]

te <- fkm[te, on="artist_id"]
F_te[,new_var_9 := te$i.V1.5]


#tr[,new_var_10 := mean(media_duration),year]
#F_tr[,new_var_10 := tr$new_var_10]

#gkm <- tr[,mean(media_duration),year]

#te <- gkm[te, on="year"]
#F_te[,new_var_10 := te$i.V1.6]

tr[,new_var_11 := mean(media_duration),user_age]
F_tr[,new_var_11 := tr$new_var_11]

hkm <- tr[,mean(media_duration),user_age]

te <- hkm[te, on="user_age"]
F_te[,new_var_11 := te$i.V1.7]

# Machine Learning with H2o -----------------------------------------------

#Give H2o your maximum memory for computation
#if your laptop is 8GB, give atleast 6GB, close all other apps while computation happens
#may be, go out take a walk! 

h2o.init(nthreads = -1,max_mem_size = "10G") 

h2o_tr <- as.h2o(F_tr)
h2o_te <- as.h2o(F_te)

h2o_tr$is_listened <- h2o.asfactor(h2o_tr$is_listened)


# Create a validation frame -----------------------------------------------

#Here I want to avoid doing k-fold CV since data set is large, it would take longer time
#hence doing hold out validation

xd <- h2o.splitFrame(h2o_tr,ratios = 0.8)

split_val <- xd[[2]]

y <- "is_listened"
x <- setdiff(colnames(F_tr), c(y,"user_id"))



# Training a GBM Model ----------------------------------------------------

gbm_clf <- h2o.gbm(x = x
                   ,y = y
                   ,training_frame = h2o_tr
                   ,validation_frame = split_val
                   ,ignore_const_cols = T
                   ,ntrees = 1000
                   ,max_depth = 20
                   ,stopping_rounds = 10
                   ,model_id = "gbm"
                   ,stopping_metric = "AUC"
                   ,learn_rate = 0.05
                   ,col_sample_rate_per_tree = 0.8
                   ,sample_rate = 0.8
                   ,#learn_rate_annealing = 0.99
                   
                   
)

gbm_clf 

gbm_clf_pred <- as.data.table(h2o.predict(gbm_clf,h2o_te))
head(gbm_clf_pred,10)

sub_pred1 <- data.table(sample_id = te$sample_id, is_listened = gbm_clf_pred$p1)
fwrite(sub_pred1,"h2o_gbm_u1.csv") 






ind<-createDataPartition(F_tr$is_listened, p=0.8, list=FALSE)
train<-F_tr[ind,]
valid<-F_tr[-ind,]

## 37. Convert back zero and one to 0 and 1
#      xgboost wants numeric classes
train$is_listened1<-as.numeric(train$is_listened1)-1
valid$is_listened1<-as.numeric(valid$is_listened1)-1
head(train,10)
head(valid,10)

## 38. Convert dataframe to dense matrix
tmatrix<-data.matrix(train[,1:41])
vmatrix<-data.matrix(valid[,1:41])
class(tmatrix)
head(tmatrix)

## 39. Convert dense matrix to xgb.DMatrix
dtrain<-xgb.DMatrix(tmatrix,label=train$is_listened1)
dvalid<-xgb.DMatrix(vmatrix, label=valid$is_listened1)

## 40. Create a watchlist
watchlist <- list(train=dtrain, test=dvalid)

# 41 Create model
model <- xgb.train(data=dtrain,                    # Must
                   max.depth=3,                    # Optional, default=6
                   eta=0.3,                        # Optional, default=0.3
                   nthread = 2,                    # Optional, default=1
                   nround=300,                      # Must
                   #subsamle= 0.75,                  # Optional, default=1
                   #colsample_bytree = 0.8,         # Optional, default=1
                   watchlist=watchlist,            # Optional, default=none
                   objective = "binary:logistic")  # Must


# 42 Make predictions on validation data
pred <- predict(model, dvalid)

# 43 COnvert probabilities to class values
prediction <- as.numeric(pred > 0.5)
print(head(prediction))

# 44 And judge accuracy
confusionMatrix(prediction,valid$Choice, positive = "1")

# 45 ROC graph
df_roc<-roc(valid$is_listened1,pred)
plot(df_roc,main="AUC = " %s+% df_roc$auc)

# 46 Make predictions on test data
#    and write to a file

# 46.1 Convert te data.frame to matrix form
test_matrix<-data.matrix(te)
# 46.2 MAke predictions
test_pred <- predict(model, test_matrix)
test_pred
# 46.3 Convert to numerics
prediction_test <- as.numeric(test_pred > 0.5)
print(head(prediction_test,20))

# 47 Read sample submission file and submit
#   probabilities
ss<-read.csv("sample_predictions.csv")
head(ss)
ss$Choice<-test_pred
head(ss)
# 48
write.csv(ss, file="xgboost.csv", quote= FALSE, row.names = FALSE)