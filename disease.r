library(dplyr)
library(tidyr)
library(car)
library(pROC)
library(randomForest)
library(ggplot2)

#IMPORTING DATA INTO R
disease = read.csv("Parkinsson disease.csv")

glimpse(disease)

#removing the column containing names since it is not important for prediction
disease = disease %>% select(-name)

#checking for NAs
sum(is.na(disease))


vocal_fundamental= ggplot(disease, aes(x=MDVP.Fo.Hz., y =MDVP.Fhi.Hz., color = factor(status))) +
  geom_point();vocal_fundamental

spread = ggplot(disease, aes(x= spread1, y = spread2, color = factor(status)))+
  geom_point()+geom_smooth(); spread


dfa1 = ggplot(disease, aes(x = DFA), color = factor(status))+geom_density(); dfa1

#SPITTING THE DATA SET INTO TRAIN AND TEST SET
set.seed(123)
s = sample(1:nrow(disease), 0.8 * nrow(disease))
train = disease[s,]
test = disease[-s,]



#Linear regression-------------------------------------------------------------

linear1 = lm(status ~., data = train)
options(scipen = 999)
#remove columns with high vif value
tail(sort(vif(linear)))

#remove columns with high p-value ie greater than 0.05
sort((summary(linear)$coefficient)[,4], decreasing = T)[1:5]

#after removing columns final model
linear = lm(status ~.-Shimmer.DDA-Jitter.DDP - MDVP.Shimmer-
              MDVP.Jitter...-MDVP.Shimmer.dB.-MDVP.RAP-
              Shimmer.APQ5-MDVP.PPQ-PPE-HNR-DFA-NHR-
              spread2-MDVP.APQ-RPDE- MDVP.Fhi.Hz.-
              MDVP.Flo.Hz.-Shimmer.APQ3-MDVP.Jitter.Abs., data = train)

#predicting Status values on test data
val.pred = predict(linear, newdata = test, type = "response")
val.value = ifelse(val.pred > 0.5, 1, 0)
rmse.val = val.pred ** 2 %>% mean() %>% sqrt();rmse.val

#predicting Status values on train data
train.pred = predict(linear, newdata = train, type = "response")
train.value = ifelse(train.pred> 0.5, 1 , 0)
rmse.train = train.value **2 %>% mean() %>% sqrt(); rmse.train

#Area under curve
auc(roc(test$status, val.pred))




#Liner regression with step() function-------------------------------------

linear1 = lm(status ~.-Shimmer.DDA-Jitter.DDP  , data = train)
summary(linear1)
tail(sort(vif(linear1)))
#building model using step()
steplinear = step(linear1)
summary(steplinear)

#get step model formula
formula(steplinear)
sort((summary(steplinear)$coefficient)[,4], decreasing = T)[1:5]

steplinear = lm(status ~ MDVP.Fo.Hz. + MDVP.Flo.Hz. + MDVP.Jitter... + 
  MDVP.RAP + MDVP.Shimmer + RPDE + spread1 + spread2, data = train)

#predicting Status values on test data
val.pred = predict(steplinear, newdata = test, type = "response")
val.value = ifelse(val.pred > 0.5, 1, 0)
rmse.val = val.pred ** 2 %>% mean() %>% sqrt();rmse.val

#predicting Status values on train data
train.pred = predict(steplinear, newdata = train, type = "response")
train.value = ifelse(train.pred> 0.5, 1 , 0)
rmse.train = train.value **2 %>% mean() %>% sqrt(); rmse.train

#Area under curve
auc(roc(test$status, val.pred))


#RandomForest--------------------------------------
Randommodel = randomForest(status~., data = train);Randommodel


#predicting Status values on test data
val.pred = predict(Randommodel, newdata = test, type = "response")
val.value = ifelse(val.pred > 0.5, 1, 0)
rmse.val = val.pred ** 2 %>% mean() %>% sqrt();rmse.val

#predicting Status values on train data
train.pred = predict(Randommodel, newdata = train, type = "response")
train.value = ifelse(train.pred> 0.5, 1 , 0)
rmse.train = train.value **2 %>% mean() %>% sqrt(); rmse.train

#Area under curve
auc(roc(test$status, val.pred))
