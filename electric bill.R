# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('electric.csv')


# Taking care of missing data

dataset$amount = ifelse(is.na(dataset$amount),
                        ave(dataset$amount, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$amount)


# Encoding categorical data
dataset$month = factor(dataset$month,
                       levels = c('jan', 'feb', 'mar' ,'apr','may','jun','jul','agu','sep','oct','nov','dec'),
                       labels = c(1, 2, 3, 4,5,6,7,8,9,10,11,12))

# Feature Scaling
dataset$e3 = as.vector(scale(dataset$e3))
pre1=8968

# Fitting Multiple Linear Regression to the Training set
#finding present consuption
regressor = lm(formula = pres ~ pre,
               data = dataset)

library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$pre, y = dataset$pres),
             colour = 'red') +
  geom_line(aes(x = dataset$pre, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('power consp') +
  xlab('previous') +
  ylab('present')
y_pred = predict(regressor,data.frame(pre=pre1))
y_pred

#finding fixed rate e3,consp
consp1=y_pred-pre1
library(randomForest)
set.seed(1234)
regressor = randomForest(x = dataset[3:4],
                         y =  dataset$consp,
                         ntree = 10)
y_pred4 = predict(regressor,data.frame(pres=y_pred,pre=pre1))
y_pred4
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$pres+dataset$pre, y =dataset$consp),
             colour = 'red')+
  geom_line(aes(x =dataset$pres+dataset$pre, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('power consp') +
  xlab('previous') +
  ylab('present')

library(randomForest)
set.seed(1234)
regressor = randomForest(x = dataset[3:5],
                         y =  dataset$e3,
                        ntree = 50)
y_pred2 = predict(regressor,data.frame(pres=y_pred,pre=pre1,consp=y_pred4))
y_pred2
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$pres+dataset$pre+dataset$consp, y =dataset$e3),
             colour = 'red')+
  geom_line(aes(x =dataset$pres+dataset$pre+dataset$consp , y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('power consp') +
  xlab('previous') +
  ylab('present')

# Predicting the Test set results


#Building the optimal model using backward elimination

#make x0 is 1 which we include in X matrix as refer<- copy we dont need this in R
#we are fitting the model which is 2nd step of backward
#if p>significance level we remove that col,lower p high accuracy


#z = predict(regressor, data.frame( pre= ))
#z

#IMP Working have to do better.
#getting amount
library(rpart)
regressor = rpart(formula = amount ~ pres+pre+e3+pricee3,
                  data = dataset,
                  control = rpart.control(minsplit = 8))
y_pred3 = predict(regressor, data.frame(pres=y_pred
,pre=pre1,e3=y_pred2,pricee3=6.5
))
y_pred3

library(ggplot2)
ggplot() +
  geom_point(aes(x =  dataset$pres + dataset$pricee3 + dataset$e3+dataset$pre, y = dataset$amount),
             colour = 'red') +
  geom_line(aes(x = dataset$pres+dataset$pre+dataset$e3, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')



#library(randomForest)
#set.seed(1234)
#regressor = randomForest(x = dataset[3:8],
#                         y =  dataset$amount,
 #                        ntree = 5000)
#y_pred = predict(regressor, data.frame(pres=8823,pre=8662,e3=61,pricee3=6.5,e1=30,e2=70
#library(ggplot2)
#ggplot() +
 # geom_point(aes(x =  dataset$pres + dataset$pricee3 + dataset$e3+dataset$pre, y = dataset$amount),
  #           colour = 'red') +
  #geom_line(aes(x = dataset$pres+dataset$pre+dataset$e3, y = predict(regressor, newdata = dataset)),
  #          colour = 'blue') +
  #ggtitle('Truth or Bluff (Decision Tree Regression)') +
  #xlab('Level') +
  #ylab('Salary')
