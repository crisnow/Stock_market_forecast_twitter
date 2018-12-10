library(lmtest)

#Chickens, Eggs, and Causality, or Which Came First???? 
# It consists of two time series from 1930 to 1983, 
# one of U.S. egg production and 
# the other the estimated U.S. chicken population.

## Which came first: the chicken or the egg?
data(ChickEgg)
## chickens granger-cause eggs?
grangertest(egg ~ chicken, order = 3, data = ChickEgg)
## eggs granger-cause chickens?
grangertest(chicken ~ egg, order = 3, data = ChickEgg)

## To perform the same tests `by hand', you can use dynlm() and waldtest():

install.packages("dynlm")
require(dynlm)) 
  ## chickens granger-cause eggs?
  em <- dynlm(egg ~ L(egg, 1) + L(egg, 2) + L(egg, 3), data = ChickEgg)
  em2 <- update(em, . ~ . + L(chicken, 1) + L(chicken, 2) + L(chicken, 3))
  waldtest(em, em2)
  
  ## eggs granger-cause chickens?
  cm <- dynlm(chicken ~ L(chicken, 1) + L(chicken, 2) + L(chicken, 3), data = ChickEgg)
  cm2 <- update(cm, . ~ . + L(egg, 1) + L(egg, 2) + L(egg, 3))
  waldtest(cm, cm2)



#----  stationary ---
library(zoo)
applPrice<-read.csv(file = '/Users/gongxuecrise/Desktop/ICS 668 social informatics/research proposal/twitter_account/apple_price.csv')
applPR<-as.numeric(applPrice[,1])

logRet <- log(applPR[-70] / (applPR[-1]))*100



plot(logRet, type = 'l')
return <- applPrice


# Correlation matrix from mtcars
# with mpg, cyl, and disp as rows 
# and hp, drat, and wt as columns 
x <- mtcars[1:3]
y <- mtcars[4:6]
cor(x, y)
cor(x, y, method = "spearman")
cor(x, y, method = "kendall")

library(glmnet)
?cv.glmnet
