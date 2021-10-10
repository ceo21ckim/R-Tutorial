
# y : medv(주택가격)
# x : crim, zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat (13th variable)

str(Boston)

# caret ----
# createDataPartition ----

if (!require('glmnet')) install.packages('glmnet')
if (!require('caret')) install.packages('caret')

library(MASS);library(caret); library(glmnet) ; set.seed(42)

# times : the number of partitions to create
# p : the percentage of data that goes to training
train <- createDataPartition(y=Boston$medv, p = 0.7, list = FALSE)

Boston.train <- Boston[train,] ; Boston.test <- Boston[-train,]

x <- model.matrix(medv ~., Boston.train)[,-1] # 절편 부분을 제거.
y <- Boston.train$medv

# alpha = 0 : Ridge, alpha = 1 : Lasso
# family  = 'gaussian', 'poisson', 'binomial', 'multinomial'
# Ridge ----
Boston.cv <- cv.glmnet(x=x, y=y, family='gaussian', alpha = 0 )

plot(Boston.cv)

Boston.cv$lambda.min
# 0.6736516
log(Boston.cv$lambda.1se)
# 1.372599

log(Boston.cv$lambda.min)
# -0.3950422


Boston.gnet <- glmnet(x, y, family = 'gaussian', alpha = 0, lambda = Boston.cv$lambda.min)
coef(Boston.gnet)

Boston.test.x <- model.matrix(medv ~., Boston.test)[,-1]
Boston.pred <- predict(Boston.gnet, newx=Boston.test.x)
postResample(pred=Boston.pred, obs=Boston.test$medv)


# Lasso ----
set.seed(42)
Boston.cv <- cv.glmnet(x=x, y=y, family='gaussian', alpha=1)

Boston.cv$lambda.min
# 0.02536255

log(Boston.cv$lambda.min)
# -3.674482

plot(Boston.cv)

Boston.cv$lambda.1se
# 0.6581643 

log(Boston.cv$lambda.1se)
# -0.4183006 

coef(Boston.cv, Boston.cv$lambda.min)
coef(Boston.cv, Boston.cv$lambda.1se)



Boston.gnet1 <- glmnet(x, y, family = 'gaussian', 
                       alpha = 1, lambda = Boston.cv$lambda.min)

Boston.pred1 <- predict(Boston.gnet1, newx=Boston.test.x)
postResample(pred=Boston.pred1, obs=Boston.test$medv)

Boston.gnet2 <- glmnet(x, y, family = 'gaussian', 
                       alpha = 1, lambda = Boston.cv$lambda.1se)

Boston.pred2 <- predict(Boston.gnet2, newx=Boston.test.x)
postResample(pred=Boston.pred2, obs=Boston.test$medv)


# Elasticnet ----
set.seed(123)
# caret package -> train function
Boston.cv <- train(form = medv ~., data=Boston.train, method = 'glmnet', 
                   trControl=trainControl(method='cv', number=10),
                   tuneLength=10)
Boston.cv$bestTune

plot(Boston.cv)
# alpha = 0.1, lambda = 0.08864354

Boston.gnet <- glmnet(x, y, family='gaussian', 
                      alpha = Boston.cv$bestTune$alpha, 
                      lambda=Boston.cv$bestTune$lambda)

coef(Boston.gnet)

Boston.pred <- predict(Boston.gnet, newx=Boston.test.x)
postResample(pred=Boston.pred, obs=Boston.test$medv)


# 비교 ----

set.seed(123)
lambda <- 10^seq(-5,5,length=100)
# expand.grid = 탐색해야할 인수값을 그리드 형태로 반환해 효율적으로 계산.
# ridge
ridge <- train(form = medv ~., data= Boston.train, method='glmnet',
               trControl=trainControl(method='cv', number=10), 
               tuneGrid=expand.grid(alpha=0, lambda=lambda))

coef(ridge$finalModel, ridge$bestTune$lambda)
ridge.pred <- predict(ridge, Boston.test)
postResample(pred=ridge.pred, obs=Boston.test$medv)

# lasso 
set.seed(123)
lasso <- train(medv ~., data= Boston.train, method='glmnet',
               trControl=trainControl(method='cv', number=10), 
               tuneGrid=expand.grid(alpha=1, lambda=lambda))
coef(lasso$finalModel, lasso$bestTune$lambda)
lasso.pred <- predict(lasso, Boston.test)
postResample(pred=lasso.pred, obs=Boston.test$medv)

# Elasticnet
set.seed(123)
elastic <- train(medv ~., data=Boston.train, method='glmnet',
                 trControl=trainControl(method='cv', number=10),
                 tuneLength=10)

coef(elastic$finalModel, elastic$bestTrun$lambda)

elastic.pred <- predict(elastic, Boston.test)
postResample(pred=elastic.pred, obs=Boston.test$medv)


models <- list(ridge=ridge, lasso=lasso, elastic=elastic)
summary(resamples(models), metric='RMSE')
summary(diff(resamples(models), metric='RMSE'))

