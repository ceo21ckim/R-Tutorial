# logistic regression ---- 
# C50 -> churn
if (!require('C50')) install.packages('C50')
if (!require('modeldata')) install.packages('modeldata')
if (!require('caret')) install.packages(caret)
library(C50) ; library(modeldata) ; library(caret)

data("mlc_churn")
str(mlc_churn)

train <- createDataPartition(y= mlc_churn$churn, p= 0.666, list = FALSE)

churnTrain <- mlc_churn[train,] ; churnTest <- mlc_churn[-train,]
churn.train <- churnTrain[-c(1, 3)]
churn.train$churn <- factor(ifelse(churn.train$churn == 'no', 1, 2),
                            levels = c(1, 2), labels = c('no', 'yes'))

str(churn.train)
levels(churn.train$churn)

table(churn.train$churn)

prop.table(table(churn.train$churn))

churn.logit <- glm(churn ~., data = churn.train, family = binomial(link = 'logit'))
summary(churn.logit)

exp(coef(churn.logit))



churn.test <- churnTest[-c(1, 3)]
churn.test$churn <- factor(ifelse(churn.test$churn=='no', 1, 2),
                           levels = c(1,2), labels = c('no', 'yes'))
churn.test$international_plan <- ifelse(churn.test$international_plan=='no', 0, 1)
churn.test$voice_mail_plan <- ifelse(churn.test$voice_mail_plan == 'no', 0, 1)

z <- coef(churn.logit)[1] + (as.matrix(churn.test[-18]) %*% coef(churn.logit)[-1])

p <- 1/(1+exp(-z))

head(p)


# predict ----
# type : response 를 하면 로그오즈 대신 좀 더 해석이 용이한 사건발생확률을 출력한다. 
churn.test <- churnTest[-c(1, 3)]
churn.test$churn <- factor(ifelse(churn.test$churn=='no', 1, 2),
                           levels = c(1,2), labels = c('no', 'yes'))


churn.logit.pred <- predict(churn.logit, newdata=churn.test, type = 'response')

head(churn.logit.pred)

churn.logit.pred <- factor(churn.logit.pred > 0.5, 
                           levels=c(FALSE, TRUE), labels = c('no', 'yes'))

table(churn.logit.pred)

# confusion matrix ----
table(churn.test$churn, churn.logit.pred, dnn=c('Actual', 'Predicted'))
mean(churn.test$churn == churn.logit.pred)



# parameter selection -----
# stepwise
churn.logit2 <- step(churn.logit)

summary(churn.logit2)

# 고객의 서비스센터 전화횟수가 고객이탈확률에 미치는 영향에 관심있다고 할 때..
churn.test <- churnTest[-c(1, 3)]
churn.test$churn <- factor(ifelse(churn.test$churn=='no', 1, 2),
                           levels = c(1, 2), labels = c('no', 'yes'))
churn.test$international_plan <- ifelse(churn.test$international_plan=='no', 0, 1)
churn.test$voice_mail_plan <- ifelse(churn.test$voice_mail_plan == 'no', 0, 1)

table(churn.test$number_customer_service_calls)

# 전화횟수가 미치는 영향만 파악하고 싶기 때문에 연속형변수는 mean을 명목형 변수는 가장 낮은 범주유형으로 지정
testdata <- data.frame(total_day_calls = mean(churn.test$total_day_calls),
                       number_vmail_messages = mean(churn.test$number_vmail_messages),
                       total_night_charge = mean(churn.test$total_night_charge),
                       voice_mail_plan = 0,
                       total_intl_calls = mean(churn.test$total_intl_calls),
                       total_intl_minutes = mean(churn.test$total_intl_minutes),
                       total_eve_charge = mean(churn.test$total_eve_charge),
                       total_day_minutes = mean(churn.test$total_day_minutes),
                       number_customer_service_calls = c(0:7),
                       international_planyes = mean(churn.test$international_plan))


testdata

z <- coef(churn.logit2)[1] + (as.matrix(testdata) %*% coef(churn.logit2)[-1])

p <- 1 / (1+exp(-z))
testdata$prob <- p

testdata[c('number_customer_service_calls', 'prob')]

# 로지스틱 회귀분석을 수행할 때에는 과산포(overdispersion)의 문제가 발생할 수 있다. 이항분포로부터 추출된 데이터의 
# 기대분산은 np(1-p)이며, 과산포문제는 y의 실제 관측된 분산이 이항분포로부터 기대되는 분산보다 클 때 발생한다.
# 과산포는 표준오차를 왜곡시켜 회귀계수의 유의성 검정을 부정확하게 만들 위험이 있다.

# family = binomial 대신에 quasibinomial을 지정한다. 

# overdispersion ----
deviance(churn.logit2) / df.residual(churn.logit2)

fit.origin <- glm(formula = churn ~ international_plan + voice_mail_plan + 
                    number_vmail_messages + total_day_charge + total_eve_minutes + 
                    total_night_charge + total_intl_calls + total_intl_charge + 
                    number_customer_service_calls, family = binomial(),
                  data = churn.train)

fit.overdis <- glm(formula = churn ~ international_plan + voice_mail_plan + 
                    number_vmail_messages + total_day_charge + total_eve_minutes + 
                    total_night_charge + total_intl_calls + total_intl_charge + 
                    number_customer_service_calls, family = quasibinomial(),
                  data = churn.train)

pchisq(summary(fit.overdis)$dispersion * fit.origin$df.residual,
       fit.origin$df.residual, lower.tail = FALSE)


# p240 ----
# penalty logistic regression ----
# !if (require('mlbench')) install.packages('mlbench')

library(mlbench)

# 환자의 당뇨병 여부 와 관련 임상 데이터가 저장된 라이브러리.
data(PimaIndiansDiabetes2)
str(PimaIndiansDiabetes2)

# 결측치 제거
PimaIndiansDiabetes3 <- na.omit(PimaIndiansDiabetes2)

library(caret)
set.seed(123)

# 7:3 으로 분할
train <- createDataPartition(y = PimaIndiansDiabetes3$diabetes, p = 0.7, list = FALSE)

diabete.train <- PimaIndiansDiabetes3[train,]
diabete.test <- PimaIndiansDiabetes3[-train,]

x <- model.matrix(diabetes ~., diabete.train)[,-1]
# 로지스틱 모델에 적용하기 위해서 char data를 numeric으로 변환
y <- ifelse(diabete.train$diabetes == 'pos', 1, 0)

library(glmnet)

set.seed(123)

# alpha = 1 : 라쏘회귀분석, alpha = 1 : 릿지회귀분석
diabete.cv <- cv.glmnet(x = x, y = y, family = 'binomial', alpha = 1)

diabete.cv$lambda.min

diabete.cv$lambda.1se

coef(diabete.cv, diabete.cv$lambda.min)

coef(diabete.cv, diabete.cv$lambda.1se)

# 예측정확도와 간명도는 서로 트레이드오프 관계에 있음.

diabete.gnet1 <- glmnet(x, y, family = 'binomial',
                        alpha = 1, lambda = diabete.cv$lambda.min)

diabete.test.x <- model.matrix(diabetes ~., diabete.test)[,-1]
diabete.pred1 <- predict(diabete.gnet1, newx=diabete.test.x, type = 'response')
diabete.pred1 <- ifelse(diabete.pred1 > 0.5, 'pos', 'neg')

# confusion matrix ----
table(diabete.test$diabetes, diabete.pred1, dnn = c("Actual", 'Predicted'))

mean(diabete.pred1==diabete.test$diabetes)

diabete.gnet2 <- glmnet(x, y, family = 'binomial',
                        alpha = 1, lambda = diabete.cv$lambda.1se)

diabete.test.x <- model.matrix(diabetes ~., diabete.test)[,-1]
diabete.pred2 <- predict(diabete.gnet2, newx=diabete.test.x, type = 'response')
diabete.pred2 <- ifelse(diabete.pred2 > 0.5, 'pos', 'neg')

table(diabete.test$diabetes, diabete.pred2, dnn = c("Actual", 'Predicted'))

mean(diabete.pred2==diabete.test$diabetes)


# multinomial logistic regression ----

