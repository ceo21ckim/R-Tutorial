# 581p ----

# preprocessing with missing values
# listwise deletion ----
# listwise deletion�� �ϳ��� ����ġ�� ������ �����ϴ� ����̴�. 
# �� ��� �մܿ��� ����� complete.cases�� na.omit�� ����Ѵ�. 

cor(airquality[complete.cases(airquality),])

cor(na.omit(airquality))

anova(lm(Ozone~Temp, data = na.omit(airquality)))

# ������ ���
# method : perason, kendall, spearman 
# use : everthing, all.obs, complete.obs, na.or.complete, pairwise.complete.obs
cor(airquality, use='complete.obs') 

if (!require(corrplot)) install.packages('corrplot')
library(corrplot)

# pairwise deletion ----
cor(airquality, use='pairwise.complete.obs')

# corrplot ----
# method : circle, square, ellipse, number, shade, color, pie
corrplot(cor(airquality, use='pairwise.complete.obs'), method = 'circle')
plot(airquality)

# 37���� ����ġ�� ���ŵ� ���� �� �� �ִ�. 
# lm ������ pairwise deletion�� �ϴµ�, �� ��� ������ NA���� �����ϱ� ������ 
# �ٸ� ������ �� ��� ����ġ�� ������ �޶��� �м��� ������ ���� ���� �ִ�. 
air.lm <- lm(Ozone ~Temp, data=airquality)
summary(air.lm)


# simple imputation ----
# ��հ��̳� ������ �� �ϳ��� ������ ä���ִ� ���� ���Ѵ�. 
airquality.new <- airquality
for ( i in 1:ncol(airquality.new)){
  if(sum(is.na(airquality.new[,i])) > 0){ # ����ġ�� �ϳ��� �ִ� ���� ã�´�.
    na.idx <- which(is.na(airquality.new[,i])) # �ش� ��ġ�� idx�� �����Ѵ�.
    airquality.new[na.idx, i] <- mean(airquality.new[,i], na.rm=TRUE) # ������ idx�� mean���� ��ü�Ѵ�.
  }
}

mean(airquality$Ozone, na.rm=TRUE)
mean(airquality$Solar.R, na.rm=TRUE)

# multi imputation ----
# mice ----
# ����ġ �����ϴ� pack.���� Aemlia, mi ���� �ִ�. 
if (!require(mice)) iinstall.packages('mice')

library(mice)

# method : norm.predict, mean, pmm(predictive mean matching)
# �����Ͱ� �������� ��� pmm�� ����. 
# m : ������ ��ü���� ���� default = 5 
# maxit : �˰����� �ݺ� Ƚ�� default = 5
imp <- mice(airquality, method='mean', m=1, maxit=1)
imp

head(complete(imp))

# nhanes dataset
# columns : age, bmi, hyp(������ ����), chl(��û�ݷ����׷�)

str(nhanes)

head(nhanes)

imp <- mice(nhanes, seed = 123)
imp

# attributes : mice() �Լ��κ��� ��ȯ�� ��ü�� ����� ���� ����� �����ϴ� �Լ� 
attributes(imp)

imp$data

imp$imp
# 3��° ��ü �����ͼ��� ����.
c3 <- complete(imp, action=3)
md.pattern(c3)

# 5���� ��ü �����ͼ��� long format���� ����Ѵ�. 
c.long <- complete(imp, action = 'long')
c.long

c.median <- aggregate(c.long[3:6], by=list(id=c.long$.id), median)
head(c.median[-1])


ini <- mice(nhanes, maxit = 0 )
pred <- ini$predictorMatrix
pred
pred[,'hyp'] <- 0
pred

imp <- mice(nhanes, predictorMatrix = pred)
imp


# quickpred ----
# ���� ���ؿ� ���� �ڵ����� ���������� ������ �� �ִ�.
# �������� 0.3�̻��� �������� ����.

imp <- mice(nhanes, predictorMatrix = quickpred(nhanes,mincor=0.3))
imp


# nhanes2 : nhanes �����Ϳ� ����������, age�� hyp������ factor�� �Ǿ��ִ�.
str(nhanes2)

# data�� �������� ��� logistic reg. �� ����Ѵ�.
imp <- mice(nhanes2)
imp


# ��밡���� ����ġ ���� ����� Ȯ���� �� ����غ���.
methods(mice)


ini <- mice(nhanes2, maxit=0)
meth <- ini$method
meth['bmi'] <- 'norm.predict'

imp <- mice(nhanes2, method=meth)
imp

# �� ���� �����ϴ�.
summary(aov(lm(age~ bmi+hyp+chl, data = nhanes)))
anova(lm(age~ bmi+hyp+chl, data = nhanes))


# stripplot ----
# �������� ��ü��, �Ķ����� ������
imp <- mice(nhanes2, seed=123)
stripplot(imp, bmi~.imp, pch=21, cex=1.5)
stripplot(imp, pch=21, cex=1.2)

nhanes
head(nhanes)
# imp <- mice(nhanes2, m=1, seed=123)
fit <- with(imp, lm(chl~bmi+hyp))
fit

with(nhanes, lm(chl~bmi+hyp))


summary(fit$analyses[[3]])


# pool ----
# mice���� m=5�� �����߱� ������ 5���� �������� ��ճ��� lm�� ���ִ� �Լ�
pooled <- pool(fit)
pooled

summary(pooled)




