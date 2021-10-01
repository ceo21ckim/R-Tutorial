# summarize

# Categorical variable ----

if (!require('MASS')) install.packages(MASS)
library(MASS)

str(survey)

# ������ ������ factor�� �����̴�.
# table ----
levels(survey$Smoke)

frqtab <- table(survey$Smoke)
frqtab

class(frqtab)

frqtab[2]

frqtab==max(frqtab)


frqtab[frqtab==max(frqtab)]

names(frqtab[frqtab==max(frqtab)])


# which.max ----
which.max(frqtab) # max value�� ��ġ�� ���� ���� ��ȯ���ش�.

frqtab[which.max(frqtab)]

names(frqtab[which.max(frqtab)])


# prop.table ----
frqtab.prop <- prop.table(frqtab)

frqtab.prop['Never']


# broadcasting �ؼ� ����� �ȴ�.
frqtab.prop * 100


# ����� ��, ������ ���� �� �ִ�.
sum(survey$Smoke == 'Never', na.rm = TRUE) / length(survey$Smoke)

mean(survey$Smoke == 'Never', na.rm=TRUE)

anorexia

mean(anorexia$Postwt > anorexia$Prewt)

# ���Ժ������� 2 sigma ���� �ָ� �ִ� �� ���� 99% �ۿ� �ִٰ� ǥ���Ѵ�.
mean(abs(mammals$brain - mean(mammals$brain)) > 2*sd(mammals$brain))

# diff ----
# diff �Լ��� �־��� ���ͷκ��� ���ӵ� �� ������ ���̸� ����Ͽ� ��ȯ�Ѵ�. 
mean(diff(SP500) > 0)



if (!require('vcd')) install.packages('vcd')
library(vcd)

# ����Ƽ�� ������ ������ 
str(Arthritis)

crosstab <- table(Arthritis$Improved, Arthritis$Treatment)
crosstab

crosstab['Marked', 'Treated']

?xtabs
# xtabs ----
crosstab <- xtabs(~ Improved + Treatment, data = Arthritis, sparse = FALSE)
crosstab


# margin.table ----
crosstab
# margin = 1 : row  / 2 : column
margin.table(crosstab, margin=1)

# �����ϴ� margin�� �����Ȱ�.
prop.table(crosstab, 2)

# margin�� ���� ������ ��ü ������ �ȴ�.
prop.table(crosstab)


# addmargins ----
addmargins(crosstab, margin = 1)


# ������ ��íǥ�� addmargins �Լ��� �����ϸ� sum column ���� �������־� ���ϴ�.
prop.table(crosstab, 2)

addmargins(prop.table(crosstab, 2), 1)

addmargins(prop.table(crosstab, 2), 2)


# CrossTable ----
# �پ��� ������ ��� �ִ� ����ǥ�� ������ �� �ִ�. 
if (!require('gmodels')) install.packages("gmodels")
library(gmodels)

# dnn �ȿ��� ���� �̸��� ���� �̸��� �������� �� �ִ�. 
# CRBD?
# randomized Complete Block Design
CrossTable(Arthritis$Improved, Arthritis$Treatment, prop.chisq=FALSE, 
           dnn = c('Improved', 'Treatment'))

# format : SAS, SPSS
CrossTable(Arthritis$Improved, Arthritis$Treatment, prop.chisq=TRUE, chisq=TRUE, fisher = TRUE, resid=TRUE, 
           dnn = c('Improved', 'Treatment'))



# ftable ----
# �Ѵ� �����ϴ�.
multtab <- with(Arthritis, table(Improved, Sex, Treatment)) # �������� ���°� �ΰ��� ������ֳ�.
multtab <- xtabs(~ Improved + Sex + Treatment, data = Arthritis)
multtab

ftable(multtab) # default : row.vars = c(1, 2), col.vars = c(3)


ftable(multtab, row.vars = c(1, 2, 3))

ftable(multtab, row.vars = c(1, 2))

ftable(multtab, col.vars = c(1, 2))

ftable(multtab, col.vars = c(1, 2, 3))


# 3���� Ȯ��
margin.table(multtab, 1)

margin.table(multtab, 2)

margin.table(multtab, 3)

margin.table(multtab, c(1, 3))


# Sex�� Treatment�� ���� Improved ���� ���� ���� �� �ִ�. 
ftable(prop.table(multtab, c(2, 3)))

ftable(addmargins(prop.table(multtab, c(2, 3)), 1))





# continuous variable ----
# median ----
median(survey$Pulse, na.rm=TRUE)

# quantile ----
quantile(survey$Pulse, probs = 0.05, na.rm = TRUE)

quantile(survey$Pulse, probs = 0.5, na.rm = TRUE)

quantile(survey$Pulse, probs = c(0.05, 0.95), na.rm = TRUE)

quantile(survey$Pulse, na.rm=TRUE)


# mean ----
mean(survey$Pulse <= 80, na.rm=TRUE)
iris.lst <- as.list(iris)
summary(iris.lst)

# range ----
range(survey$Pulse, na.rm=TRUE)

# var ----
var(survey$Pulse, na.rm=TRUE)

# sd ----
sd(survey$Pulse, na.rm=TRUE)

str(mtcars)


# pastecs ----
# nbr.val : ������ ����, nbr.null : null ����, nbr.na : na ����

if (!require('pastecs')) install.packages('pastecs')

library(pastecs)

stat.desc(mtcars[c('mpg', 'hp', 'wt')])


# psych ----
if (!require('psych')) install.packages('psych')

library(psych)

# skew : �ֵ�, kurtosis : ÷��
describe(mtcars[c('mpg', 'hp', 'wt')])

# tapply ----
# ù��° �μ����� ���� ������ ������ ���� �����Ϥ���,
# �ι�° �μ����� ������ ��Ÿ���� ������ ����
# ����° �μ����� ����ϰ��� �ϴ� ��跮 ��� �Լ�.
tapply(survey$Pulse, INDEX = survey$Exer, FUN=mean, na.rm=TRUE)

tapply(survey$Pulse, list(survey$Exer, survey$Sex), FUN=mean, na.rm=TRUE)


# aggregate ----
# ��������� ��� ���°� ���� �ٸ���.
aggregate(survey$Pulse, by = list(Exercise = survey$Exer), FUN=mean, na.rm=TRUE)

aggregate(survey$Pulse, by = list(Exercise = survey$Exer , Sex = survey$Sex), FUN=mean, na.rm=TRUE)


# aggregate�� tapply�� �޸� ������������ ������ �����ͼ��� ó���� �� �ִ�. 
# tapply�� ù��° �μ����� vector ���°� �����Ѵ�. 
aggregate(survey[c('Pulse', 'Age')],
          list(Exercise = survey$Exer), mean, na.rm=TRUE)

# ���� FUN�� lambdaó�� �������� �� ���ִ�. 
 myStats <- function(x, na.rm=FALSE){
   if (na.rm) x <- x[!is.na(x)]
   n <- length(x)
   mean <- mean(x)
   sd <- sd(x)
   skew <- sum((x-mean)^3/sd^3) / n
   kurt <- sum((x-mean)^4/sd^4) / n -3
   return (c(n=n, mean=mean, sd=sd, skewness = skew, kurtosis=kurt))
 }

 aggregate(survey[c('Pulse', 'Age')],
           list(Exercise = survey$Exer, Sex = survey$Sex), FUN=myStats, na.rm=TRUE)


# by ----
by(survey[c('Pulse', 'Age')], INDICES=list(Exercise = survey$Exer), FUN=summary)

aggregate(survey[c('Pulse', 'Age')], list(Exercise = survey$Exer), FUN=summary) 

by(survey[c('Pulse', 'Age')], list(Exercise = survey$Exer), FUN= function(x) sapply(x, myStats, na.rm=TRUE))

# describeBy ----
# describeBy �� aggregate�� by�� �޸� ����� ���� �Լ��� ������ ���Ѵ�. 

describeBy(survey[c('Pulse', 'Age')], group=list(Exercise = survey$Exer))
