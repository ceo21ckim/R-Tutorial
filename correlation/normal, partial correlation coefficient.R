library(MASS) ; library(dplyr) ; library(tidyr)
str(cats)

# cats %>% 
plot(cats$Hwt ~ cats$Bwt, 
     col = 'forestgreen', pch = 19,
     # xlab = 'Body Weight (kg)', ylab = 'Heart Weight (g)',
     main = 'Body Weight and Heart Weight of Cats')


# cor ----

cor(cats$Bwt, cats$Hwt)

with(cats, cor(Bwt, Hwt))

# cor의 method 인수에는 상관계수의 종류를 지정할 수 있다. 
# default : method = 'pearson', 'spearman', 'kendall'
# 결측치가 있을경우 use = 'everything' , use = 'pairwise.complete.obs'
# 피어슨 상관계수는 정규성의 가정을 필요로 한다. 그렇기 때문에 정규성을 만족하지 못할경우
# rank로 대체하여 상관계수를 계산할 수 있다.
# 스피어만은 정규성 가정을 충족하지 못할 경우 서열척도를 바탕으로 계산한다.
# 순위데이터를 가지고 하기 때문에 이상점에 덜 민감하다.

# cor.test ----
with(cats, cor.test(Bwt, Hwt))
# 상관관계가 존재한다. 

with(cats, cor.test(Bwt, Hwt, alternative = 'greater', conf.level = 0.99))

with(cats, cor.test(~Bwt + Hwt))

cor.test(~Bwt + Hwt, data = cats)

# subset 를 통해 성별이 여자인 고양이만 비교할수도 있다.
cor.test( ~ Bwt + Hwt, data = cats, subset = (Sex == 'F'))



str(iris)

cor(iris[-5])

iris.cor <- cor(iris[-5])

class(iris.cor)

str(iris.cor)

iris.cor['Petal.Width', 'Petal.Length']

library(psych)
corr.test(iris[-5])

print(corr.test(iris[-5]), short = FALSE)


options(digits=2)

cor(state.x77)

pairs.panels(state.x77, bg='red', pch=21, hist.col='gold',
             main = 'Correlation Plot of US States Data')


if (!require('corrgram')) install.packages('corrgram')
library(corrgram)
# corrgram ----
# panel.shade : 사선으로 표현된 셀은 정의 상관관계 / 색이 짙을수록 큰 상관관계를 나타냄
# panel.txt : 변수명 나열
corrgram(state.x77, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt,
         main = 'Corrgram of US States Data')

cols <- colorRampPalette(c('darkgoldenrod4', 'burlywood1', 'darkkhaki', 'darkgreen'))

corrgram(state.x77, order = FALSE, col.regions = cols, lower.panel=panel.pie,
         upper.panel = panel.conf, text.panel=panel.txt, main='Corrgram of US States Data')


# 편상관관계 ----

colnames(mtcars)

mtcars2 <- mtcars[, c('mpg', 'cyl', 'hp', 'wt')]
cor(mtcars2)
if (!require('ggm')) install.packages('ggm')
library(ggm)
# pcor ----
# 두 변수의 상관관계를 파악할 때 나머지 변수들은 통제해야한다. 
# 편상관계수를 통해 두 변수 간의 이러한 순수한 상관관계를 파악할 수 있다.
pcor(c(1, 3, 2, 4), cov(mtcars2))
pcor(c('mpg', 'hp', 'cyl', 'wt'), cov(mtcars2))

# pcor의 첫번째 인수에는 변수의 인덱스나 변수명을 지정한다.
# 두번째 인덱스는 편상관계수를 계산할 변수고, 나머지는 통제변수다.
# 위경우 cyl와 wt를 통제하니 mpg와 마력간의 상관관계는 -0.28로 1/3 수준이 되었다.

# q 는 통제할 변수의 개수, n은 관측값의 개수
pcor.test(pcor(c(1, 3, 2, 4), cov(mtcars2)), q = 2, n = nrow(mtcars2))

# cyl와 wt를 통제하면 연비와 마력 간 순수한 상관관계는 존재하지 않는다는 의미.
# 두 변수 간 드러난 상관관계는 상당 부분 실린더 개수와 무게로 인한 것이다.

# ppcor ----
if (!require('ppcor')) install.packages('ppcor')
library(ppcor)

# ppcor 패키지의 pcor함수와 pcor.test() 함수는 ggm 패키지의 함수명과 같다.
# 아무런 문제 없이 사용하기 위해서는 detach를 사용해서 제거해주어야한다.
pcor(mtcars2)


pcor.test(mtcars2['mpg'], mtcars2['hp'], mtcars2[c('cyl', 'wt')])

