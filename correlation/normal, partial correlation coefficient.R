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

# cor�� method �μ����� �������� ������ ������ �� �ִ�. 
# default : method = 'pearson', 'spearman', 'kendall'
# ����ġ�� ������� use = 'everything' , use = 'pairwise.complete.obs'
# �Ǿ �������� ���Լ��� ������ �ʿ�� �Ѵ�. �׷��� ������ ���Լ��� �������� ���Ұ��
# rank�� ��ü�Ͽ� �������� ����� �� �ִ�.
# ���Ǿ�� ���Լ� ������ �������� ���� ��� ����ô���� �������� ����Ѵ�.
# ���������͸� ������ �ϱ� ������ �̻����� �� �ΰ��ϴ�.

# cor.test ----
with(cats, cor.test(Bwt, Hwt))
# ������谡 �����Ѵ�. 

with(cats, cor.test(Bwt, Hwt, alternative = 'greater', conf.level = 0.99))

with(cats, cor.test(~Bwt + Hwt))

cor.test(~Bwt + Hwt, data = cats)

# subset �� ���� ������ ������ �����̸� ���Ҽ��� �ִ�.
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
# panel.shade : �缱���� ǥ���� ���� ���� ������� / ���� £������ ū ������踦 ��Ÿ��
# panel.txt : ������ ����
corrgram(state.x77, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt,
         main = 'Corrgram of US States Data')

cols <- colorRampPalette(c('darkgoldenrod4', 'burlywood1', 'darkkhaki', 'darkgreen'))

corrgram(state.x77, order = FALSE, col.regions = cols, lower.panel=panel.pie,
         upper.panel = panel.conf, text.panel=panel.txt, main='Corrgram of US States Data')


# ��������� ----

colnames(mtcars)

mtcars2 <- mtcars[, c('mpg', 'cyl', 'hp', 'wt')]
cor(mtcars2)
if (!require('ggm')) install.packages('ggm')
library(ggm)
# pcor ----
# �� ������ ������踦 �ľ��� �� ������ �������� �����ؾ��Ѵ�. 
# ���������� ���� �� ���� ���� �̷��� ������ ������踦 �ľ��� �� �ִ�.
pcor(c(1, 3, 2, 4), cov(mtcars2))
pcor(c('mpg', 'hp', 'cyl', 'wt'), cov(mtcars2))

# pcor�� ù��° �μ����� ������ �ε����� �������� �����Ѵ�.
# �ι�° �ε����� ���������� ����� ������, �������� ����������.
# ����� cyl�� wt�� �����ϴ� mpg�� ���°��� �������� -0.28�� 1/3 ������ �Ǿ���.

# q �� ������ ������ ����, n�� �������� ����
pcor.test(pcor(c(1, 3, 2, 4), cov(mtcars2)), q = 2, n = nrow(mtcars2))

# cyl�� wt�� �����ϸ� ����� ���� �� ������ �������� �������� �ʴ´ٴ� �ǹ�.
# �� ���� �� �巯�� �������� ��� �κ� �Ǹ��� ������ ���Է� ���� ���̴�.

# ppcor ----
if (!require('ppcor')) install.packages('ppcor')
library(ppcor)

# ppcor ��Ű���� pcor�Լ��� pcor.test() �Լ��� ggm ��Ű���� �Լ����� ����.
# �ƹ��� ���� ���� ����ϱ� ���ؼ��� detach�� ����ؼ� �������־���Ѵ�.
pcor(mtcars2)


pcor.test(mtcars2['mpg'], mtcars2['hp'], mtcars2[c('cyl', 'wt')])
