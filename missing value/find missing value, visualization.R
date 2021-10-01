
# missing value

# is.na() ----
# NA ���� TRUE�� ��ȯ�Ѵ�.
x <- c(1, 2, 3, NA)
is.na(x)

x[!is.na(x)]

# complete.cases() ----
# na.omit()
# ����̳� �����������ӿ��� �������� ���Ե��� ���� ���� �ĺ��Ѵ�.
# ������ �������� ���� �࿡ ���ؼ��� TRUE�� ��ȯ�ϰ�, �ϳ��� ����ġ�� ������ FALSE�� ��ȯ�Ѵ�.


str(airquality)

# complete.cases() == na.omit()
head(airquality[complete.cases(airquality),])

na.omit(airquality)


# reverse
airquality[!complete.cases(airquality),]

# sum, mean

# �� �ٸ��� ���ñ�?
# is.na�� NA���� ��� ���� �������� �Լ��̰�, complete.cases�� �ϳ��� NA�� ���� �������°Ŷ�
# 2���̻� NA���� ���� ��찡 ���� �� �ֱ� �����̴�. 
sum(is.na(airquality)) # 44

# 4.7%�� �ش��Ѵ�. 44 / (153*6) = 0.047
sum(!complete.cases(airquality)) 

# question1 ----
# �ϳ��󵵰� �ƴ϶� columns���� ����ġ�� ���� ���� �̻� �ִ� ���� �����ϰ� ������ ��� ���־���ұ�?
dt = data.frame(x = c(1, 2, 3, NA, 4), y = c(NA, 1, 2, 3, 4))
dt[complete.cases(dt),]

# �ϳ��� ����ġ�� row�� ������ ���
sum(!complete.cases(airquality)) # 42

# 27% �� �ش��Ѵ�. 42/153 = 0.27
mean(!complete.cases(airquality))



# visualization ----
if (!require(mice)) install.packages('mice')

library(mice)


# md.pattern() ----
# ��µǴ� 0�� 1�� �������� ��������� �����ش�.
# 1�� ��� �������� ���� ���� ����
# ������ ���ڴ� ������ ����
# �������� ���ڴ� �������� �߻��� ������ ���� 
# �Ʒ��� ���� �ش� ���� ����ġ ��

md.pattern(airquality)

# VIM package ----

if (!require(VIM)) install.packages('VIM')
library(VIM)

# aggr ----
# prop : default = TRUE, Ȯ������ ������, FALSE�� ����ġ�� ����.
# number : default = FALSE,�������� �� �����Ǵ� ������ ������ �ǹ��Ѵ�.
# sortVar : sort_values
miss <- aggr(airquality, prop=FALSE ,numbers=TRUE, sortVar=TRUE)

summary(miss)$combinations

# matrixplot ----
# sortby : �ش� ���� ������ ������ ���ش�.
# ������������ �������ش�. 
matrixplot(airquality, sortby= 'Month') # Month ��ſ� 5 ���ڷ� �־ ����.

# marginplot ----
# scatterplot�� Solar.R�� Ozone�� ���踦 �����ָ�, ������ ���� �����Ѵ�.
# Y�� margin region�� boxplot�� Solar.R �������� �����ϴ� ���� ������ ����� Ozone ������ ����.
# X�� margin region�� boxplot�� Ozone �������� �����ϴ� ���� ������ ����� Solar.R ������ ����.

# �������� ���������� ǥ�õǾ� �ִ�. (orangered)
# Y�� barplot �Ʒ��� ���ڴ� Ozone ������ ����ġ�� �߻��� ���̽��� ��
# X�� barplot �Ʒ��� ���ڴ� Solar.R ������ ����ġ�� �߻��� ���̽��� ��
# ������ ���������� �� ���� ��� ����ġ�� �߻��� ���̽��� �� 

marginplot(airquality[c('Solar.R', 'Ozone')],
           pch=20, col=c('cornflowerblue', 'orangered', 'purple'))


# correlation ----

# boolean ���� abs�� ���ָ� 0, 1 �� ��ȯ�ȴ�. 
x <- data.frame(abs(is.na(airquality)))
head(airquality)
head(x)

# ����ġ�� �ִ� columns�� �����´�.
# ����ġ�� �ִ� ���� �� �������� check�� �� �״� ���ǹ��ϴٰ��� �� �� ����.
y <- x[colSums(x) > 0]
cor(y)

with(y, cor.test(Ozone, Solar.R))

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

head(y)


y %>% 
  cor.test(Ozone, Solar.R)

with(y, cor.test(Ozone, Solar.R))


cor.test(y$Ozone, y$Solar.R)

