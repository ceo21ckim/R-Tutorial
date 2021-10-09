# ������������ ���ռ�����

# chi-square test  ----

survivors <- matrix(c(1443, 151, 47, 1781, 312, 135), ncol = 2)
dimnames(survivors) <- list('Status' = c('minor injury', 'serious injury', 'dead'),
                            'Seatbelt' = c('With Seatbelt', 'Without Seatbelt'))

survivors

# ������Ʈ ���� ����
addmargins(survivors)

addmargins(prop.table(addmargins(survivors, 2), 2), 1)


barplot(survivors, ylim=c(0, 2500), las = 1,
        col = c('yellowgreen', 'lightsalmon', 'orangered'),
        ylab = 'frequency', main = 'frequency of Survivors')
legend(0.2, 2500, rownames(survivors),
       fill = c('yellowgreen', 'lightsalmon', 'orangered'))
survivors.prop <- prop.table(survivors, 2)

# Ȯ���� ������ �׷���
barplot(survivors.prop*100, las = 1, col = c('yellowgreen', 'lightsalmon', 'orangered'),
        ylab = 'Percent', main = 'Percent of Survivors')

# �͹������� ������Ʈ ����� �°� ���� ������ ������ ����.
# ��� ���� ȯ�� ���� ������ ������Ʈ ����, ������� �����ϴٶ�� �ǹ�

# chisq value�� (������ - ����)^2 / ����

# �������� (�� ���� ���ְ��� -1 ) * (�� ���� ���ְ��� -1)  = 2*1
pchisq(45.91, df=2, lower.tail = FALSE)

qchisq(0.05, df=2, lower.tail = FALSE)


# independence test ----
str(Titanic)

# margin�� column ������� �� �� ����.
# 1 : Class
# 2 : Sex
# 3 : Age
# 4 : Survived
Titanic.margin <- margin.table(Titanic, margin = c(4, 1))
Titanic.margin

# margin.table�� Sum�� ���ش�.
addmargins((Titanic.margin))

# 1��� ź ������� ���� ��Ҵ�. 
addmargins(prop.table(addmargins(Titanic.margin, 2), 2), 1)


chisq.test(Titanic.margin)

library(vcd)
# vcd ----
# vcd ��Ű���� ���ü��� ������ �����ϴ� ��ǥ(Phi-Coefficient, Contingency Coeff, Cramer's V)�� ����Ѵ�.
# ��ǥ ���� Ŭ���� �� ���� ���� ���ü��� ũ�ٴ� ���� ��Ÿ����.
assocstats(Titanic.margin)


mosaic(Titanic.margin, shade = TRUE, legend = TRUE)
mosaic(~Survived + Class, data = Titanic.margin, shader =TRUE, legend=TRUE)


# ��¯�� ������ �� ��� ���� ���ʿ� ��ġ�ϴ� �����Ϳ� ������ ���̰� �ִ��� Ȯ��.
library(MASS)
str(survey)

# sex, fold

# ���̰� ����.
# �Ʒ��� ������ ����� ����ȴ�.
with(survey, chisq.test(Fold, Sex))

crosstab <- with(survey, table(Fold, Sex))
crosstab
chisq.test(crosstab)


# goodness of fit test ----
# ������ ������ �ϳ��� ��쿡�� ���ֺ� ���� ������ ���� ������ ������ �� �ִ�.
# �Һ��� ��ü���� 150���� �޴���ȭ ����ڸ� ������� �̿��ϰ� �ִ� ��Ż縦 �����Ͽ���.
# A:60, B:55, C:35 �� �����͸� �̿��Ͽ� �� ��Ż��� ���� �������� �������� �����Ϸ��� �Ѵ�.
# �̶� ����ϴ� ���� : ���ռ�����(goodness of fit test)

# �� ī�װ����� ����
chisq.test(c(60, 55, 35))

# �͹����� : �� ��Ż��� ������������ �����ϴ�
# ���� �������� A:45% , B:30%, C:25% ��� �����Ѵ�. �ѹ� Ȯ���غ���.

oc <- c(60, 50, 35)
null.p <- c(0.45, 0.30, 0.25)

# ������ ������ �Ǿ���Ѵ�. �����󵵶� Ȯ��!
chisq.test(oc, p=null.p)
# Ÿ���ϴٰ� �� �� �ִ�. 

# �ų� ������ ���縦 �����ϰ� �ִµ� �۳� ���翡 ���ϸ� 85�� �� A:45, B:25, C:15����.
# �׷� �������� Ȯ���غ���.

chisq.test(oc, p = c(45, 25, 15)/85)



# Hair  : �Ӹ� ����
# Eye : �� ����
# Sex : ����
str(HairEyeColor)

hairs <- margin.table(HairEyeColor, margin = 1)

# �����Ӹ� : 50%, �������Ӹ� : 25%, �ݹ߸Ӹ� : 15%, �������Ӹ� : 10% ��� �����Ѵ�.

# �������ڰ� �����ϴ� �Ӹ� ������ ������ �޾Ƶ��̱� ��ƴ�.
chisq.test(hairs, p=c(0.25, 0.5, 0.10, 0.15))

# data.frame���� ����� �����ʹ� ���ռ������� �����ϱ� ���ؼ��� 1���� �������·� �������Ѵ�.

smokers <- table(survey$Smoke)
smokers

chisq.test(smokers, p=c(0.1, 0.7, 0.1, 0.1))