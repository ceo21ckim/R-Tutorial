# two-way analysis  of variance ----

# 10������ ����Ǳ׸� ������� ��Ÿ��C�� �̻��� ���忡 ��ġ�� ����

# len : �̻��� �����̸� ���Ӻ����μ��� ����
# supp :  ��Ÿ��C ������
# dose : 0.5, 1.0, 2.0 �и��׷��� ������ ������

str(ToothGrowth)

ToothGrowth$dose <- factor(ToothGrowth$dose, levels = c(0.5, 1.0, 2.0),labels = c('Low', 'med', 'high'))

str(ToothGrowth)


ToothGrowth[seq(1,60,5), ]


with(ToothGrowth, tapply(len,list(supp, dose), length))

with(ToothGrowth, tapply(len,list(supp, dose), mean))