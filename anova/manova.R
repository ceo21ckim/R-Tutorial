# multivariable analysis of variance
# MANOVA ----

# load data
if (!require('heplots')) install.packages('heplots')
library(heplots)


str(Skulls)

attach(Skulls)

# �ٺ��� �м��� �ϱ� ���� ���Ӻ������� �����Ͽ� �ϳ��� ��ķ� ��ȯ���ش�.
y <- cbind(mb, bh, bl, nh)

aggregate(y, by=list(epoch), FUN = mean)

# �� ������ ����� ���̰� �ִ��� Ȯ���ϴ� �ٺ��� �л�м�
skull.manova <- manova(y ~ epoch)
summary(skull.manova)


# ���� ���̰� ������ Ȯ�������� � ������ ���̰� ������ Ȯ���� �ʿ䰡 �ִ�. 
summary.aov(skull.manova)

# nh�� ������ ������ �ΰ��� ���� ���� �ô뺰�� ���̰� �ִ� ���� Ȯ���� �� �ִ�. 