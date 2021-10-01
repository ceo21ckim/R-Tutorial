str(InsectSprays)

spray.aov <- aov(count ~ spray, data = InsectSprays)

# car package ���� qqplot�� ���� ���Լ� ������ �ؾ��Ѵ�. 
library(car)

qqPlot(InsectSprays$count, pch = 20, col  = 'deepskyblue', id = FALSE,
       main = 'Q-Q plot', xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles')


# �� �׷����� ���� �׷��������δ� ��Ȯ�ϰ� ���Լ� ������ �ϱ� ����� ��� shapiro-Wilk test�� ����Ѵ�.

# shapiro.test ----
shapiro.test(InsectSprays$count)
# p - value�� ��û���� ���� ���� Ȯ���� �� �ִ�. 

# outlierTest ----
# car package �� outlierTest()�� ���� ��������� outlier�� ������ �� �ִ�.
# Bonferroni p ���� ���� outlier�� �ִ��� test�� �� �ִ�. 
outlierTest(spray.aov)


# leveneTest ----
# ���� ������ ������ ���� �� �л��� �����ϴٴ� ������ �ϰ� �м��� �Ѵ�.
# p-value�� �۰� ���Ա� ������ ��л��� ������ �� ����.
leveneTest( count ~ spray, data = InsectSprays)


# bartlett.test ----
# ��Ʋ�� ���� ���� ���� ������ �����ϰ� ��л��� üũ�ϴ� �м��̴�. 
bartlett.test(count ~ spray, data = InsectSprays)

#�� ��л� ���� �м��� ���� ��л��� ������ �������� ���� ��� oneway.test �Լ��� �̿��� �Ͽ��л�м��� �����Ѵ�.
# oneway.test�� anova�� ���̰� �ִ� ���� �� �� �ִ�.
# oneway.test�� ��л꼺�� �������� �ʴ� �ݸ�, anova�� ��л����� �����ϰ� �м��� �����ϱ� �����̴�.

oneway.test(count ~ spray, data = InsectSprays)

summary(aov(count ~ spray, data = InsectSprays))


