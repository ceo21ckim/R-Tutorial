# ANOVA ----
# ������ �����ϴ� ������ ������ �� ���� ��츦 one-way ANOVA��� �ϸ�, �� ���� ��� two-way ANOVA��� �Ѵ�.
# ����л�м�, ����л�м��� �̷л� ���������� �ؼ����� ��������� �Ѱ� ���� �ΰ��� ������ ������ �ٷ��.

# ���̰� ���ٴ� �͹������� �Ⱒ�� �� �ִ�. 
pf(9.59, df1 =1, df=8, lower.tail = FALSE)


qf(0.05, df1=1, df=8, lower.tail = FALSE)

# one-way ANOVA ----
# ���������� �������� ���� 12���� ���� ������ ������ �� ��Ƴ��� ������ ������ ����ߴ�.
# spray : ������ ����
# count : ��Ƴ��� ������ �� 
str(InsectSprays)

# ����� F�� ���� ũ��.
tapply(InsectSprays$count, InsectSprays$spray, mean)


# ǥ�������� F�� ���� ũ��. 
tapply(InsectSprays$count, InsectSprays$spray, sd)


# ���̴� �����ϴ�.
tapply(InsectSprays$count, InsectSprays$spray, length)


library('gplots')
plotmeans(count ~ spray, data = InsectSprays,
          barcol = 'tomato', col = 'cornflowerblue', lwd = 2, barwidth = 3,
          xlab = 'Type of Sprays', ylab = 'Insect Count', 
          main = 'Performance of Insect Sprays \n with 95% Confidence Interval of Mean')



boxplot(count ~ spray, data = InsectSprays, col = 'tomato', 
        xlab = 'Type of Sprays', ylab = 'Insect Count', 
        main = 'Performance of Insect Sprays')

spray.aov <- aov(count ~ spray, data = InsectSprays)
spray.aov


anova(lm(count ~ spray, data = InsectSprays))

# Df : ������
# Sum Sq : ������
# Mean Sq : �������� �������� ���� �л�
# F : MStreatment / MSerror
summary(spray.aov)



# ���ߺ�
# �� �л�м��� ���ؼ� ���� ���� ������ �� ����ȿ���� ���̴� ���ٶ�� �͹������� �Ⱒ�� �� �־���.
# �� ����� ������ ������ ����� ��� �����ϴٴ� ������ �Ⱒ�� �� ���� ���̸�, 
# ��� ������ ���� �޶� �̷��� ����� ���Դ��� ���� ���Ѵ�.
# ���� �Լ��� ����� ���� ���� �� ����� ������ Ȯ���� �� �ִ�. 

# model.table ----

# default type = 'effects' �� �������ָ� ���ܺ��� �� ������հ� ��ü ����� ���̸� �� �� �ִ�.
model.tables(spray.aov, type='mean')


model.tables(spray.aov, type='effects')

# anova�� ���� �� ����� ���̰� �ִٴ� ���� �ľ�������, � �������� ������ �ִ��� ���� �򰡸� �Ѵ�.

spray.compare <- TukeyHSD(spray.aov)

# p adj : ��� ���̿� ���� p-value
spray.compare

if (!require('multcompView')) install.packages('multcompView')
library(multcompView)

plot(spray.compare , las=1 , col="blue")



spray.compare$spray['D-C', ]


# mulcomp ----
# ���ݴ� �پ��� ���ߺ񱳸� ���� ����� �����Ѵ�.
# mcp(spray = 'Tukey') spray ������ ���� ���еǴ� ������ �������� Tukey ���ߺ񱳸� �����Ѵ�
# cld : glht() �Լ��� ��� ��ü�� ���� ��� ���ֽ��� �񱳰���� ���ĺ� ������������ ����Ѵ�.
# �� �� �׷����� �׸����ν� ������ ���� boxplot�� �����س� �� �ִ�. 

if (!require('multcomp')) install.packages('multcomp')
library(multcomp)

tuk.hsd <- glht(model=spray.aov, linfct=mcp(spray='Tukey'))
plot(cld(tuk.hsd, level=0.05), col = 'orange')

