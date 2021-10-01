# 602p ----
# stargazer ----
# R�� �⺻������ ��� ����� text�� ����ϴµ�, ��� ����� ������ ���̺� ���·� �����ϱ� ���ؼ��� ������ �۾��� �ʿ��ϴ�.
# ��� ����� �������� �������� �ٷ� ����� �� �ֵ��� �ϼ��� ���� ���̺��� �ٲ��� �� stargazer�� ����Ѵ�. 

if (!require(stargazer)) install.packages('stargazer')

library(stargazer)

data(mtcars)

head(mtcars)

stargazer(mtcars, type='html', title='Descriptive Statistics',
          digits = 1, out='cars.html')

# flip=TRUE �� �ϰԵǸ� index�� column�� ��ġ�� �ٲ��ش�. Transform
stargazer(mtcars, type='html', title='Descriptive Statistics',
          digits = 1, out='cars.html', flip=TRUE)

# covariate.labels
# �������� ������� �ʰ�, �����ϱ� ���� ��Ī�� ����Ҷ� ����Ѵ�.

stargazer(mtcars[c('mpg', 'hp', 'wt')], type='html', title='Descriptive Statistics',
          digits = 1, out='cars.html',
          covariate.labels = c('Mile per gallon', 
                               'Gross horsepower', 
                               'Weight (1000 lbs)'))

# summary = FALSE
# �����跮�� �ƴ� ������������ ���� ��ü�� ���̺� ���·� ����ϱ� ����.

stargazer(mtcars, type='latex', summary = FALSE, title='Descriptive Statistics', 
           digits =1, out = 'cars.html')

# correlation matrix ----
stargazer(cor(mtcars), type = 'latex', title = 'correlation Matrix', 
          digits = 1, out = 'cor.html')


# regression table ----

mtcars$highmpg <- factor(mtcars$mpg > mean(mtcars$mpg))
mtcars$gear <- factor(mtcars$gear)
mtcars$am <- factor(mtcars$am)
m1 <- lm(mpg~disp, data = mtcars)
m2 <- lm(mpg~disp + drat, data=mtcars)
m3 <- with(mtcars, lm(mpg~disp+drat+gear))
m4 <- glm(highmpg~disp+drat+am, family=binomial(link='logit'), data=mtcars)


# omit.stat : ��¿� ������ ��跮 
# LL = Log Likelihood, ser = ser, residual standard error, f = F statistic
# no.space = TRUE, �� ������ �������ش�.
# dep.var.labels, covariate.labels �� label�� �������ִ� ��.
stargazer(m1, m2, m3, m4, type='html', title = 'Model Comparison', out = 'cars.html',
          dep.var.labels = c('Miles per gallon', 'High MPG car'),
          covariate.labels = c('Displacement (cu.in.)', 
                               'Rear axle ratio', 
                               'Four gears', 
                               'Five gears',
                               'Transmission (manual=1)'),
          omit.stat=c('LL', 'ser', 'f'), no.space=TRUE)


# single.row = TURE, CI�� p-value�� ���� �ٿ� ǥ���ص� �Ǵ°�?
# ci : confidence interval �ŷڱ��� ����.
stargazer(m1, m2, m3, type='html', title = 'Model Comparison', out = 'cars.html',
          dep.var.labels = 'Miles per gallon',
          covariate.labels = c('Displacement (cu.in.)', 
                               'Rear axle ratio', 
                               'Four gears', 
                               'Five gears'
                               ),
          omit.stat=c('LL', 'ser', 'f'),  ci=TRUE, ci.level=0.95, single.row=FALSE)


# type = 'text' ���ϸ� ASCII text���·� ����� �ȴ�. 
stargazer(m1, m2, m3, type='text', title = 'Model Comparison', out = 'cars.txt',
          dep.var.labels = 'Miles per gallon',
          covariate.labels = c('Displacement (cu.in.)', 
                               'Rear axle ratio', 
                               'Four gears', 
                               'Five gears'
          ),
          omit.stat=c('LL', 'ser', 'f'),  ci=TRUE, ci.level=0.95, single.row=FALSE)




