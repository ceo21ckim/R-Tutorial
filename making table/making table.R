# 602p ----
# stargazer ----
# R은 기본적으로 출력 결과를 text로 출력하는데, 출력 결과를 문서에 테이블 형태로 삽입하기 위해서는 별도의 작업이 필요하다.
# 출력 결과를 보고서나 논문에서 바로 사용할 수 있도록 완성도 높은 테이블로 바꿔줄 때 stargazer를 사용한다. 

if (!require(stargazer)) install.packages('stargazer')

library(stargazer)

data(mtcars)

head(mtcars)

stargazer(mtcars, type='html', title='Descriptive Statistics',
          digits = 1, out='cars.html')

# flip=TRUE 를 하게되면 index와 column의 위치를 바꿔준다. Transform
stargazer(mtcars, type='html', title='Descriptive Statistics',
          digits = 1, out='cars.html', flip=TRUE)

# covariate.labels
# 변수명을 사용하지 않고, 이해하기 쉬운 명칭을 사용할때 사용한다.

stargazer(mtcars[c('mpg', 'hp', 'wt')], type='html', title='Descriptive Statistics',
          digits = 1, out='cars.html',
          covariate.labels = c('Mile per gallon', 
                               'Gross horsepower', 
                               'Weight (1000 lbs)'))

# summary = FALSE
# 요약통계량이 아닌 데이터프레임 내용 자체를 테이블 형태로 출력하기 위함.

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


# omit.stat : 출력에 제외할 통계량 
# LL = Log Likelihood, ser = ser, residual standard error, f = F statistic
# no.space = TRUE, 빈 라인을 제거해준다.
# dep.var.labels, covariate.labels 는 label을 지정해주는 것.
stargazer(m1, m2, m3, m4, type='html', title = 'Model Comparison', out = 'cars.html',
          dep.var.labels = c('Miles per gallon', 'High MPG car'),
          covariate.labels = c('Displacement (cu.in.)', 
                               'Rear axle ratio', 
                               'Four gears', 
                               'Five gears',
                               'Transmission (manual=1)'),
          omit.stat=c('LL', 'ser', 'f'), no.space=TRUE)


# single.row = TURE, CI와 p-value를 같은 줄에 표시해도 되는가?
# ci : confidence interval 신뢰구간 설정.
stargazer(m1, m2, m3, type='html', title = 'Model Comparison', out = 'cars.html',
          dep.var.labels = 'Miles per gallon',
          covariate.labels = c('Displacement (cu.in.)', 
                               'Rear axle ratio', 
                               'Four gears', 
                               'Five gears'
                               ),
          omit.stat=c('LL', 'ser', 'f'),  ci=TRUE, ci.level=0.95, single.row=FALSE)


# type = 'text' 로하면 ASCII text형태로 출력이 된다. 
stargazer(m1, m2, m3, type='text', title = 'Model Comparison', out = 'cars.txt',
          dep.var.labels = 'Miles per gallon',
          covariate.labels = c('Displacement (cu.in.)', 
                               'Rear axle ratio', 
                               'Four gears', 
                               'Five gears'
          ),
          omit.stat=c('LL', 'ser', 'f'),  ci=TRUE, ci.level=0.95, single.row=FALSE)





