
# bootstrapping ----

# boot ----
# data : bootstrapping할 데이터셋
# statistic : 통계량을 계산하는 함수
# R : 생성할 표본의 개수


# R-sqruare에 대한 신뢰구간을 만드려고 함.
rsq <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(summary(fit)$r.square)
}


if (!require('boot')) install.packages('boot')
library(boot)

set.seed(123)
state <- data.frame(state.x77)

bs.results <- boot(data=state, statistic=rsq, R=1000, 
                   formula=Life.Exp ~ Income + Illiteracy)

bs.results

bs.results$t0

head(bs.results$t, 3) ; tail(bs.results$t, 3)

# Q-Q plot도 같이 제공해준다.
plot(bs.results)

# 신뢰구간을 제공해준다.
# boot.ci ----
# Percentile(perc), BCa(bca) 두가지 방법으로 신뢰구간을 계산하는데 서로 조금 다른 방법으로 신뢰구간을 계산한다.
boot.ci(bs.results, type = c('perc', 'bca'))


# multi statistic bootstrapping----
# 회귀계수의 신뢰구간을 구하기 위함.
coeffs <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

set.seed(123)
bs.results <- boot(data=state, statistic=coeffs, R=1000, 
                   formula = Life.Exp ~ Income + Illiteracy)

bs.results

# index를 조절해 원하는 값의 그래프를 볼 수 있다. 
plot(bs.results, index = 2)

boot.ci(bs.results, type = 'bca', index = 2)

boot.ci(bs.results, type = 'bca', index = 3)
