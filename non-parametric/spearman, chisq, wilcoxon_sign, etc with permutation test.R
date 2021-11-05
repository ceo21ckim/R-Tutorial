# permutation test -----
# 두 집단의 차이 검정
# coin -> oneway_test() ----

library(coin)

score <- c(73, 68, 79, 85, 65, 77, 91, 88, 83, 95)
area <- factor(c(rep('A',5), rep('B', 5)))
math <- data.frame(area, score)


oneway_test(score ~ area, data =math, distribution = 'exact')
oneway_test(score ~ area, data =math, distribution = 'approximate')


t.test(score ~ area, data = math, var.equal = TRUE)

library(MASS)

# coin -> wilcoxon_test ----
# coin package를 이용하기 위해서는 factor로 변환시켜주어야한다. 
wilcox_test(Prob ~ factor(So), data = UScrime, distribution = 'exact')

# R에 내장된 함수와 동일한 결과값을 출력한다.
# 내장된 함수는 표본의 크기가 50 미만이면 모든 가능한 배열을 바탕으로 exact 검정을 수행하기 때문이다. 
wilcox.test(Prob ~ So, data = UScrime)


# 세 집단 이상 분석을 할 때에도 oneway_test를 사용한다.
library(multcomp)

str(cholesterol)

set.seed(123)
oneway_test(response ~ trt, data = cholesterol, distribution = 'approximate')

set.seed(123)
kruskal_test(response ~ trt, data = cholesterol, distribution = 'approximate')

# paired sample ----
# coin -> wilcoxonsign_test ----
# 대응표본일 경우 wilcoxon test가 아닌 wilcoxonsign_test()를 사용한다.

wilcoxsign_test(U1 ~ U2, data = UScrime, distribution = 'exact')

library(agricolae)
data(grass)

set.seed(123)
friedman_test(evaluation ~ trt | factor(judge), data = grass, distribution = 'approximate')


# 독립성검정 ----
# independent test ----

library(vcd)
str(Arthritis)

set.seed(123)
chisq_test(Treatment ~ factor(Improved, ordered = FALSE), data = Arthritis, 
           distribution = 'approximate')


# correlation test ----
# cor.test <-> spearman_test ----

str(Animals)

set.seed(123)
spearman_test(body ~ brain, data = Animals, distribution = 'approximate')
