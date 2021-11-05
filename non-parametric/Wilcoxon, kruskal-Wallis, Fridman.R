# non-parametric test ----
# Wilcoxon rank sum test ----
# Mann-Whitney U test ----

# t test <-> Wilcoxon test
# independent sample ----
# Prob(수감률), 교육기간, 지역, 경찰, 예산, 인구, 실업률 등의 데이터가 포함되어있음.
# MASS -> UScrime
library(MASS)
str(UScrime)

# 지역에 따라 수감률의 차이가 있는지 검정
with(UScrime, by(Prob, So, median))

shapiro.test(UScrime$Prob)

wilcox.test(Prob ~ So, data = UScrime)



# paired sample test -----
# 보리수확량 데이터 
library(MASS)
str(immer)

sapply(immer[c('Y1', 'Y2')], median)

# paired = TRUE를 해주어야한다.
wilcox.test(immer$Y1, immer$Y2, paired = TRUE)


# 세 개 이상의 집단을 비교할 때 
# ANOVA <-> Kruskal-Wallis test
# Kruskal-Wallis test ----
# airquality 데이터셋을 활용해 월별 오존농도에 있어서 차이가 있는지 검정.
str(airquality)

with(airquality, by(Ozone, Month, median, na.rm = TRUE))

kruskal.test(Ozone ~ Month, data = airquality)



# Fridman test ----
if (!require('agricolae')) install.packages('agricolae')
# agricolae -> friedman
library(agricolae)

# judge : 참가자
# trt : 잔디
data(grass)
str(grass)

head(grass); tail(grass)
# | 뒤에 들어가는 변수는 집단별 관측값을 구분해주는 블록변수
friedman.test(evaluation ~ trt | judge, data = grass)


# agricolae package 안에 속해있는 friedman은 post - Hoc 까지 제공해준다. 
with(grass, friedman(judge, trt, evaluation, alpha = 0.05, 
                     group = FALSE, main = NULL, console = TRUE))

