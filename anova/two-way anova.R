# two-way analysis  of variance ----

# 10마리의 기니피그를 대상으로 비타민C가 이빨의 성장에 미치는 실험

# len : 이빨의 길이이며 종속변수로서의 역할
# supp :  비타민C 보충제
# dose : 0.5, 1.0, 2.0 밀리그램의 보충제 투여량

str(ToothGrowth)

ToothGrowth$dose <- factor(ToothGrowth$dose, levels = c(0.5, 1.0, 2.0),labels = c('Low', 'med', 'high'))

str(ToothGrowth)


ToothGrowth[seq(1,60,5), ]


with(ToothGrowth, tapply(len,list(supp, dose), length))

with(ToothGrowth, tapply(len,list(supp, dose), mean))
