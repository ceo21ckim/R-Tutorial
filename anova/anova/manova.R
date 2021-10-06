# multivariable analysis of variance
# MANOVA ----

# load data
if (!require('heplots')) install.packages('heplots')
library(heplots)


str(Skulls)

attach(Skulls)

# 다변량 분석을 하기 위해 종속변수들을 결합하여 하나의 행렬로 변환해준다.
y <- cbind(mb, bh, bl, nh)

aggregate(y, by=list(epoch), FUN = mean)

# 각 집단의 평균이 차이가 있는지 확인하는 다변량 분산분석
skull.manova <- manova(y ~ epoch)
summary(skull.manova)


# 집단 차이가 있음을 확인했으니 어떤 집단이 차이가 나는지 확인할 필요가 있다. 
summary.aov(skull.manova)

# nh을 제외한 나머지 두개골 측정 값은 시대별로 차이가 있는 것을 확인할 수 있다. 