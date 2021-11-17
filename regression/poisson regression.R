# poisson regression ----
if (!require('robust')) install.packages('robust')

# robust -> breslow.dat
library(robust)

data(breslow.dat)

str(breslow.dat)

seizure <- breslow.dat[c('Base', 'Age', 'Trt', 'sumY')]

summary(seizure)


hist(seizure$sumY, breaks = 20, col='cornflowerblue', 
     xlab = 'seizure Count', main = 'Distribution of Seizures')

seizure.poisson <- glm(sumY ~ Base + Age + Trt, data = seizure, family = poisson())
summary(seizure.poisson)

coef(seizure.poisson)


exp(coef(seizure.poisson))

# 과산포 ----
# 과산포는 종종 상태의존성이라는 현상으로 인해 발생함.
# 포아송분포는 각 사건이 독립적이고 사건발생률이 일정하다고 가정함.
# 잔차이탈도 대 잔차자유도의 비율이 1을 크게 상회하면 과산포를 의심해야함.
deviance(seizure.poisson) / df.residual(seizure.poisson)

if (!require('qcc')) install.packages('qcc')
library(qcc)

# qcc -> overdispersion.test ----
# 과산포 가능성을 검정하는 패키지.
qcc.overdispersion.test(seizure$sumY, type = 'poisson')

# 과산포 문제가 존재할때에도 포아송 분포를 사용할 수 있다. 
# family = poisson -> family = quasipoisson

seizure.qpoisson <- glm(sumY ~ Base + Age + Trt, data = seizure, 
                       family = quasipoisson())

summary(seizure.qpoisson)

# poisson을 가지고 회귀분석을 수행했을 때는 Age, Trt 모두 유의미한 변수라는 결과가 도출되었지만
# quasipoisson을 가지고 회귀분석을 수행했을 때는 Age, Trt 는 유의미한 변수가 아니라는 결과가 도출.

# 시간이 포함된 포아송 분포는 다음과 같다.
# 뇌전증 발작 연구에서 환자들이 10일에서 60일까지 실험에 참여했고 그 참여기간이
# 발작횟수와 함께 환자별로 time 변수에 지정되어있다고 가정.
glm(sumY ~ Base + Age + Trt, data = seizure, family = poisson(), offset = log(time))


# 파도로 인한 선박의 손상횟수(incidents)와 월 단위로 측정된 사용기간 데이터 : ships
library(MASS)
str(ships)

shipsinc <- subset(ships, service > 0 )

shipsinc$year <- factor(shipsinc$year)
shipsinc$period <- factor(shipsinc$period)

levels(shipsinc$year); levels(shipsinc$period)

shipsinc.poisson <- glm(incidents ~ type + year + period, data = shipsinc, 
                        family = poisson(), offset = log(service))

summary(shipsinc.poisson)

# 과산포 확인 
deviance(shipsinc.poisson) / df.residual(shipsinc.poisson)

library(qcc)
qcc.overdispersion.test(shipsinc$incidents, type = 'poisson')

shipsinc.qpoisson <- glm(incidents ~ type + year + period, data = shipsinc, 
                        family = quasipoisson(), offset = log(service))

summary(shipsinc.qpoisson)

exp(coef(shipsinc.qpoisson))

