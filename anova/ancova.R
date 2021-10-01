
if (!require('faraway')) install.packages('faraway')
library(faraway)

# faraway  ----

# csa : 아동기의 성폭력을 경험했으면 Abused, 아니라면 NotAbused
# ptsd : 외상후 스트레스 장애 
# cpa : 공변량으로써 아동기의 신체적 학대

# 아동기의 성폭력 경험이 성인의 정신건강에 미치는 영향을 분석한 연구 
str(sexab)

tapply(sexab$ptsd, sexab$csa, mean)

tapply(sexab$ptsd, sexab$csa, sd)

tapply(sexab$ptsd, sexab$csa, length)

# cpa : 공변량
# csa : 독립변수
# ptsd : 종속변수
sexab.aov <- aov(ptsd ~ cpa + csa, data = sexab)

summary(sexab.aov)



# 공변량의 영향을 제거한 후의 조정된 외상 후 스트레스 장애의 집단 평균 파악
if (!require('effects')) install.packages('effects')
library(effects)

effect('csa', sexab.aov)

# HH package에 있는 ancova를 사용한다
# ancova ----

ancova(ptsd ~ cpa + csa, data = sexab)N
