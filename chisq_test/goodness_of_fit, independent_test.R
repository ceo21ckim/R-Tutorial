# 독립성검정과 적합성검정

# chi-square test  ----

survivors <- matrix(c(1443, 151, 47, 1781, 312, 135), ncol = 2)
dimnames(survivors) <- list('Status' = c('minor injury', 'serious injury', 'dead'),
                            'Seatbelt' = c('With Seatbelt', 'Without Seatbelt'))

survivors

# 안전벨트 착용 유무
addmargins(survivors)

addmargins(prop.table(addmargins(survivors, 2), 2), 1)


barplot(survivors, ylim=c(0, 2500), las = 1,
        col = c('yellowgreen', 'lightsalmon', 'orangered'),
        ylab = 'frequency', main = 'frequency of Survivors')
legend(0.2, 2500, rownames(survivors),
       fill = c('yellowgreen', 'lightsalmon', 'orangered'))
survivors.prop <- prop.table(survivors, 2)

# 확률을 가지고 그래프
barplot(survivors.prop*100, las = 1, col = c('yellowgreen', 'lightsalmon', 'orangered'),
        ylab = 'Percent', main = 'Percent of Survivors')

# 귀무가설은 안전벨트 착용과 승객 안전 간에는 관련이 없다.
# 사고 이후 환자 상태 비율이 안전벨트 착용, 미착용와 동일하다라는 의미

# chisq value는 (관측빈도 - 기대빈도)^2 / 기대빈도

# 자유도는 (행 변수 범주개수 -1 ) * (열 변수 범주개수 -1)  = 2*1
pchisq(45.91, df=2, lower.tail = FALSE)

qchisq(0.05, df=2, lower.tail = FALSE)


# independence test ----
str(Titanic)

# margin은 column 순서대로 인 것 같다.
# 1 : Class
# 2 : Sex
# 3 : Age
# 4 : Survived
Titanic.margin <- margin.table(Titanic, margin = c(4, 1))
Titanic.margin

# margin.table에 Sum을 해준다.
addmargins((Titanic.margin))

# 1등석에 탄 사람들이 많이 살았다. 
addmargins(prop.table(addmargins(Titanic.margin, 2), 2), 1)


chisq.test(Titanic.margin)

library(vcd)
# vcd ----
# vcd 패키지는 관련성의 강도를 측정하는 지표(Phi-Coefficient, Contingency Coeff, Cramer's V)를 계산한다.
# 지표 값이 클수록 두 변수 간의 관련성이 크다는 것을 나타낸다.
assocstats(Titanic.margin)


mosaic(Titanic.margin, shade = TRUE, legend = TRUE)
mosaic(~Survived + Class, data = Titanic.margin, shader =TRUE, legend=TRUE)


# 팔짱을 끼었을 때 어느 손이 위쪽에 위치하는 데이터와 성별간 차이가 있는지 확인.
library(MASS)
str(survey)

# sex, fold

# 차이가 없다.
# 아래와 동일한 결과가 도출된다.
with(survey, chisq.test(Fold, Sex))

crosstab <- with(survey, table(Fold, Sex))
crosstab
chisq.test(crosstab)


# goodness of fit test ----
# 범주형 변수가 하나일 경우에는 범주별 비율 분포에 대한 가설을 검정할 수 있다.
# 소비자 단체에서 150명의 휴대전화 사용자를 대상으로 이용하고 있는 통신사를 조사하였다.
# A:60, B:55, C:35 이 데이터를 이용하여 세 통신사의 시장 점유율이 동일한지 검정하려고 한다.
# 이때 사용하는 검정 : 적합성검정(goodness of fit test)

# 각 카테고리별 갯수
chisq.test(c(60, 55, 35))

# 귀무가설 : 세 통신사의 시장점유율이 동일하다
# 시장 전문가가 A:45% , B:30%, C:25% 라고 주장한다. 한번 확인해보자.

oc <- c(60, 50, 35)
null.p <- c(0.45, 0.30, 0.25)

# 동일한 순서로 되어야한다. 관측빈도랑 확률!
chisq.test(oc, p=null.p)
# 타당하다고 볼 수 있다. 

# 매년 동일한 조사를 진행하고 있는데 작년 조사에 의하면 85명 중 A:45, B:25, C:15였다.
# 그럼 동일한지 확인해보자.

chisq.test(oc, p = c(45, 25, 15)/85)



# Hair  : 머리 색깔
# Eye : 눈 색깔
# Sex : 성별
str(HairEyeColor)

hairs <- margin.table(HairEyeColor, margin = 1)

# 갈색머리 : 50%, 검은색머리 : 25%, 금발머리 : 15%, 붉은색머리 : 10% 라고 주장한다.

# 생리학자가 주장하는 머리 색깔의 분포는 받아들이기 어렵다.
chisq.test(hairs, p=c(0.25, 0.5, 0.10, 0.15))

# data.frame으로 저장된 데이터는 적합성검정을 수행하기 위해서는 1차원 벡터형태로 만들어야한다.

smokers <- table(survey$Smoke)
smokers

chisq.test(smokers, p=c(0.1, 0.7, 0.1, 0.1))
