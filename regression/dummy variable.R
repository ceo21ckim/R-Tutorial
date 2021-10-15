# dummy variable ----
# 선형회귀분석을 하기 위해서는 변수들이 모두 등간척도나 비율척도로 구성된 연속형 변수여야한다.
# 하지만 독립변수의 경우 명목척도나 서열척도와 같은 범주형 변수로 측정되었다 하더라도 이를 더미 변수로 변환해 분석을 한다.
# e.g. 남자는 1 여자는 0으로 변환.
# 더미변수는 기준을 0으로 두기 때문에 더미변수의 개수는 범주의 개수보다 하나 작다.

str(InsectSprays)

# lm 함수의 경우 변수가 factor인 경우 자동으로 더미변수로 인식해 계산한다.
levels(InsectSprays$spray)

tapply(InsectSprays$count, InsectSprays$spray, mean)

sprays.lm <- lm(count ~ spray, data = InsectSprays)
summary(sprays.lm)


# contrasts ----
# contrasts 함수를 사용하면 어떤식으로 더미화 되었는지 확인이 가능하다.

# 더미변수는 사후분석과 동일한 결과를 불러온다.
contrasts(InsectSprays$spray)

sprays.aov <- aov(count ~ spray, data = InsectSprays)
summary(sprays.aov)

TukeyHSD(sprays.aov)

# 기준 범주를 변경하기 위해서는 다음과 같은 코드를 작성한다.
# relevel ----
# relevel을 쓰면 마지막에 있는 범주가 기준범주로 바뀐다.
respray <- relevel(InsectSprays$spray, ref=6)
respray
?relevel

# ref 에 인자를 넣어도되고 변수값을 넣어도 된다.
respray <- relevel(InsectSprays$spray, ref='B')

sprays.lm <- lm(count ~ respray, data = InsectSprays)

summary(sprays.lm)

contrasts(relevel(InsectSprays$spray, ref='B'))
contrasts(relevel(InsectSprays$spray, ref=6))
