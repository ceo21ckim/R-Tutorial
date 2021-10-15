# multiple regression ----

library(car)

# lty : 선의 타입
# lwd : 선의 굵기
scatterplot(income ~ education, data = Prestige, pch = 19, col = 'orangered', cex=1.2,
            regLine = list(method = lm, lty = 2, lwd = 2, col = 'royalblue'),
            smooth = list(smoother = loessLine, spread=FALSE, 
                          lty.smooth = 1, lwd.smooth=3, col.smooth='green3'),
            xlab = 'Education (years)', ylab = 'Income (dollars)', 
            main = 'Education and Income')

# LOESS(local regression, estimated scatterplot smoothing)
# 구간별로 가장 적합한 추세선을 찾는다. 
# 단일의 적합선으로 파악하기 어려운 추세나 주기를 탐색할 경우 유용하다. 
# 위 식의 경우 단일 추세보다 2차항을 갖는 모델을 사용할 경우 더 좋은 결과를 도출 할 수 있을 것 같다. 

# formula symbol
# : 독립변수 간의 상호작용을 나타낸다.
# * 독립변수 간의 모든 가능한 상호작용을 나타낸다.
# ^ 지정한 차수까지의 상호작용을 나타낸다. 
# . 데이터셋에 포함된 종속변수를 제외한 다른 모든 변수를 나타낸다.
# - 변수를 제외한다.
# I() 괄호 안의 수식을 산술적으로 해석한다.
library(dplyr)
Prestige.poly <- lm(income ~ education + I(education^2), data = Prestige)

summary(Prestige.poly)

# arrage() 함수는 첫 번째 인수로 주어진 데이터프레임을 두 번째 인수로 주어진 변수기준으로 오름차순 한다.
plot(Prestige$income ~ Prestige$education, pch = 19, col = 'darkorange', 
     xlab = 'Education (years)', ylab = 'Income (dollars)',
     main = 'Education and Income')
lines(arrange(data.frame(Prestige$education, fitted(Prestige.poly)), Prestige$education ),
      col = 'cornflowerblue', lwd=2)



data.frame(Prestige$education, fitted(Prestige.poly))

# spread = FALSE 를 하면 주위 spread를 없애준다.
scatterplot(eruptions ~ waiting, data = faithful, pch=19, col = 'deepskyblue', cex=1.2,
            regLine = list(method = lm, lty = 2, lwd = 2, col = 'blueviolet'),
            smooth = list(smoother = loessLine, spread=FALSE, lty.smooth = 1,
                          lwd.smooth=3, col.smooth='coral'),
            xlab = 'waiting (minutes)', ylab = 'Eruptions (minutes)', 
            main = 'Waiting Time Between Eruptions and the Duration of the Eruption')


# eruptions ~ waiting 은 3차 다항식(cubic polynomial) 으로 표현되는 것을 볼 수 있다.
faithful.poly <- lm(eruptions ~waiting + I(waiting^2) + I(waiting^3), data= faithful)

summary(faithful.poly)

faithful.lm <- lm(eruptions ~ waiting, data = faithful)

summary(faithful.lm)

# 3차 다항식이 R^2의 값이 더 좋아진 것을 볼 수 있다. 