# factor analysis ----

if (!require('ade4')) install.packages('ade4')

library(ade4)

data("olympic")
str(olympic)

# psysh::fa.parallel ----
# fa.parallel 함수의 fm 인수에는 요인 추출 방법을 지정함.
# fm = 'ml'은 maximum likelihood를 지정하여 추출
library(psych)
fa.parallel(olympic$tab, fm='ml', fa='fa', n.iter = 100)

# nFactors::nScree ----
# 여기서는 총 네가지 방법이 적정 요인의 개수를 추정하는데 사용되었음.
library(nFactors)
nScree(olympic$tab)

eigen(cor(olympic$tab))

# factannal ----
# 요인분석을 수행할 때 사용
fa <- factanal(olympic$tab, factors = 2, scores = 'regression')
fa

olympic$tab

# loadings : 요인적재값을 확인할 수 있음.
# 큰 요인적재값을 확인하기 위해 default 는 0.1이라 더 작은 값을 보기 위해서는 cutoff = 0.01을 해주어야한다.
fa$loadings

# 고유요인에 의해 설명되는 분산은 요인분석 객체의 uniquenesses에 저장되어 있음.
# 분산 = 공통요인 + 고유요인
# 고유요인이 높을수록 좋지않음. .error
round(fa$uniquenesses, 3)

round(1-fa$uniquenesses, 3)

round(fa$loadings %*% t(fa$loadings), 3)

# 상관계수 행렬
round(cor(olympic$tab), 3)

# 잔차 행렬
round(cor(olympic$tab) - 
        (fa$loadings %*% t(fa$loadings) + diag(fa$uniquenesses)), 3)

# 잔차 행렬을 보면, 관측된 실제 상관계수에서 재현된 상관계수를 빼서 계산된다. 잔차가 크면
# 추출한 요인과 데이터 간의 적합도가 좋지 않다는 것을 나타낸다. 
factor.plot(fa, labels=colnames(olympic$tab), pch = 20, pos = 4, title = 'Factor Plot')


#gplots::heatmap2 ----
if (!require('gplots')) install.packages(gplots)
library(gplots) ; library(RColorBrewer)

heatmap.2(abs(fa$loadings), col = brewer.pal(9, 'Blues'), trace = 'none', key = FALSE, 
          dend = 'none', cexCol = 1.2, main = 'Factor Loadings')

# 경로도 ----
# semPlot::semPaths ----
if (!require('semPlot')) install.packages('semPlot')

library(semPlot)

# cut을 통해 0.3 보다 작은 요인적재값을 가진 인수는 그래프상에 나타나지 않도록 함.
# posCol 인자는 0.3보다 작은 값은 white로 처리해 보이지 않게 하고, 큰 값은 darkgreen으로 초록색을 띄게함.
semPaths(fa, what = 'est', residuals = FALSE, 
         cut = 0.3, posCol = c('white', 'darkgreen'), negCol = c('white', 'red'), 
         edge.label.cex = 0.75)

# 요인점수는 요인분석 객체의 scores에 저장됨.
fa.scores <- fa$scores
fa.scores

colnames(fa.scores) <- c('Run', 'Throw')
heatmap.2(fa.scores, col = brewer.pal(9, "GnBu"), trace = 'none', key = FALSE, 
          dend = 'none', cexCol = 1.2, main = 'Factor Scores by Athletes')


# psych::fa ----
# nfactors : 추출할 요인 수 
# rotate : 회전 방법 default = oblimin
# fm : 요인분석에 사용할 방법 default = minres
# minres(minimum residual), wls(weighted least square), gls(generalized weighted least square)
# pa(principal axis),ml(maximum likelihood)

library(psych)

fa <- fa(olympic$tab, nfactors = 2, rotate = 'varimax', fm = 'ml')
fa

fa$loadings

# psych::fa.diagram ----
# simple : FALSE로 하게 되면 그래프상에 나타낼 요인적재값의 크기를 지정할 수 있음.
# adj : 요인을 나타내는데 그래프에 표시할 위치를 정하는 것 e.g. 1이면 동일한 위치에 나열, 2이면 2군데 위치에 나누어 나열 (1~3)
# 숫자가 겹칠때 유용하게 사용할 수 있음.
# e.size : 요인을 둘러싼 타원형의 크기를 지정
fa.diagram(fa, simple=FALSE, cut = 0.3, digits = 2, col = 'blue', adj = 2, e.size = 0.05)

