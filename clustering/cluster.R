# clustering ----
# flexclust -> nutrient data set
if (!require('flexclust')) install.packages('flexclust')

library(flexclust)

data(nutrient)

head(nutrient, 5)


# dist default : method = euclidean ----
# lower triangle matrix ----
d <- dist(nutrient)
class(d)

labels(d)

as.matrix(d)[1:5,1:5]

# 명목형이나 비계량 데이터의 경우 유클리드거리를 이용하여 거리를 측정하는 것은 적합하지 않음.
# 이럴때 범주형 변수를 더미변수로 변환하여 계산함.
# method = binary
# (1, 1)로 일치하는 경우를 계산한 척도를 Jaccard similarity 라고 함.
# 다양한 유형의 데이터가 혼재하는 경우 daisy() 함수를 사용함. method = gower

# hierarchical cluster analysis ----

nutrition <- nutrient

# scale -> normalization ----
row.names(nutrition) <- tolower(row.names(nutrition))
nutrition.scaled <- scale(nutrition)
d <- dist(nutrition.scaled)

# hclust ----
# clustering
clustering.average <- hclust(d, method = 'average')
# 평균 연결법 : average 
# 완전 연결법 : complete
# 단일 연결법 : single
# 중심 연결법 : centroid
# 최소 분산 연결법 : ward.D, ward.D2

plot(clustering.average, hang = -1, cex = 0.9, col = 'darkgreen', 
     xlab = 'Food', main = 'Hierarchical Clustering with Average Linkage')


# NbClust -> NbClust ----
# 최적의 군집의 개수를 결정하는 다양한 패키지를 제공해줌.

if (!require('NbClust')) install.packages('NbClust')
library(NbClust)

nc <- NbClust(nutrition.scaled, distance = 'euclidean', 
              min.nc = 3, max.nc = 15, method = 'average')

# Best.nc 
nc$Best.nc

# 군집별 지지 받은 수
table(nc$Best.nc[1,])

barplot(table(nc$Best.nc[1,]),
        xlab = 'Number of Cluster', ylab = 'Number of Supporting Index', 
        main = 'Number of Clusters Proposed by Indices')

clusters <- cutree(clustering.average, k = 5)
clusters

table(clusters)

# 5개의 cluster에 box를 그려준다.
plot(clustering.average, hang = -1, cex = 0.9, col = 'darkgreen', 
     xlab = 'Food', 
     main = 'Hierarchical Clustering with Average Linkage \n Five Clusters')
rect.hclust(clustering.average, k = 5)

# 헤도닉가격모형에서 회귀분석에 들어가는 변수를 설정할 때 계층? 형태로 변수를 집어넣어 분석을 수행하여
# 가격의 변동을 파악하는 것으로 알고 있습니다. 
# 예를 들면 y = x1 + x2 + x3 형태로 구성되어있다면 x1 : 인구통계학적 변수(성별, 나이 등)
# x2 : 지리학적 변수(접면 수, 지하철, 버스 정류장 수 등), x3 : 경제적 변수( 일인당 GDP 등)
# 형태로 변수를 넣고 분석을 할 때 변수 내 변수를 설정하는데 있어서 클러스터링으로 변수를 파악하는지, 
# 아니면 연구자의 주관적인 판단으로 하는지 궁금합니다. 
# 만약 연구자의 주관적인 판단으로 한다고 가정하면 클러스터링으로 했을 시 보다 신빙성이 있다라든지 차이점이 있는지도 궁금합니다.