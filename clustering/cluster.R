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

a <- aggregate(nutrition.scaled, by = list(cluster=clusters), mean)
n <- as.vector(table(clusters))
cbind(a, n)

# partitioning cluster analysis ----
# k-mean cluster analysis ----
head(state.x77)
state.scaled <- scale(state.x77)

set.seed(123)
nc <- NbClust(state.scaled, distance = 'euclidean', 
              min.nc = 2, max.nc = 15, method = 'kmeans')

table(nc$Best.nc[1,])

barplot(table(nc$Best.nc[1, ]), col = 'lightsteelblue', 
        xlab = 'Number of Cluster', ylab = 'Number of Supporting Index', 
        main = 'Number of Clusters Proposed by Indices')

set.seed(123)
# kmeans 는 cluster, centers, size를 반환한다.
clustering.km <- kmeans(state.scaled, centers = 3, nstart = 25)

clustering.km$cluster

clustering.km$centers

clustering.km$size

aggregate(state.x77, by = list(cluster = clustering.km$cluster), mean)

# cluster -> clusplot ----
# shade : 빗금
# lines = 0 : 군집간 거리를 나타내는 선을 삭제.
# labels = 2 : 개별 케이스 행 이름을 레이블로 나타냄.
library(cluster)
clusplot(x = state.x77, clus = clustering.km$cluster, color = TRUE, shade = TRUE, 
         labels = 2, lines = 0, main = 'Cluster Plot')

# PAM ----
# k-means ++
# partitioning around medoids cluster analysis 

if (!require('rattle')) install.packages('rattle')
library(rattle)

head(wine)

# cluster -> pam ----
set.seed(123)
clustering.pam <- pam(wine[-1], k = 3, stand = TRUE)

clustering.pam$clusinfo

clustering.pam$medoids

clustering.pam$id.med

clustering.pam$clustering

# labels = 4 : 군집 번호만 나타남.
clusplot(clustering.pam, color=TRUE, shade=TRUE, labels=4, lines=0, main = 'Cluster Plot')

result.pam <- table(wine$Type, clustering.pam$clustering, 
                    dnn=c('Actual', 'Clustered'))

result.pam

# 분류 정확도 89.3%
mean(wine$Type==clustering.pam$clustering)

# flexclust -> randIndex ----
# 일치 정도를 계량화 가능.
# 일치 정도가 0.7 정도면 나쁘지 않은 결과임을 나타냄.
# -1:완전 불일치, 1:완전일치
library(flexclust)
randIndex(result.pam)
