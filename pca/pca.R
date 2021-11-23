# Principal component analysis ----

str(state.x77)

colnames(state.x77)

# prcomp ----
# princomp, psych::principal 다 가능.
# scale = TRUE로 지정하여 모든 변수가 표준편차 1을 갖도록 표준화 해준다. 
pca <- prcomp(state.x77, scale = TRUE)
summary(pca)

plot(pca, type = 'l', pch = 19, lwd = 2, col = 'red', main = 'Scree Plot')

# prcomp 함수는 서분적재값을 계산하여 주성분 객체의 rotation 원소에 행렬 형식으로 성분적재값(component loading)을 저장함.
# 성분 적재값은 성분과 변수 간의 상관 정도를 나타내며 값이 클수록 성분과 변수 간 관련성이 높다는 것을 의미.

pca$rotation

# 성분적재값을 바탕으로 각 성분을 기존 변수들의 선형결합으로 바꿀 수 있다. 
# 이렇게 선형결합으로 나온 성분 값을 성분점수(component score)라고 한다.
#
round(scale(state.x77) %*% pca$rotation, 3)

round(pca$x, 3)

# pca를 계산할 때 eigen value가 큰 부분을 하나의 eigen vector로 설정하고, 그 vector를 통해 만들어지는 span으로 기저를 만들고
# 그 기저로 변수들을 정사영시켜 
# PCA 의 성분점수로 출력하는 것으로 알고 있습니다. 
# 정사영 시키는 값이기 때문에 만약 데이터가 다양체로 구성되어 있다면 정사영 시킨 값이 작게 나타낼 것이고, 
# 이 경우 t-SNE로 각 변수간의 유사도를 반영하여 차원 축소를 한다고 알고 있습니다.
# 하지만 데이터를 직접 다루지 않고는 데이터셋이 manifold인지 linear인지 알 수 없습니다. 
# 그럴때에는 직접 해보는 수 밖에 없나요 ? 아니면 선형성 검정을 먼저 수행한 후 분석을 진행해야되는 건가요 ?

round(pca$x[,c(1, 2)], 3)

round(cor(pca$x), 3)

# biplot ----
# 주성분분석의 결과를 시각적으로 살펴볼 때 가장 많이 사용하는 함수
biplot(pca, cex=c(0.5, 0.75), main = 'Biplot')


