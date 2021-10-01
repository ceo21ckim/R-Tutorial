# hypothesis test ----
# d(density), p(probability), q(quantile), r(random)

# dnorm : 정규분포 확률밀도함수
# pnorm : 정규분포 확률분포함수
# qnorm : 정규분포 백분위수함수
# rnorm : 정규분포 난수생성함수


# dbinom ----
# 첫번째 인수 : 사건이 발생할 횟수 x
# 두번째 인수 : size ; n
# 세번째 인수 : prob ; p

dbinom(7, size = 10, prob = 0.5)


# 0~7회까지 발생할 확률.
pbinom(7, size = 10, prob = 0.5)
# same
sum(dbinom(0:7, size=10, prob=0.5))

# pbinom ----
# 누적확률을 반대편 영역의 확률값을 구할때 :  lower.tail=FALSE
pbinom(7, size = 10 , prob=0.5, lower.tail=FALSE)

pbinom(7, size=10, prob=0.5) - pbinom(3, size=10, prob=0.5)

prob <- pbinom(c(3,7), size=10, prob=0.5)
prob[2] - prob[1]

# diff ----
# 두 값의 차이를 보여주는데 왼쪽에서 오른쪽을 빼는 것을 확인할 수 있다.
diff(pbinom(c(3, 7), size=10, prob=0.5))

diff(pbinom(c(7, 3), size=10, prob=0.5))

# random으로 size=10 중 몇번 성공할지 출력해준다. 
rbinom(1, size=10, prob=0.5) # 관측되는 값.
?rbinom

mean(rbinom(1000, size=10, prob=0.5))

# 정규분포의 확률분포함수 
# 평균과 표준편차가 나왔고, 110이 넘을 확률을 구하는것
# pnorm ----
pnorm(110, mean=100, sd=15)
pnorm(90, mean=100, sd=15, lower.tail=FALSE)

pnorm(110, mean=100, sd=15, lower.tail = FALSE)

# default normal distribution
pnorm(0)

pnorm(0, 0, 1)

# dnorm ----
# 확률밀도함수
# 해당 값의 높이를 말하는 것이지 확률값을 말하는 것은 아니다.
dnorm(110, 100, 15)

pnorm(110, 100, 15) - pnorm(90, 100, 15)

diff(pnorm(c(90,110), 100, 15))

# qnorm ----
# 0.05 퍼센트가 차지하는 값. 
# 누적확률 0.05 인 부분을 찾으면 75.3272 의 아이큐를 가진 사람이라는 뜻
qnorm(0.05, 100, 15)

# 상위 5%의 아이큐
qnorm(0.05, 100, 15, lower.tail=FALSE)

qnorm(0.95, 100, 15)

# default : normal distribution
qnorm(0.025)

# normal distribution ( mean = 100, sd = 15 ) 인거에서 랜덤으루 n개를 출력
# rnorm ----
rnorm(1, mean=100, sd =15)

rnorm(5)

# 총 6개를 뽑아주는데 랜덤
rnorm(6, mean = c(-10, 0, 10), sd=1)
# 4개라면 돌아가면서 해준다 1, 2, 3, 4, 1, 2 이런 식
rnorm(6, mean = c(-10, 0, 10, 11), sd=1)

# t-test, chisq, unif, f 전부 동일하게 적용 가능하다. 
# shapiro.wilk test ----
set.seed(123)
shapiro.test((rnorm(100, mean = 100, sd = 15)))

shapiro.test(runif(100, min = 2, max = 4 ))

# qqnorm ----
# line에 근접할수록 정규분포를 따른다. 
set.seed(123)
qqnorm(rnorm(100, mean=100, sd=15), col='blue', main='Sample from Normal Distribution')
qqline(rnorm(100, 100, 15))

qqnorm(runif(100, 2, 4), col = 'red', main='Sample from Uniform Distribution')
qqline(runif(100, 2, 4))













