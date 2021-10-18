# list ----
# list는 데이터를 보관할 수 있도록 기존의 벡터를 확장한 또 하나의 벡터형태다.
# 기존 벡터는 단순히 값만 가지고 있다면, list는 그 외의 정보들이 포함되어있다.
# 하나의 모드가 아닌 다양한 모드(data.frame, vector, matrix, etc..)를 가질 수 있다.

# rnorm : 난수 생성
# set.seed() : 매번 동일한 난수를 가져오기 위해 쓰는 함수.
set.seed(123456)
list.normal <- rnorm(10)
list.normal

# 여러 정보를 담을 수 있다.
list.normal <- list(sample = rnorm(10),
                    dist = 'normal', 
                    param = list(mean = 0,
                                 std = 1)
                    )

list.normal

# list내부의 원소를 $ 로 출력이 가능하다.
list.normal$sample

list.normal$dist

list.normal$param

list.normal$param$mean

list.normal$param$std

set.seed(123456)
x <- c(rnorm(10))
print(x)

x > 0 & x < 1.0

# 만족시키는 값의 index를 불러온다.
which(x > 0 & x < 1.0)


# is.character ----
is.character(x)

x.c <- as.character(x)

is.character(x.c)

# vector는 같은 mode만 담을 수 있기 때문에 '1'을 character로 변환해 받는다.
c(1, 'a')

c(1>0, 2)

