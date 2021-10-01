# 데이터형 : 숫자형, 문자형, 범주형, 논리형, 특수 상수 등
# 연산자 : 산술, 비교, 논리 연산자
# 벡터 : 단일값들의 모임
# 배열 : 열과 행을 가지는 데이터 집합
# 데이터 프레임 : 서로 다른 데이터 형이 표 형태로 정리된 구조. 각 속성의 크기가 같음
# 리스트 : 데이터프레임과 유사한 구조를 띔.
# ctrl + L 을 사용하면 console 창이 지워진다.
a <- 1
b <- 2
c <- a + b

cat(c)


x <- 1
y <- 2
z <- x + y

# x + y = z 는 불가하다.
x + y -> z

# x와 y를 맞교환 하고 싶을 때에는 바로 바꾸는건 힘들고 temp라는 변수를 지정해 저장한 후 옮겨준다.
temp <- x
x <- y
y <- temp 

# 데이터형 확인
class(x)
is.integer(x)
is.numeric(x)
is.complex(x)
is.character(x)
is.na(x)


# 바꾸고 싶다면 as....
# L을 붙혀주면 정수로 표기된다.
is.integer(1)
is.integer(1L)
y
x
# 나머지
x %% y 

# 몫
x %/% y




1:7
c(1:5)


y <- c()

y <- c(y, c(1, 2, 3))
y


# seq =====
# seq  == sequence
seq(from = 1, to = 10, by = 2)

# length.out == 갯수를 지정
seq(0, 1, length.out = 11)

seq(0, 1, by = 0.1)


# rep ----
# 반복 벡터 생성
rep(c(1:3), times = 2)

rep(c(1:3), each = 2)

rep(1:3, 2)

# vector operate ----
x <- seq(2, 10, 2)
length(x)

x[1]

# x[1, 2, 3]

x[c(1:3)]

x[-c(1, 2, 3)]

x[c(1:3)]















