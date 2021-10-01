# 529p ----
11 + 12 + 13 # 36


# 533p ----
fah <- readline('Fahrenheit? ') # input
fah <- as.numeric(fah)
cel <- (fah-32) /1.8
print(paste('Celsius = ', cel))



# config ----
getwd()


path = "C:/Users/EonKim/Desktop"
setwd(dir = path)

# 작업 공간 저장 
save.image()


x <- 100
y <- c(2, 3, 5, 7)
z <- 3.14
hero <- c('Superman', 'Batman', 'Spiderman')
f <- function(y) (y-32) / 1.8
ls() # 변수 이름을 string으로 이루어진 하나의 vector로 반환한다.

ls(pattern = 'e') # hero

history()

# 어떤 library가 잇는지 찾아낸다.
library()


# library를 사용하기 위해서는 컴퓨터의 메모리에 적재를 시켜야하는데 
# 어떤 library들이 적재되어있는지 확인하는 함수.
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
search()

# 적재된 패키지를 없애기 위해서는 detach()를 사용
library(lattice)
search()

detach(package:lattice)
search()



# library의 path를 보여준다. 
.libPaths()


# check dataset ----
data(package = 'ggplot2')


head(cars) # default 6

tail(cars)

head(cars, 3)



# help ----

help.start()

help("median")
help(median)
?median

example(median)


help.search(median)
??median

help(xyplot, package = 'lattice')

example(xyplot, package = 'lattice')


# RSiteSearch() r환경을 벗어나지 않고 검색해준다.
# RSiteSearch() == sos.findFn()

RSiteSearch('topicmodels')


# data structure ----
# vector, factor, matrix, array, dataframe, list, etc..

# vector ----
vec1 <- c(2, 3, 5, 7, 11)
vec2 <- c('cat', 'dog', 'fox', 'horse', 'pig', 'rabbit')
vec3 <- c(TRUE, FALSE, TRUE)
vec4 <- c(4:-2)

vec2

vec2[3]

vec2[c(1, 3, 5)]

vec2[3:5]


# vector operation broadcasting

c(1:3) + c(4:9)
# 1, 2, 3, 1, 2, 3
# 4, 5, 6, 7, 8, 9

10 + c(1, 3, 5)

# concat string
fruit <- c('Apple', 'Banana', 'Strawberry')
food <- c('Pie', 'Juice', 'Cake')

# paste operation is broadcasting
paste(fruit, food)

# absolute
abs(-3:3)

# sqrt
sqrt(1:5)
sqrt(c(1:5))



# factor ----
review <- c('Good', 'Good', 'Indifferent', 'Bad', 'Good', 'Bad')
review.factor <- factor(review)

class(review.factor)
class(review) # type

str(review.factor) # info

summary(review.factor) # value_counts, describe


as.numeric(review.factor) # astype('int64')

levels(review.factor) # find label

levels(review.factor) <- c('B', 'G', 'I')

# 레벨을 변경하면 자동으로 바뀐다. 
review.factor


eval <- c('Medum', 'Low', 'High', 'Medium', 'High')
eval.ordered <- factor(eval, levels = c('Low', 'Medium', 'High'), ordered = TRUE)

# ordered = TRUE 를 안해주면 부등호가 생기지 않는다. 
# default : ordered = FALSE
eval.ordered

eval.factor <- factor(eval)

table(eval.factor) # summary(eval.factor)


# levels에 없는 값은 <NA>로 변경된다.
sex <- c(1, 2, 2, 1, 0, 1, 2)
sex.factor <- factor(sex, levels = c(1, 2), labels = c('Male', 'Female'))

sex.factor



# matrix ----
# default : byrow = F
matrix(data = 1:12, nrow = 3, ncol = 4, byrow = F) 

rnames <- c('R1', 'R2', 'R3')
cnames <- c('C1', 'C2', 'C3', 'C4')


matrix(data = 1:12, nrow = 3, ncol = 4, dimnames = list(rnames, cnames), byrow = T)

mat <- matrix(1:12, 3)
# 행또는 열만 추출하면 1-dim vector로 반환한다.
mat[2,]
mat[,3]


# same
mat[1:2, 3:4]
mat[c(1,2), c(3, 4)]

mat[2,, drop = F]



# array ----
ary <- array(data = 1:12, dim = c(2, 3, 2))
ary

ary[1, 3, 2]

ary[, 1, 2]


ary[1, 3, 2, drop=F]

ary[, 1, 2, drop=F]


# reduce dimention 
ary[2,,]



# dataframe ----
# 구조상 matrix와 동일한 구조를 가지지만 matrix와는 달리 각 열마다 다른 성분을
# 가질 수 있다는 것이 가장 큰 차이점이다. 
ID <- c(1:5)
name <- c('Mouse', 'Keyboard', 'USB', 'CPU', 'Monitor')
price <- c(30000, 90000, 45000, 550000, 250000)
madeby <- c('Logitech', 'Logitech', 'Samsung', 'Intel', 'Samsung')
country <- c('USA', 'China', 'Korea', 'USA', 'Korea')
product <- data.frame(ID, name, price, madeby, madein = country)

product


names(product) # columns


product[1:2]

product[, c('name', 'madeby')]


product$ID

# 교차표를 만들 수 있다. user-item table
table(product$madeby, product$madein)



# list ----
a <- 'List Example'
b <- 1:3
c <- c('one', 'two', 'three')
d <- matrix(1:12, nrow=3)
e <- data.frame(num=b, word=c)
f <- list(num=b, word=c)
g <- mean
h <- lm(mpg~wt, data=mtcars)
lst <- list(title = a, number = b, c, d, e, f, g, h)
lst



lst[[2]]

lst[['number']]

lst$number

lst <- list(one = 1, two = 2, three = list(alpha = 3, beta = 2))
lst

lst[['three']]

lst$three

lst[['three']][['beta']]

lst$three$beta


rainfall <- list(21.6, 23.6, 45.8, 77.0, 102.2, 133.3, 327.9, 348.0, 137.6, 49.3, 53.0, 24.9)
mean(rainfall) # error

mean(unlist(rainfall))


# trans type ----

# 롱 포맷과 와이드 포맷
# 와이드 포맷은 우리가 일반적으로 보는 dataframe
# 롱 포맷은 groupby를 취해준 dataframe 과 유사하다.
head(airquality)














