# vector ----
# vector는 같은 성분만 들어가야한다. numeric, character....

# mode를 크게보면 총 3가지가 존재한다.
# 1. numeric, 2. character, 3. logical
x <- c(1:5)
print(x)

# vector
mode(x)

y <- c('a', 'b', 'c', 'd', 'e')

# character
mode(y)


names(x) <- y
x


z <- x>3
print(z)

# logical
mode(z)


x.s1 <- x[1:3]
print(x.s1)

x.s2 <- x[c('a', 'b', 'c')]
x.s2


x.s3 <- x[z]
x.s3


x[x>3]
