# create random number ----
set.seed(123456)
rnorm(n = 10)

rnorm(n = 10, mean = 2, sd = 2)

rchisq(n = 10, df = 5)

s <- c(1:10)
print(s)

# 비복원 추출
set.seed(123456)
sample(s, size = 5)

# 복원 추출
sample(s, size = 5, replace = TRUE)


set.seed(1234567)
sample(c('red', 'blue'),
       size = 5, 
       replace = TRUE, 
       prob = c(0.7, 0.3))


