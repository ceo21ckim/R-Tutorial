


test <- c(15, 20, 30, NA, 45)
test[test > 30]

test[test%%3!=0] # 값이 3으로 나누어 떨어지지 않는 요소 추출

test[is.na(test)] # NA인 요소 추출

test[!is.na(test)] # NA가 아닌 요소 추출

test[test%%2==0 & !is.na(test)] # 2의 배수면서 NA가 아닌 요소 추출

x <- 5
if(x>0){
  print('x is a positive value.')
} else if(x<0){
  print('x is negative value.')
} else {
  print('zero')
}

x <- -1
if(x>0) print('x is a positive value.') else if(x<0)print('x is negative value.') else print('zero')

x <- c(-5:5)
options(digits = 3) # 숫자 표현시 유효자릿수 3자리로 설정
sqrt(x)

# if else
sqrt(ifelse(x>= 0, x, NA))

# repeat ----
# repeat 문을 이용해 1~10까지 수 증가시키기
i <- 1
repeat {
  if (i > 10) {
    break
  } else {
    print(i)
    i = i + 1
  }
}


i <- 1 
while(i<11){
  print(i)
  i=i+1
}

i <- 1
while(i<10){
  print(paste(2, 'x', i, '=', 2*i)) ; i = i + 1
}


i <- 1
for (i in 2:9){
  print(paste('----', i, '단입니다', '----'))
  for (j in 1:9){
    print(paste(i, 'x', j, '=', i * j))
  }
}


for (i in 1:10){
  if (i%%2 == 0 ){
    print(i)
  }
}

# 소수찾기 ----
for( i in 1:10){
  check = 0 
  for (j in 1:i){
    if(i%%j == 0){
      check = check+1
    }
  }
  if (check == 2){
    print(i)
  }
}

# function -----
# factorial
fact <- function(x){
  fa <- 1
  while (x>1){
    fa = fa*x
    x = x-1
  }
  return(fa)
}


fact(5)
fact(6)



