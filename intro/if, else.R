read.csv('C:\\Users\\EonKim\\Desktop\\git_upload\\R\\sam.csv', encoding = 'utf-8')


test <- c(15, 20, 30, NA, 45)
test[test > 30]

test[test%%3!=0] # ���� 3���� ������ �������� �ʴ� ��� ����

test[is.na(test)] # NA�� ��� ����

test[!is.na(test)] # NA�� �ƴ� ��� ����

test[test%%2==0 & !is.na(test)] # 2�� ����鼭 NA�� �ƴ� ��� ����

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
options(digits = 3) # ���� ǥ���� ��ȿ�ڸ��� 3�ڸ��� ����
sqrt(x)

# if else
sqrt(ifelse(x>= 0, x, NA))

# repeat ----
# repeat ���� �̿��� 1~10���� �� ������Ű��
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
  print(paste('----', i, '���Դϴ�', '----'))
  for (j in 1:9){
    print(paste(i, 'x', j, '=', i * j))
  }
}


for (i in 1:10){
  if (i%%2 == 0 ){
    print(i)
  }
}

# �Ҽ�ã�� ----
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