#5.31 Machine
y= c(56.25,    75,      115.625, 68.75,   96.875, 168.750,  84.375,  171.875, 109.375, 103.125)
x1=c(106.329, 144.726, 136.287, 154.430, 385.232, 585.544, 489.451, 445.992, 270.886, 163.291)
x2=c(90.756,  203.361, 672.269, 183.193, 140.336, 146.218, 184.874, 537.815, 309.244, 190.756)
x3=c(94.650,  131.687, 123.457, 113.169, 117.284, 152.263, 121.399, 150.206, 185.185, 139.918)
x4=c(162.791, 255.814, 191.860, 133.721, 174.419, 273.256, 255.814, 552.326, 534.884, 360.465)
x5=c(114.337, 112.632, 153.684, 116.842,  87.368,  94.737,  95.789, 113.684, 108.421, 106.321)

x<-cbind(1,x1,x2,x3,x4,x5)
u=svd(x)$u;v=svd(x)$v;d=svd(x)$d
n=nrow(x);p=ncol(x)-1

sum(d[1:4]^2)/sum(d^2)
sum(d[1:3]^2)/sum(d^2)
sum(d[1:2]^2)/sum(d^2)
sum(d[1]^2)/sum(d^2) #충분. 자동으로 95%를 넘는 k를 찾아서 Regression 을 만들어라.


s<-function(x){
  n=nrow(x);p=ncol(x)-1
  u=svd(x)$u;v=svd(x)$v;d=diag(svd(x)$d)
  seta=c()
  z=x%*%v
  for(i in 1:(p+1)){
    seta[i]=sum(z[,i]*y)/sum(z[,i]^2)
  }
  return(seta)
}
s(x)

s(x)[1]*v[,1]+s(x)[2]*v[,2]+s(x)[3]*v[,3]+s(x)[4]*v[,4]+s(x)[5]*v[,5]+s(x)[6]*v[,6] #98p 3.62 Beta.hat

coef(lm(y~x))
fitted(lm(y~x))

beta1<-0
for (i in 1:1){
  z0<-x%*%v[,i]
  theta<-t(z0)%*%y/(t(z0)%*%z0)
  beta1<-beta1+c(theta)*v[,i]
}
beta1

sum(abs(y-x%*%beta1))

beta2<-0
for (i in 1:2){
  z0<-x%*%v[,i]
  theta<-t(z0)%*%y/(t(z0)%*%z0)
  beta2<-beta2+c(theta)*v[,i]
}
beta2

sum(abs(y-x%*%beta2))

beta3<-0
for (i in 1:3){
  z0<-x%*%v[,i]
  theta<-t(z0)%*%y/(t(z0)%*%z0)
  beta3<-beta3+c(theta)*v[,i]
}
beta3

sum(abs(y-x%*%beta3))

beta4<-0
for (i in 1:4){
  z0<-x%*%v[,i]
  theta<-t(z0)%*%y/(t(z0)%*%z0)
  beta4<-beta4+c(theta)*v[,i]
}
beta4

sum(abs(y-x%*%beta4)) #4번째 까지 더했을 때가 가장 작은게 나온다!! 따라서 4번째 까지

beta5<-0
for (i in 1:5){
  z0<-x%*%v[,i]
  theta<-t(z0)%*%y/(t(z0)%*%z0)
  beta5<-beta5+c(theta)*v[,i]
}
beta5

sum(abs(y-x%*%beta5))

#d^2 즉 lambda의 비율을 통해서 Beta.hat , y.hat을 만드는 Algorithm

beta.hat<-function(x,alpha){
  n=nrow(x);p=ncol(x)-1
  u=svd(x)$u;v=svd(x)$v;d=svd(x)$d
  seta=c()
  z=x%*%v
  k=rep(p+2,p+1)
  B=rep(0,p+1)
  for(i in 1:(p+1)){
    seta[i]=sum(z[,i]*y)/sum(z[,i]^2)
  }
  seta
  
  for(i in 1:(p+1)){
    if((sum(d[1:i]^2)/sum(d^2))>=(1-alpha)){
      k[i]<-i
    }
    
  }
  k=min(k)
  
  for(i in 1:k){
    B<-B+seta[i]*v[,i]
  }
  return(B)
}
y.hat<-function(x,alpha){
  n=nrow(x);p=ncol(x)-1
  u=svd(x)$u;v=svd(x)$v;d=svd(x)$d
  seta=c()
  z=x%*%v
  k=rep(p+2,p+1)
  B=rep(0,p+1)
  for(i in 1:(p+1)){
    seta[i]=sum(z[,i]*y)/sum(z[,i]^2)
  }
  seta
  
  for(i in 1:(p+1)){
    if((sum(d[1:i]^2)/sum(d^2))>=(1-alpha)){
      k[i]<-i
    }
    
  }
  k=min(k)
  
  for(i in 1:k){
    B<-B+seta[i]*v[,i]
  }
  
  yhat=rep(0,n)
  
  for(i in 1:k){
    yhat<-yhat + seta[i]*z[,i]
  }
  
  return(yhat)
}

beta.hat(x,0.05)



#### file lecture-17 16p
circle <- function(x0, y0, r, n=1000, ...) { 
  theta <- seq(from=0, to=2*pi, length.out=n) 
  x <- x0 + r*cos(theta)  
  y <- y0 + r*sin(theta)  
  lines(x,y,...)  
  } 
plot(0,type="n",xlab=expression(beta[1]),ylab=expression(beta[2]), xlim=c(-10,10), ylim=c(-10,10)) #mtext 로 세워봐!
abline(a=10,b=-2) 
points(0,0) 
circle(0,0,sqrt(20),col="grey") 
points(4,2,col="black",pch=19) 
circle(0,0,5,col="grey",lty="dashed") 
circle(0,0,6,col="grey",lty="dashed")

#ESLII_print10 80p

#lecture17 
#11p
x1 <- rnorm(100, mean=70, sd=15) 
x2 <- rnorm(100, mean=70, sd=15) 
x3 <- (x1+x2)/2  #multico
x4 <- x1+runif(100,min=-100,max=100)  #multico, runif = random uniform
y <- 0.7*x1 + 0.3*x2 + rnorm(100, mean=0, sd=sqrt(15)) 
df <- data.frame(x1=x1, x2=x2, x3=x3, x4=x4, y=y)
pairs(df) #x3,x1 / x3,x2 등이 서로 직선관계가 나타난다.
cor(df)
var.x <- var(df[,c("x1","x2","x3","x4")]) 
#21p
library(ridge) 
demo.ridge <- linearRidge(y ~ x1 + x2 + x3 + x4, data=df, lambda="automatic") #lamda는 알아서 찾아준다.
summary(demo.ridge)
coefficients(demo.ridge)

demo.ridge$lambda

library(MASS) #Ch07Selection 25p 에 있어.
lm.ridge(y~x1+x2+x3+x4,df,lambda=seq(0,0.1,0.01)) #lambda값에 따라 Beta.hat이 다르다. 
#lambda 가 0 ~ 0.1 까지 0.01씩 늘리면서 보고있다.
#lambda 찾는건 다음시간에

#Homework 8-5
y= c(56.25,    75,      115.625, 68.75,   96.875, 168.750,  84.375,  171.875, 109.375, 103.125)
x1=c(106.329, 144.726, 136.287, 154.430, 385.232, 585.544, 489.451, 445.992, 270.886, 163.291)
x2=c(90.756,  203.361, 672.269, 183.193, 140.336, 146.218, 184.874, 537.815, 309.244, 190.756)
x3=c(94.650,  131.687, 123.457, 113.169, 117.284, 152.263, 121.399, 150.206, 185.185, 139.918)
x4=c(162.791, 255.814, 191.860, 133.721, 174.419, 273.256, 255.814, 552.326, 534.884, 360.465)
x5=c(114.337, 112.632, 153.684, 116.842,  87.368,  94.737,  95.789, 113.684, 108.421, 106.321)

x<-cbind(1,x1,x2,x3,x4,x5)
df <- data.frame(x1=x1, x2=x2, x3=x3, x4=x4,x5=x5, y=y)
ridge=linearRidge(y ~ x1 + x2 + x3 + x4+ x5, data=df, lambda="automatic")
linearRidge(y ~ x1 + x2 + x3 + x4+ x5, data=df, lambda="automatic")$lambda
l1=linearRidge(y ~ x1 + x2 + x3 + x4+ x5, data=df, lambda="automatic")$lambda[1] #lambda 1번으로 선택해볼겡
l2=linearRidge(y ~ x1 + x2 + x3 + x4+ x5, data=df, lambda="automatic")$lambda[2]
l3=linearRidge(y ~ x1 + x2 + x3 + x4+ x5, data=df, lambda="automatic")$lambda[3]

linearRidge(y ~ x1 + x2 + x3 + x4+ x5, data=df, lambda="automatic")$lambda
?linearRidge
## Ridge??
library(glmnet)

?glmnet
glmnet(x,y,alpha=1)#Lasso
glmnet(x,y,alpha=0) #Ridge
model.matrix(lm(y~x))
##
summary(ridge)
B=c(coef(ridge))
x%*%B

l1;l2;l3

#내가 해본 (3.47)

u=svd(x)$u;v=svd(x)$v;d=svd(x)$d
u[,1]%*%t(u[,1])%*%y

x%*%solve(t(x)%*%x + l1*diag(6))%*%t(x)%*%y #(3.47) 첫줄

xb<-0
for(i in 1:6){
  xb<-xb + u[,i]%*%t(u[,i])%*%y*(d[i]^2/(d[i]^2+l1))
}
xb

xb2<-0
for(i in 1:6){
  xb2<-xb2 + u[,i]%*%t(u[,i])%*%y*(d[i]^2/(d[i]^2+l2))
}
xb2

xb3<-0
for(i in 1:6){
  xb3<-xb3 + u[,i]%*%t(u[,i])%*%y*(d[i]^2/(d[i]^2+l3))
}
xb3

xb;xb2;xb3

b1=solve(t(x)%*%x+l1*diag(6))%*%t(x)%*%y #b (3.44)
b2=solve(t(x)%*%x+l2*diag(6))%*%t(x)%*%y
b3=solve(t(x)%*%x+l3*diag(6))%*%t(x)%*%y

coef(ridge)

cbind(x%*%b1,x%*%b2,x%*%b3,x%*%B)
