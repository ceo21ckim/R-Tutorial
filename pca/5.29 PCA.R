#5.29 Machine
#file Ch07selection 11p~ , old_IDAPILecture14 ,ESLII_print10 98p


#98p 3.61, 3.62 구해
y= c(56.25,    75,      115.625, 68.75,   96.875, 168.750,  84.375,  171.875, 109.375, 103.125)
x1=c(106.329, 144.726, 136.287, 154.430, 385.232, 585.544, 489.451, 445.992, 270.886, 163.291)
x2=c(90.756,  203.361, 672.269, 183.193, 140.336, 146.218, 184.874, 537.815, 309.244, 190.756)
x3=c(94.650,  131.687, 123.457, 113.169, 117.284, 152.263, 121.399, 150.206, 185.185, 139.918)
x4=c(162.791, 255.814, 191.860, 133.721, 174.419, 273.256, 255.814, 552.326, 534.884, 360.465)
x5=c(114.337, 112.632, 153.684, 116.842,  87.368,  94.737,  95.789, 113.684, 108.421, 106.321)

x<-cbind(1,x1,x2,x3,x4,x5)
x0=(x-mean(x))/sd(x) # centering 
svd(x)
u=svd(x)$u;v=svd(x)$v;d=diag(svd(x)$d) 



z=x%*%v #eigen vector 
z
x%*%v[,1]
sum(z[,1]*y)/sum(z[,1]^2)
n=nrow(x);p=ncol(x)-1

y


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
v

s(x)[1]*v[,1]+s(x)[2]*v[,2]+s(x)[3]*v[,3]+s(x)[4]*v[,4]+s(x)[5]*v[,5]+s(x)[6]*v[,6] #98p 3.62 Beta.hat

Beta.hat=0
for(i in 1:(p+1)){
  Beta.hat<-Beta.hat + s(x)[i]*v[,i]
}
Beta.hat #(3.62)




#centering 되지 않은 x들을 가지고 하면, 앞에 더해주지 않는다.
c(rep(mean(y),10)) + s(x)[1]*z[,1]+s(x)[2]*z[,2]+s(x)[3]*z[,3]+s(x)[4]*z[,4]+s(x)[5]*z[,5]+s(x)[6]*z[,6] #(3.61) 
fitted(lm(y~x))

#centering 된 x0 모델 앞에(1)col은 빼고

x0<-cbind((x1-mean(x1))/sd(x1),(x2-mean(x2))/sd(x2),(x3-mean(x3))/sd(x3),(x4-mean(x4))/sd(x4),(x5-mean(x5))/sd(x5))
z0=x0%*%svd(x0)$v
s(x0)
c(rep(mean(y),10)) + s(x0)[1]*z0[,1]+s(x0)[2]*z0[,2]+s(x0)[3]*z0[,3]+s(x0)[4]*z0[,4]+s(x0)[5]*z0[,5] #(3.61)
fitted(lm(y~x))

v0=svd(x0)$v
s(x0)[1]*v0[,1]+s(x0)[2]*v0[,2]+s(x0)[3]*v0[,3]+s(x0)[4]*v0[,4]+s(x0)[5]*v0[,5] #(3.62) Beta.hat
coef(lm(y~x0)) #(3.61) Beta.hat centering 된 x0로 만든 beta.hat과 같아.
coef(lm(((y-mean(y)/sd(y)))~x0)) 



beta1<-0
for (i in 1:(p+1)){
  z0<-x%*%v[,i]
  theta<-t(z0)%*%y/(t(z0)%*%z0)
  beta1<-beta1+c(theta)*v[,i]
}
beta1

x%*%v[,1]
t(x%*%v[,1])%*%y/(t(x%*%v[,1])%*%x%*%v[,1])

####

t(z0)%*%y
v[,1]
v


s(x)%*%v
c(s(x))%*%v
s(x0)

coef(lm(y~x1+x2+x3+x4+x5))
fitted(lm(y~x1+x2+x3+x4+x5))


