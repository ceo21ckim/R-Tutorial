# matrix ----

A <- matrix(1:12, nr = 4)
A


B <- matrix(1:12, nr = 3)
B

dim(A) 

nrow(A)

ncol(A)


# determinant ---- 
# 행렬식 계산
A.1 <- A[c(1,2), c(1, 3)]
det(A.1)

A.i <- solve(A.1)
A.i

A.i %*% A.1

A.1 %*% A.i

# diag ----
D <-  diag(x = 1,nrow = 5,ncol = 5)
D


rep(c(1,3), 3)
# diag의 x인자에 matrix가 들어가면 대각선 인자를 가져온다.
diag(D)


# triangular matrix ----

upper.tri(A.1)

A.L <- A.1
A.L[upper.tri(A.L)] <- 0
A.L


A.U <- A.1
A.U[lower.tri(A.U)] <- 0
A.U


# cbind ----
cbind(1, A.1)

# rbind ----
rbind(1, A.1)
