# hypothesis test ----
# d(density), p(probability), q(quantile), r(random)

# dnorm : ���Ժ��� Ȯ���е��Լ�
# pnorm : ���Ժ��� Ȯ�������Լ�
# qnorm : ���Ժ��� ��������Լ�
# rnorm : ���Ժ��� ���������Լ�


# dbinom ----
# ù��° �μ� : ����� �߻��� Ƚ�� x
# �ι�° �μ� : size ; n
# ����° �μ� : prob ; p

dbinom(7, size = 10, prob = 0.5)


# 0~7ȸ���� �߻��� Ȯ��.
pbinom(7, size = 10, prob = 0.5)
# same
sum(dbinom(0:7, size=10, prob=0.5))

# pbinom ----
# ����Ȯ���� �ݴ��� ������ Ȯ������ ���Ҷ� :  lower.tail=FALSE
pbinom(7, size = 10 , prob=0.5, lower.tail=FALSE)

pbinom(7, size=10, prob=0.5) - pbinom(3, size=10, prob=0.5)

prob <- pbinom(c(3,7), size=10, prob=0.5)
prob[2] - prob[1]

# diff ----
# �� ���� ���̸� �����ִµ� ���ʿ��� �������� ���� ���� Ȯ���� �� �ִ�.
diff(pbinom(c(3, 7), size=10, prob=0.5))

diff(pbinom(c(7, 3), size=10, prob=0.5))

# random���� size=10 �� ��� �������� ������ش�. 
rbinom(1, size=10, prob=0.5) # �����Ǵ� ��.
?rbinom

mean(rbinom(1000, size=10, prob=0.5))

# ���Ժ����� Ȯ�������Լ� 
# ��հ� ǥ�������� ���԰�, 110�� ���� Ȯ���� ���ϴ°�
# pnorm ----
pnorm(110, mean=100, sd=15)
pnorm(90, mean=100, sd=15, lower.tail=FALSE)

pnorm(110, mean=100, sd=15, lower.tail = FALSE)

# default normal distribution
pnorm(0)

pnorm(0, 0, 1)

# dnorm ----
# Ȯ���е��Լ�
# �ش� ���� ���̸� ���ϴ� ������ Ȯ������ ���ϴ� ���� �ƴϴ�.
dnorm(110, 100, 15)

pnorm(110, 100, 15) - pnorm(90, 100, 15)

diff(pnorm(c(90,110), 100, 15))

# qnorm ----
# 0.05 �ۼ�Ʈ�� �����ϴ� ��. 
# ����Ȯ�� 0.05 �� �κ��� ã���� 75.3272 �� ����ť�� ���� ����̶�� ��
qnorm(0.05, 100, 15)

# ���� 5%�� ����ť
qnorm(0.05, 100, 15, lower.tail=FALSE)

qnorm(0.95, 100, 15)

# default : normal distribution
qnorm(0.025)

# normal distribution ( mean = 100, sd = 15 ) �ΰſ��� �������� n���� ���
# rnorm ----
rnorm(1, mean=100, sd =15)

rnorm(5)

# �� 6���� �̾��ִµ� ����
rnorm(6, mean = c(-10, 0, 10), sd=1)
# 4����� ���ư��鼭 ���ش� 1, 2, 3, 4, 1, 2 �̷� ��
rnorm(6, mean = c(-10, 0, 10, 11), sd=1)

# t-test, chisq, unif, f ���� �����ϰ� ���� �����ϴ�. 
# shapiro.wilk test ----
set.seed(123)
shapiro.test((rnorm(100, mean = 100, sd = 15)))

shapiro.test(runif(100, min = 2, max = 4 ))

# qqnorm ----
# line�� �����Ҽ��� ���Ժ����� ������. 
set.seed(123)
qqnorm(rnorm(100, mean=100, sd=15), col='blue', main='Sample from Normal Distribution')
qqline(rnorm(100, 100, 15))

qqnorm(runif(100, 2, 4), col = 'red', main='Sample from Uniform Distribution')
qqline(runif(100, 2, 4))












