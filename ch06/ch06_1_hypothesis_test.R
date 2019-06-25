# 6장. 가설검정
# 6-1. 가설검정
data <- read.csv("D:/Workspace/R_Statistics/ch06/2010_6차.csv")
str(data)

tmp <- subset(data, data$나이==7 )
height.p <- tmp$X104.키

set.seed(9)
height <- height.p[sample(length(height.p), 15)]
height

mean(height)
sd(height)
t.test(height, mu=1220)

# 그림 6-4
par(mar=c(0.5,1,1,1))
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=14)
ll <- -ul
polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), col=2)
text(-2.5, 0.1, expression(plain(P)(T<t) == 0.025), cex=0.7)
text(2.5, 0.1, expression(plain(P)(T>t) == 0.025), cex=0.7)
text(ll, -0.02, expression(-t[0.025]==-2.14), cex=0.8)
text(ul, -0.02, expression(t[0.025]==2.14), cex=0.8)

# 그림 6-5의 세 개의 그림
par(mar=c(0,1,1,1))

windows()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))

# 양쪽검정
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=14)
ll <- -ul
polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), col=2)
text(-2.5, 0.1, expression(plain(P)(T<c[l]) == over(alpha, 2)))
text(2.5, 0.1, expression(plain(P)(T>c[u]) == over(alpha, 2)))
text(ll, -0.02, expression(c[l]==-t[alpha / 2]), cex=1.2)
text(ul, -0.02, expression(c[u]==t[alpha / 2]), cex=1.2)

# (왼쪽) 한쪽검정
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="", ylab="")
abline(h=0)
alpha <- 0.05
ll <- qt(alpha, df=14)
polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
text(-2.5, 0.1, expression(plain(P)(T<c[l]) == alpha))
text(ll, -0.02, expression(c[l]==-t[alpha]), cex=1.2)

# (오른쪽) 한쪽검정
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-alpha, df=14)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), col=2)
text(2.5, 0.1, expression(plain(P)(T>c[u]) == alpha))
text(ul, -0.02, expression(c[u]==t[alpha]), cex=1.2)

par(mfrow=c(1, 1))

# 그림 6-6
xbar <- mean(height)
mu0 <- 1220
s <- sd(height)
n <- length(height)
t.t <- (xbar - mu0) / (s / sqrt(n-1))

x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=14)
ll <- -ul
polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), col=2)

arrows(t.t, 0.05, t.t, 0, length=0.1)
text(t.t, 0.07, paste("t=", round(t.t, 3)))

text(ll, -0.02, expression(-t[0.025]==-2.14))
text(ul, -0.02, expression(t[0.025]==2.14))

# 그림 6-7
par(mar=c(0,1,1,1))
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), 
     main="", xlab="t", ylab="")
abline(h=0)
alpha <- 0.05
ul <- qt(1-(alpha/2), df=14)
ll <- -ul

polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), col=2)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), col=2)
text(ll, -0.02, expression(-t[0.025]==-2.14))
text(ul, -0.02, expression(t[0.025]==2.14))

t.t <- 0.727
p.value <- 1 - pt(t.t, df=14)
polygon(c(t.t, x[x>t.t], 3), c(0, y[x>t.t], 0), density=20, angle=45)
text(t.t, -0.02, paste("t=", round(t.t, 3)))

text(1.7, 0.2, paste("P(T>t)=",round(p.value, 3)))
arrows(1.7, 0.18, 1.5, dt(1.5, df=14), length=0.1)
