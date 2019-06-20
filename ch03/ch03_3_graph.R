# 그래프 그리기

# 그림 3.7 ~ 3.9
n <- 10
x <- 0:n

p <- 0.5
px <- dbinom(x, size=n, prob=p)
barplot(px, names=x, xlab="성공의 횟수(x)",
        ylab="확률(P[X=x])", main="B(10, 0.5)")

p <- 0.1
px <- dbinom(x, size=n, prob=p)
barplot(px, names=x, xlab="성공의 횟수(x)",
        ylab="확률(P[X=x])", main="B(10, 0.1)")

p <- 0.9
px <- dbinom(x, size=n, prob=p)
barplot(px, names=x, xlab="성공의 횟수(x)",
        ylab="확률(P[X=x])", main="B(10, 0.9)")

# 그림 3.11
n1 <- 5
n2 <- 10
n3 <- 30
p <- 0.3

x3 <- 1:n3
x2 <- 1:n2
x1 <- 1:n1

b3 <- dbinom(x3, size=n3, prob=p)
b2 <- dbinom(x2, size=n2, prob=p)
b1 <- dbinom(x1, size=n1, prob=p)

par(mar=c(4,4,0,0))
plot(x3, b3, type="l", ylim=c(0, 0.4), col="red",
     main="", xlab="x", ylab="P[X=x]")
points(x3, b3, pch=16, col="red")
lines(x2, b2, col="blue")
points(x2, b2, pch=17, col="blue")
lines(x1, b1, col="black")

#그림 3.12
x <- seq(-4, 4, by=0.01)
p <- dnorm(x, mean=0, sd=1)
#par(mar=c(4,4,0,0))
plot(x, p, type="l",  xlab="x", ylab="P(X=x)", lwd=2, col="red")

# 그림 3.13
set.seed(1)
x <- rnorm(10000, mean=170, sd=4)
hist(x, breaks=seq(150, 190, 1), right=F, freq=F)

x <- seq(-3, 3, by=0.01)
p <- dnorm(x, mean=0, sd=1)
plot(x, p, type="l")

(x.1 <- seq(-3, 0.5, length=500))
(p.1 <- dnorm(x.1, mean=0, sd=1))
(x.2 <- rep(x.1, each=2))
(p.2 <- rep(NA, 1000))
for(i in 1:1000) {
  if(i %% 2 == 0) {
    p.2[i] <- 0   
  } else {
    p.2[i] <- p.1[((i-1)/2)+1]
  }
}

plot(x, p, xlab="z", type="l")
lines(x.2, p.2, col="red")

# 표준정규분포 그림
z <- seq(-3, 3, by=0.001)
z.p <- dnorm(z)

# 그림 3-16
plot(z, z.p, axes=F, type="l", 
     main="표준정규분포 (90%)", ylab="", ylim=c(-0.04, 0.4))
axis(1)
lines(c(-3, 3), c(0, 0))
points(-1.645, -0.02, pch=17, col="red")
text(-1.645, -0.035, "-1.645", col="red")
points(1.645, -0.02, pch=17, col="red")
text(1.645, -0.035, "1.645", col="red")

s <- seq(-1.645, 1.645, by=0.001)
s.z <- dnorm(s, mean=0, sd=1)
s <- c(-1.645, s, 1.645)
s.z <- c(0, s.z, 0)

polygon(s, s.z, density=10, col="red")


# 그림 3-17
plot(z, z.p, axes=F, type="l", 
     main="표준정규분포 (95%)", ylab="", ylim=c(-0.04, 0.4))
axis(1)

lines(c(-3, 3), c(0, 0))
points(-1.96, -0.02, pch=17, col="red")
text(-1.96, -0.035, "-1.96", col="red")
points(1.96, -0.02, pch=17, col="red")
text(1.96, -0.035, "1.96", col="red")
