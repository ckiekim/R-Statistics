# 5.2 구간추정
# 신뢰구간, 신뢰수준
par(mar=c(0,1,0,1))
x <- seq(-3, 3, by=0.01)
y <- dnorm(x)
plot(x, y, axes=F, type="l", ylim=c(-0.1, 0.5), xlab="", ylab="")
abline(h=0)
ll <- qnorm(0.025)
ul <- qnorm(0.975)
polygon(c(-3, x[x<ll], ll), c(0, y[x<ll], 0), col='red', density=20)
polygon(c(ul, x[x>ul], 3), c(0, y[x>ul], 0), col='red', 
        density=20, angle=135)

text(0, 0.2, expression(1-alpha))
text(-2.5, 0.1, expression(plain(P)(Z<z) == over(alpha, 2)), cex=0.7)
text(2.5, 0.1, expression(plain(P)(Z>z) == over(alpha, 2)), cex=0.7)
text(-1.96, -0.02, expression(-z[over(alpha, 2)]), cex=0.8)
text(1.96, -0.02, expression(z[over(alpha, 2)]), cex=0.8)

# 예제 5-4. 모평균에 대한 95% 신뢰구간
set.seed(9)
n <- 10
x <- 1:100
y <- seq(-3, 3, by=0.01)

smps <- matrix(rnorm(n * length(x)), ncol=n)
head(smps)
xbar <- apply(smps, 1, mean)
se <- 1 / sqrt(n)
alpha <- 0.05
z <- qnorm(1 - alpha/2)
ll <- xbar - z * se
ul <- xbar + z * se

windows()
plot(y, type="n", xlab="표본추출", ylab="z", 
     xlim=c(1, 100), ylim=c(-1.5, 1.5), cex.lab=1.8)
abline(h=0, col="red", lwd=2, lty=2)
l.c <- rep(NA, length(x))
l.c <- ifelse(ll * ul > 0, "red", "black")
arrows(1:length(x), ll, 1:length(x), ul, code=3, 
       angle=90, length=0.02, col=l.c, lwd=1.5)

# 예제 5-5. 모평균에 대한 95% 신뢰구간(모분산을 모를 때)
ci.t <- function(x, alpha=0.05) {
  n <- length(smp)
  m <- mean(x)
  s <- sd(x)
  t <- qt(1-(alpha/2), df=n-1)
  ll <- m - t * (s / sqrt(n))
  ul <- m + t * (s / sqrt(n))
  ci <- c(1-alpha, ll, m, ul)
  names(ci) <- c("Confidence Level", "Lower limit", "Mean", "Upper limit")
  return( ci )
}

smp <- c(520, 498, 481, 512, 515, 542, 520, 518, 527, 526)
ci.t(smp)
ci.t(smp, 0.1)
