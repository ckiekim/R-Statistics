# 다양한 표본분포 그래프
par(mfrow=c(1,1), oma = c(0, 0, 0, 0))

# 그림 4.10 카이제곱 분포
df <- c(1, 3, 5, 10)
x <- seq(0, 20, by=0.01)
chi2.1 <- dchisq(x, df[1])
chi2.3 <- dchisq(x, df[2])
chi2.5 <- dchisq(x, df[3])
chi2.10 <- dchisq(x, df[4])

plot(x, type="n", xlim=c(0, 20), ylim=c(0, 0.3), 
     main="", xlab="x", ylab="", axes=F)
axis(1); axis(2)
lines(x, chi2.1, lwd=2, lty=1, col="black")
lines(x, chi2.3, lwd=2, lty=2, col="red")
lines(x, chi2.5, lwd=2, lty=3, col="blue")
lines(x, chi2.10, lwd=2, lty=4, col="green")
legend("topright", paste("df :", df), lty=1:4, 
       col=c("black","red", "blue", "green"), cex=0.7)

# 그림 4.11 T 분포
df <- c(1, 2, 8, 30)
x <- seq(-3, 3, by=0.01)
y <- dnorm(x)
t.1 <- dt(x, df=df[1])
t.2 <- dt(x, df=df[2])
t.8 <- dt(x, df=df[3])
t.30 <- dt(x, df=df[4])

par(mar=c(4,2,2,2))
plot(x, y, type="l", lty=1, axes=F, xlab="x", ylab="", col="red")
axis(1)
lines(x, t.1, lty=4, col="black")
lines(x, t.2, lty=3, col="magenta")
lines(x, t.8, lty=2, col="blue")
lines(x, t.30, lty=6, col="green")
legend("topright", paste("df :", df), lty=c(4, 3, 2, 6), 
       col=c("black", "magenta", "blue", "green"), cex=0.7)

# 그림 4.12 F 분포
df1 <- c(3, 10)
df2 <- c(5, 20)
x <- seq(0, 2, by=0.01)

f3.5 <- df(x, df1[1], df2[1])   # df(x, 3, 5)
f3.20 <- df(x, df1[1], df2[2])  # df(x, 3, 20)
f10.5 <- df(x, df1[2], df2[1])  # df(x, 10, 5)
f10.20 <- df(x, df1[2], df2[2]) # df(x, 10, 20)

plot(x, f3.5, type="l", ylim=c(0, 0.9), lwd=2, 
     axes=F, xlab="x", ylab="")
axis(1)
lines(x, f3.20, lty=2, lwd=2, col="blue")
lines(x, f10.5, lty=3, lwd=2, col="green")
lines(x, f10.20, lty=4, lwd=2, col="magenta")

legend("topright", paste("df :", c("3, 5", "3, 20", "10, 5", "10, 20")),
       col=c("black", "blue", "green", "magenta"), lty=1:4, cex=0.7)

# 참고자료: 포아송 분포
x <- 1:20

p.3 <- dpois(x, lambda=3)
p.5 <- dpois(x, lambda=5)
p.10 <- dpois(x, lambda=10)

plot(x, p.3, type="l", lwd="2", col="red",
     main="포아송 분포", xlab="x", ylab="P[X=x]")
points(x, p.3, pch=16, col="red")
lines(x, p.5, lwd="2", col="blue")
points(x, p.5, pch=17, col="blue")
lines(x, p.10, lwd="2", col="black")
points(x, p.10, pch=15, col="black")

legends <- c("lambda=3", "lambda=5", "lambda=10")
legend("topright", legend=legends, pch=c(16, 17, 15), 
       col=c("red", "blue", "black"))
