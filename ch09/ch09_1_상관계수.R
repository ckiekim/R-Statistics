# 9장. 상관과 회귀
# 9-1. 상관 계수
# 그림 9-1
set.seed(9)
rvnorm <- function(r) {
  x <- rnorm(50, 0, 1)
  y <- rnorm(50, r*x, sqrt(1-r^2))
  return(cbind(x,y))
}

par(mfrow=c(1, 3), mar=c(2, 2, 2, 1), oma=c(0,0,0,0))

r1 <- rvnorm(0.8)
plot(r1, main="r=0.8")
abline(lm(r1[,2] ~ r1[,1]), col="red")

plot(rvnorm(0), main="r=0")
abline(h=0, col="red")

r3 <- rvnorm(-0.8)
plot(r3, main="r=-0.8")
abline(lm(r3[,2] ~ r3[,1]), col="red")

par(mfrow=c(1, 1))

# 예제 9-1. 아버지와 아들 키의 공분산과 상관계수
hf <- read.table("http://www.randomservices.org/random/data/Galton.txt",  
                 header=T, stringsAsFactors = FALSE)
str(hf)
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]
str(hf.son)

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)
cov.num <- sum((hf.son$Father-f.mean) * (hf.son$Height - s.mean))
(cov.xy <- cov.num / (nrow(hf.son) - 1))
# R 함수를 이용한 공분산(표본)
cov(hf.son$Father, hf.son$Height) 

(r.xy <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height)))
# R 함수를 이용한 상관계수
cor(hf.son$Father, hf.son$Height)
