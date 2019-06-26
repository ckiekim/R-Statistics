# 7장. 여러 모집단의 평균 비교 검정
# 7-1. 모집단이 두개인 경우
data <- read.table("D:/Workspace/R_Statistics/ch07/chapter7.txt", header=T)

# 그림 7-1, 히스토그램
par(mar=c(2, 1, 1, 1))
hist(data$weight[data$gender==1], xlim=c(1500, 4500), ylim=c(0, 12), 
     col="orange", border=NA, main="", xlab="", ylab="", axes=F)
hist(data$weight[data$gender==2], density=10, angle=45, 
     add=TRUE, col="green")
axis(1)
abline(v = mean(data$weight[data$gender==1]), lty=1, lwd=3, col="orange")
abline(v = mean(data$weight[data$gender==2]), lty=1, lwd=3, col="green")
legends = c("여자아이", "남자아이")
legend("topright", legend=legends, 
       fill=c("orange", "green"), density=c(NA, 20))

boy <- subset(data, gender==1)
girl <- subset(data, gender==2)
# 정규성 테스트
shapiro.test(boy$weight)
qqnorm(boy$weight)
qqline(boy$weight)
shapiro.test(girl$weight)
qqnorm(girl$weight)
qqline(girl$weight)

iriss <- subset(iris, Species=='setosa')
shapiro.test(iriss$Sepal.Length)  # p-value > 0.05, 정규성 있음
qqnorm(iriss$Sepal.Length)
qqline(iriss$Sepal.Length)
shapiro.test(iriss$Petal.Width)   # p-value < 0.05, 정규성 없음
qqnorm(iriss$Petal.Width)
qqline(iriss$Petal.Width)

# 그림 7-3. 분산의 동일성 여부에 따른 차이
x <- seq(-3, 3, by=0.01)
y <- dnorm(x)

plot(x, y, type="l", xlim=c(-3, 3.5), ylim=c(0, 0.5), axes=FALSE)
axis(1)
lines(c(0, 0), c(0, max(y)), lty=3)
text(-0.3, max(y)+0.05, "① 평균 0, 표준편차 1")
arrows(-0.2, max(y)+0.03, 0, max(y), length=0.1)

y2 <- dnorm(x+0.5, mean=0.5)
lines(x+0.5, y2, col="red")
lines(c(0.5, 0.5), c(0, max(y2)), lty=3)
text(2.3, max(y2)+0.05, "② 평균 0.5, 표준편차 1")
arrows(2, max(y2)+0.03, 1, dnorm(1, mean=0.5), length=0.1)

y3 <- dnorm(x-1, mean=-1, sd=1.5)
lines(x-1, y3, col="blue")
lines(c(-1, -1), c(0, max(y3)), lty=3)
text(-2, max(y3)+0.05, "③ 평균 -1, 표준편차 1.5")
arrows(-2, max(y3)+0.03, -1.5, dnorm(-1.5, mean=-1, sd=1.5), length=0.1)

# 등분산성 테스트
var.test(data$weight ~ data$gender)
var.test(weight ~ gender, data=data)

# 2-sample T test
t.test(data$weight ~ data$gender, mu=0, alternative="less", 
       var.equal=TRUE )

# 예제 2. 식욕부진증 치료요법의 효과 검정
install.packages("PairedData")
library(PairedData)
data(Anorexia)
data <- Anorexia
str(data)

install.packages('psych')
library(psych)
summary(data)
describe(data)

n <- length(data$Prior - data$Post)
m <- mean( data$Prior - data$Post )
s <- sd (data$Prior - data$Post)
t.t <- m/(s / sqrt(n))
alpha <- 0.05
qt(alpha, df=16)
pt(t.t, df=16)    # 검정통계량으로부터 구한 유의확률

t.test(data$Prior, data$Post, paired=T, alternative="less")

