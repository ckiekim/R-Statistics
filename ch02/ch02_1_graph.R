# 2장
setwd('D:/Workspace/R_Statistics/ch02')

# 사전준비
data <- read.csv("2010년 인구사항.csv", header=F, na.strings=c("."))
str(data)
data$V1 <- factor(data$V1, levels=c(1, 2), labels=c("남자", "여자"))
data$V3 <- factor(data$V3, levels=1:14, 
                  labels=c("가구주",  "가구주의 배우자", "자녀",
                  "자녀의 배우자",  "가구주의 부모",  "배우자의 부모", 
                  "손자녀, 그 배우자", "증손자녀, 그 배우자",
                  "조부모", "형제자매, 그 배우자", 
                  "형제자매의 자녀,그 배우자","부모의 형제자매, 그 배우자",
                  "기타 친인척", "그외같이사는사람") )
data$V4 <- factor(data$V4, levels=1:8, 
                  labels=c("안 받았음", "초등학교", "중학교", 
                           "고등학교", "대학-4년제 미만", "대학-4년제 이상", 
                           "석사과정", "박사과정") )
str(data)
head(data); tail(data)

# 1. 그래프
library(ggplot2)
# 산점도
cars
par(mfrow=c(1,2))
plot(cars$speed, cars$dist, main="속도와 제동거리", 
     xlab="속도(mph)", ylab="제동거리(ft)", pch=1, col="red")
plot(jitter(cars$speed), jitter(cars$dist), 
     main="속도와 제동거리", 
     xlab="속도(mph)", ylab="제동거리(ft)", pch=1, col="red")
par(mfrow=c(1,1))

# 선 그래프
Nile
plot(Nile, main="Nile강의 연도별 유량 변화", 
     xlab="연도", ylab="유량")
plot(Nile, type='p', main="Nile강의 연도별 유량 변화", 
     xlab="연도", ylab="유량")
plot.ts(Nile)
plot(Nile)

df_nile <- as.data.frame(Nile)
head(df_nile)
year <- c(1871:1970)
df_nile$year <- year
ggplot(df_nile, aes(x=year, y=x)) +
  geom_line()

# Time series를 Dataframe으로 변환
df_Nile <- data.frame(year=time(Nile),
                      flood = as.matrix(Nile))
head(df_Nile)
ggplot(df_Nile, aes(x=year, y=flood)) +
  geom_line()

# 막대 그래프
tableV5 <- table(data$V5)
tableV5
barplot(tableV5, main="출생아(남자)별 빈도", 
        xlab="출생아수", ylab="빈도")

# 히스토그램
hist(data$V2, main="연령별 분포", xlab="연령", ylab="빈도")
par(mfrow=c(1,2))
hist(data$V2, breaks=c(seq(0, 90, 10)), right=F, 
     main="연령별 분포", xlab="연령", ylab="빈도")
hist(data$V2, probability=T, breaks=c(seq(0, 90, 10)), right=F,
     main="연령별 분포", xlab="연령", ylab="밀도")
par(mfrow=c(1,1))

# 원 그래프
table(data$V4)
pie(table(data$V4), main="학력수준별 비중", cex=0.8)
