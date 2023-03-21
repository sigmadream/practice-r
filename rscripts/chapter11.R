# Chapter 11

# 실습: 전체 데이터 셋의 특성 보기
# 단계 1: 실습 데이터 셋 가져오기 
setwd("C:/Rwork/Part-III")
data <- read.csv("descriptive.csv", header = TRUE)
head(data)

# 단계 2: descriptive.csv 데이터 셋의 데이터 특성 보기 
dim(data)
length(data)
length(data$survey)
str(data)

# 단계 3: 데이터 특성(최소값, 최대값, 평균, 분위수, 결측치(NA) 등) 제공
summary(data)

# 실습: 성별(gender) 변수의 기술 통계량과 빈도수 구하기 
length(data$gender)
summary(data$gender)
table(data$gender)


# 실습: 이상치(outlier) 제거 
data <- subset(data, gender == 1 | gender == 2)
x <- table(data$gender)
x
barplot(x)


# 실습: 구성 비율 계산
prop.table(x)
y <- prop.table(x)
round(y * 100, 2)


# 실습: 학력 수준(level) 변수를 대상으로 구성 비율 구하기 
length(data$level)
summary(data$level)

table(data$level)


# 실습: 학력 수준(level) 변수의 빈도수 시각화하기 
x1 <- table(data$level)
barplot(x1)


# 실습: 만족도(survey) 변수를 대상으로 요약 통계량 구하기
# 단계 1: 등간척도 변수 추출
survey <- data$survey
survey

# 단계 2: 등간척도 요약 통계량
summary(survey)


# 실습: 등간척도 빈도분석
x1 <- table(survey)
x1


# 실습: 등간척도 시각화하기 
hist(survey)

pie(x1)


# 실습: 생활비(cost) 변ㄴ수 대상 요약 통계량 구하기 
length(data$cost)
summary(data$cost)

# 실습: 데이터 정제(결측치 제거)
plot(data$cost)
data <- subset(data, data$cost >= 2 & data$cost <= 10)
x <- data$cost
mean(x)


# 실습: 생활비(cost) 변수를 대상으로 대표값 구하기 
# 단계 1: 평균과 중위수 구하기 
mean(x)
median(x)

sort(x)
sort(x, decreasing = T)

# 단계 2: 사분위수 구하기 
quantile(x, 1/4)
quantile(x, 2/4)
quantile(x, 3/4)
quantile(x, 4/4)

# 실습: 생활비(cost) 변수의 최빈수 구하기 
# 단계 1: 최빈수는 빈도수가 가장 많은 변량을 의미
length(x)
x.t <- table(x)
max(x.t)


# 단계 2: 두 개의 행을 묶어서 matrix 생성
x.m <- rbind(x.t)
class(x.m)
str(x.m)
which(x.m[1, ] == 18)

# 단계 3: 데이터프레임으로 변경
x.df <- as.data.frame(x.m)
which(x.df[1, ] == 18)

# 단계 4: 최빈수와 변량 확인
x.df[1, 19]
attributes(x.df) 
names(x.df[19])



# 실습: 생활비(cost) 변수를 대상으로 산포도 구하기 
var(x)
sd(x)
sqrt(var(data$cost, na.rm = T))


# 실습: 생활비(cost) 변수의 빈도분석과 시각화하기 
# 단계 1: 연속형 변수의 빈도분석
table(data$cost)

# 단계 2: 연속형 변수의 히스토그램 시각화
hist(data$cost)

# 단계 3: 연속형 변수의 산점도 시각화
plot(data$cost)


# 단계 4: 연속형 변수 범주화 
data$cost2[data$cost >= 1 & data$cost <= 3] <- 1
data$cost2[data$cost >= 4 & data$cost <= 6] <- 2
data$cost2[data$cost >= 7] <- 3

# 단계 5: 범주형 데이터 시각화 
table(data$cost2)
par(mfrow = c(1, 2))
barplot(table(data$cost2))
pie(table(data$cost2))


# 실습: 패키지를 이용한 비대칭도 구하기 
# 단계 1: 왜도와 첨도 사용을 위한 패키지 설치 
install.packages("moments")
library(moments)
cost <- data$cost

# 단계 2: 왜도 구하기 
skewness(cost)

# 단계 3: 첨도 구하기 
kurtosis(cost)

# 단계 4: 히스토그램으로 왜도와 첨도 확인
hist(cost)
par(mfrow = c(1, 1))


# 실습: 히스토그램과 정규분포 곡선 그리기 
hist(cost, freq = F)
lines(density(cost), col = 'blue')
x <- seq(0, 8, 0.1)
curve(dnorm(x, mean(cost), sd(cost)), col = 'red', add = T)


# 실습: attach() / detach() 함수로 기술 통계량 구하기 
attach(data)
length(cost)
summary(cost)
mean(cost)
min(cost)
max(cost)
range(cost)
sd <- sd(cost, na.rm = T)
sqrt(var(cost, na.rm = T))
sd(cost, na.rm = T)
detach(data)



# 실습: NA가 있는 경우 제거한 뒤에 기술 통계량 구하기 
# 단계 1: NA가 있으면 error가 발생하는 함수
test <- c(1:5, NA, 10:20)
min(test)
max(test)
range(test)
mean(test)

# 단계 2: NA 제거 후 통계량 구하기 
min(test, na.rm = T)
max(test, na.rm = T)
range(test, na.rm = T)
mean(test, na.rm = T)



# 실습: 변수 리코딩과 빈도분석 하기 
# 단계 1: 거주지역(resident) 변수의 리코딩고 비율계산
data$resident2[data$resident == 1] <- "특별시"
data$resident2[data$resident >= 2 & data$resident <= 4] <- "광역시"
data$resident2[data$resident == 5] <- "시구군"

x <- table(data$resident2)
x


prop.table(x)

y <- prop.table(x)
round(y * 100, 2)


# 단계 2: 성별(gender) 변수의 리코딩과 비율계산
data$gender2[data$gender == 1] <- "남자"
data$gender2[data$gender == 2] <- "여자"
x <- table(data$gender2)
prop.table(x)
y <- prop.table(x)
round(y * 100, 2)


# 단계 3: 나이(age) 변수의 리코딩과 비율계산
data$age2[data$age <= 45] <- "중년층"
data$age2[data$age >= 46 & data$age <= 59] <- "장년층"
data$age2[data$age >= 60] <- "노년층"
x <- table(data$age2)
x

prop.table(x)
y <- prop.table(x)
round(y * 100, 2)


# 단계 4: 학력 수준(level) 번수의 리코딩과 비율계산
data$level2[data$level == 1] <- "고졸"
data$level2[data$level == 2] <- "대졸"
data$level2[data$level == 3] <- "대학원졸"
x <- table(data$level2)
x

prop.table(x)
y <- prop.table(x)
round(y * 100, 2)

# 단계 5: 합격여부(pass) 변수의 리코딩과 비율계산
data$pass2[data$pass == 1] <- "합격"
data$pass2[data$pass == 2] <- "실패"
x <- table(data$pass)

x
prop.table(x)

y <- prop.table(x)
round(y * 100, 2)

head(data)
