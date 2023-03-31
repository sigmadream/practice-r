# Chapter 13

# 실습: 우리나라 중학교 2학년 남학생의 평균 신장 표본조사 
# 신뢰수준 95%의 신뢰구간 구하기 
N = 10000
X = 165.1
S = 2
low <- X -1.96 * S / sqrt(N)
high <- X + 1.96 # S / sqrt(N)
low; high

# 실습: 신뢰구간으로 표본오차 구하기
high - X
(low - X) * 100
(high - X) * 100


# 실습: 단일 표본 빈도수와 비율계산
# 단계 1: 실습 데이터 가져오기 
setwd("C:/Rwork/Part-III")
data <- read.csv("one_sample.csv", header = TRUE)
head(data)

x <- data$survey

# 단계 2: 빈도수와 비율계산
summary(x)
length(x)
table(x)

# 단계 3: 패키지를 이용한 빈도수와 비율계산
install.packages("prettyR")
library(prettyR)
freq(x)



# 실습: 불만율 기준 비율검정
# 단계 1: 양측 검정
binom.test(14, 150, p = 0.2)
binom.test(14, 150, p = 0.2, alternative = "two.sided", conf.level = 0.95)

# 단계 2: 방향성을 갖는 단측 가설검정
binom.test(c(14, 150), p = 0.2, 
           alternative = "greater", conf.level = 0.95)

binom.test(c(14, 150), p = 0.2, 
           alternative = "less", conf.level = 0.95)



# 실습: 단일 표본 평균 계산하기 
# 단계 1: 실습 파일 가져오기 
 setwd("C:/Rwork/Part-III")
data <- read.csv("one_sample.csv", header = TRUE) 
str(data)
head(data)

x <- data$time
head(x)

# 단계 2: 데이터 분포/결측치 제거 
summary(x)
mean(x)

# 단계 3: 데이터 정제
mean(x, na.rm = T)
x1 <- na.omit(x)
mean(x1)



# 실습: 정규분포 검정
shapiro.test(x1)



# 실습: 정규분포 시각화
par(mfrow = c(1, 2))
hist(x1)


# 실습: 단일 표본 평균 차이 검정
# 단계 1: 양측검정 - x1 객체와기존 모집단의 평균 5.2시간 비교
t.test(x1, mu = 5.2)
qqnorm(x1)
qqline(x1, lty = 1, col = "blue")
t.test(x1, mu = 5.2, alter = "two.side", conf.level = 0.95)

# 단계 2: 방향성을 갖는 단축 가설검정
t.test(x1, mu = 5.2, alter= "greater", conf.level = 0.95)

# 단계 3: 귀무가설의 임계값 계산
qt(7.08e-05, 108)


# 실습: 두 집단의 subset 작성과 교차분석 수행
# 단계 1: 실습 파일 가져오기 
setwd("C:/Rwork/Part-III")
data <- read.csv("two_sample.csv", header = TRUE)
head(data)

#단계 2: 두 집단의 sub 작성 및 데이터 전처리 
x <- data$method
y <- data$survey

# 단계 3: 집단별 빈도분석
table(x)
table(y)

# 단계 4: 두 변수에 대한 교차분석
table(x, y, useNA = "ifany")


# 실습: 두 집단 비율 차이 검정
# 단계 1: 양측검정
prop.test(c(110, 135), c(150, 150),
          alternative = "two.sided", conf.level = 0.95)


# 단계 2: 방향성을 갖는 단측가설 검정
prop.test(c(110, 135), c(150, 150),
          alter = "greater", conf.level = 0.95)
prop.test(c(110, 135), c(150, 150), 
          alter = "less", conf.level = 0.95)



# 실습: 독립표본 평균 계산
# 단계 1: 실습 파일 가져오기 
data <- read.csv("C:/Rwork/Part-III/two_sample.csv", header = TRUE)
head(data)
summary(data)

# 단계 2: 두 집단의 subset 작성 및 데이터 전처리 
result <- subset(data, !is.na(score), c(method, score))

# 단계 3: 데이터 분리 
a <- subset(result, method == 1)
b <- subset(result, method == 2)
a1 <- a$score
b1 <- b$score 

# 단계 4: 기술 통계량
length(a1)
length(b1)
mean(a1)
mean(b1)


# 실습: 두 집단 간의 동질성 검정
var.test(a1, b1)


# 두 집단의 평균 차이 검정
# 단계 1: 양측검정
t.test(a1, b1, altr = "tow.sided", 
       conf.int = TRUE, conf.level = 0.95)

# 단계 2: 방향성을 갖는 단측가설 검정
t.test(a1, b1, alter = "greater", 
       conf.int = TRUE, conf.level = 0.95)
t.test(a1, b1, alter = "less", 
       conf.int = TRUE, conf.level = 0.95)



# 실습: 대응표본 평균 계산
#단계 1: 실습 파일 가져오기 
setwd("C:/Rwork/Part-III")
data <- read.csv("paired_sample.csv", header = TRUE)

# 단계 2: 대응 두 집단 subset 생성
result <- subset(data, !is.na(after), c(before, after))
x <- result$before
y <- result$after
x; y

# 단계 3: 기술 통계량 계산
length(x)
length(y)
mean(x)
mean(y)


# 실습: 대응표본의 동질성 검정
var.test(x, y, pared = TRUE)


# 실습: 대응 두 집단 평균 차이 검정
# 단계 1: 양측검정
t.test(x, y, paired = TRUE,
       alter = "two.sided",
       conf.int = TRUE, conf.level = 0.95)

# 단계 2: 방향성을 갖는 단측 가설검정
t.test(x, y, paired = TRUE, 
       alter = "greater",
       conf.int = TRUE, conf.level = 0.95)

t.test(x, y, paired = TRUE, 
       alter = "less",
       conf.int = TRUE, conf.level = 0.95)



# 실습: 세 집단 subset 작성과 기술 통계량 계산
# 단계 1: 파일 가져오기 
setwd("C:/Rwork/Part-III")
data <- read.csv("three_sample.csv", header = TRUE)
head(data)

# 단계 2: 세 집단 subset 작성(데이터 전처리)
method <- data$method
survey <- data$survey
method; survey

# 단계 3: 기술 통계량(빈도수)
table(method, useNA = "ifany")
table(method, survey, useNA = "ifany")



# 실습: 세 집단 비율 차이 검정
prop.test(c(34, 37, 39),
          c(50, 50, 50))



# 실습: 데이터 전처리 수행
# 단계 1: 실습 파일 가져오기 
data <- read.csv("C:/Rwork/Part-III/three_sample.csv")
head(data)

# 단계 2: 데이터 전처리 - NA, outlier 제거 
data <- subset(data, !is.na(score), c(method, score))
head(data)

# 단계 3: 차트 이용 outlier 보기(데이터 분포 현황 분석)
par(mfrow = c(1, 2))
plot(data$score)
barplot(data$score)
mean(data$score)

#단계 4: 데이터 정제(outlier 제거: 평균(14) 이상 제거)
length(data$score)
data2 <- subset(data, score <= 14)
length(data2$score)

# 단계 5: 정제된 데이터 확인
x <- data2$score
par(mfrow = c(1, 1))
boxplot(x)



# 실습: 세 집단 subset 작성고 기술 통계량 구하기 
# 단계 1: 세 집단 subset 작성
data2$method2[data2$method == 1] <- "방법1"
data2$method2[data2$method == 2] <- "방법2"
data2$method2[data2$method == 3] <- "방법3"


# 단계 2: 교육 방법별 빈도수
table(data2$method2)

# 단계 3: 교육 방법을 x 변수에 저장
x <- table(data2$method2)
x

# 단계 4: 교육 방법에 따른 시험성적 평균 구하기 
y <- tapply(data2$score, data2$method2, mean)
y

# 단계 5: 교육 방법과 시험성적으로 데이터프레임 생성
df <- data.frame(교육방법 = x, 시험성적 = y)
df



# 실습: 세 집단 간 동질성 검정 수행
bartlett.test(score ~ method, data = data2)



# 실습: 분산분석 수행
help(aov)
result <- aov(score ~ method2, data = data2)
names(result)

summary(result)



# 실습: 사후검정 수행
# 단계 1: 분산분석 결과에 대한 사후검정
TukeyHSD(result)

# 단계 2: 사후검정 시각화 
plot(TukeyHSD(result))
