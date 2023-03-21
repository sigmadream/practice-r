# Chapter 14

# 실습: 변수와 데이터프레임 생성
# 단계 1: 과목 변수 생성

s1 <- c(1, 2, 1, 2, 3, 4, 2, 3, 4, 5)
s2 <- c(1, 3, 1, 2, 3, 4, 2, 4, 3, 4)
s3 <- c(2, 3, 2, 3, 2, 3, 5, 3, 4, 2)
s4 <- c(2, 4, 2, 3, 2, 3, 5, 3, 4, 1)
s5 <- c(4, 5, 4, 5, 2, 1, 5, 2, 4, 3)
s6 <- c(4, 3, 4, 4, 2, 1, 5, 2, 4, 2)
name <- 1:10

# 단계 2: 과목 데이터프레임 생성
subject <- data.frame(s1, s2, s3, s4, s5, s6)
str(subject)



# 실습: 변수의 주성분 분석
# 단계 1: 주성분 부석으로 요인 수 알아보기 
pc <- prcomp(subject)
summary(pc)
plot(pc)

# 단계 2: 고유값으로 요인 수 분석
en <- eigen(cor(subject))
names(en)

en$values
en$vectors

plot(en$values, type = "o")



# 실습: 변수 간의 상관관계 분석과 요인분석
# 단계 1: 상관관계 분석 - 벼수 간의 상관성으로 공통요인 추출
cor(subject)

# 단계 2: 요인분석 - 요인회전법 적용(Varimax 회전법이 기본)
# 단계 2-1: 주성분 분석의 가정에 의해서 2개 요인으로 분석
result <- factanal(subject, factors = 2, rotation = "varimax")
result 

# 단계 2: 요인분석 - 요인회전법 적용(Varimax 회전법이 기본)
# 단계 2-2: 고유값으로 가정한 3개 요인으로 분석
result <- factanal(subject, 
                   factor = 3, 
                   rotation = "varimax", 
                   scores = "regression")
result


# 단계 3: 다양한 방법으로 요인적재량 보기 
attributes(result)

result$loadings

print(result, digits = 2, cutoff = 0.5)

print(result$loadings, cutoff = 0)


# 실습: 요인점수를 이용한 요인적재량 시각화 
# 단계 1: Factor1과 Factor2 요인적재량 시각화 
plot(result$scores[ , c(1:2)],
     main = "Factor1과 Factor2 요인점수 행렬")

text(result$scores[ , 1], result$scores[ , 2],
     labels = name, cex = 0.7, pos = 3, col = "blue")

# 단게 2: 요인적재량 추가
points(result$loadings[ , c(1:2)], pch = 19, col = "red")

text(result$loadings[ , 1], result$loadings[ , 2],
     labels = rownames(result$loadings),
     cex = 0.8, pos = 3, col = "red")

# 단계 3: Factor1과 Factor3 요인적재량 시각화
plot(result$scores[ , c(1, 3)],
     main = "Factor1과 Factor3 요인점수 행렬")
text(result$scores[ , 1], result$scores[ , 3],
     labels = name, cex = 0.7, pos = 3, col = "blue")

# 단계 4: 요인적재량 추가 
points(result$loadings[ , c(1, 3)], pch = 19, col = "red")
text(result$loadings[ , 1], result$loadings[ , 3],
     labels = rownames(result$loadings), 
     cex = 0.8, pos= 3, col = "red")



# 실습: 3차원 산점도로 요인적재량 시각화 
# 단계 1: 3차원 산점도 패키지 로딩
library(scatterplot3d)

# 단계 2: 요인점수별 분류 및 3차원 프레임 생성
Factor1 <- result$scores[ , 1]
Factor2 <- result$scores[ , 2]
Factor3 <- result$scores[ , 3]

d3 <- scatterplot3d(Factor1, Factor2, Factor3, type = 'p')

# 단계 3: 요인적재량 표시
loadings1 <- result$loadings[ , 1]
loadings2 <- result$loadings[ , 2]
loadings3 <- result$loadings[ , 3]
d3$points3d(loadings1, loadings2, loadings3, 
            bg = 'red', pch = 21, cex = 2, type = 'h')



# 실습: 요인별 변수 묶기
# 단계 1: 요인별 과목 변수 이용 데이터프레임 생성
app <- data.frame(subject$s5, subject$s6)
soc <- data.frame(subject$s3, subject$s4)
nat <- data.frame(subject$s1, subject$s2)

# 단계 2: 요인별 산술평균 계산
app_science <- round((app$subject.s5 + app$subject.s6) / ncol(app), 2)
soc_science <- round((soc$subject.s3 + soc$subject.s4) / ncol(soc), 2)
nat_science <- round((nat$subject.s1 + nat$subject.s2) / ncol(net), 2)

# 단계 3: 상관관계 분석
subject_factor_df <- data.frame(app_science, soc_science, nat_science)
cor(subject_factor_df)




# 실습: 요인분석에 사용될 데이터 셋 가져오기 
# 단계 1: spss 데이터 셋 가져오기 
install.packages("memisc")
library(memisc)
setwd("C:/Rwork/Part-III")
data.spss <- as.data.set(spss.system.file('drinking_water.sav'))
data.spss[1:11]

# 단계 2: 데이터프레임으로 변경
drinking_water <- data.spss[1:11]
drinking_water_df <- as.data.frame(data.spss[1:11])
str(drinking_water_df)


# 실습: 요인 수를 3개로 지정하여 요인분석 수행
result2 <- factanal(drinking_water_df, factor = 3, rotation = "varimax")
result2


# 실습: 요인별 변수 묶기
# 단계 1: q1를 제외하고 데이터프레임 생성
dw_df <- drinking_water_df[-4]
str(dw_df)
dim(dw_df)

# 단계 2: 요인에 속하는 입력 변수별 데이터프레임 구성
s <- data.frame(dw_df$Q8, dw_df$Q9, dw_df$Q10, dw_df$Q11)
c <- data.frame(dw_df$Q1, dw_df$Q2, dw_df$Q3)
p <- data.frame(dw_df$Q5, dw_df$Q6, dw_df$Q7)

# 단계 3: 요인별 산술평균 계산
satisfaction <- round(
  (s$dw_df.Q8 + s$dw_df.Q9 + s$dw_df.Q10 + s$dw_df.Q11) / ncol(s), 2)
closeness <- round(
  (c$dw_df.Q1 + c$dw_df.Q2 + c$dw_df.Q3) / ncol(s), 2)
pertinence <- round(
  (p$dw_df.Q5 + p$dw_df.Q6 + p$dw_df.Q7) / ncol(s), 2)

# 단계 4: 상관관계 분석
drinking_water_factor_df <- data.frame(satisfaction, closeness, pertinence)
colnames(drinking_water_factor_df) <- c("제품만족도", "제품친밀도", "제품적절성")
cor(drinking_water_factor_df)

length(satisfaction); length(closeness); length(pertinence)




# 실습: 기술 통계량 구하기 
# 단계 1: 데잍 가져오기 
product <- read.csv("C:/Rwork/Part-III/product.csv", header = TRUE)
head(product)

# 단계 2: 기술 통계량
summary(product)
sd(product$제품_친밀도); sd(product$제품_적절성); sd(product$제품_만족도)


# 실습: 상관계수 보기 
# 단계 1: 변수 간의 상관계수 보기 
cor(product$제품_친밀도, product$제품_적절성)
cor(product$제품_친밀도, product$제품_만족도)

# 단계 2: 제품_적절성과 제품_만족도의 상관계수 보기 
cor(product$제품_적절성, product$제품_만족도)

# 단계 3: (제품_적절성 + 제품_친밀도)와 제품_만족도의 상관계수 보기 
cor(product$제품_적절성 + product$제품_친밀도, product$제품_만족도)



# 실습: 전체 벼수 간의 상관계수 보기 
cor(product, method = "pearson")


# 실습: 방향성 있는 색상으로 표현
install.packages("corrgram")
library(corrgram)
corrgram(product)
corrgram(product, upper.panel = panel.conf)
corrgram(product, lower.panel = panel.conf)


# 실습: 차트에 밀도곡선, 상관성, 유의확률(별표) 추가하기 
# 단계 1: 패키지 설치 
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# 단게 2: 상관성, p값(*), 정규분포(모수 검정 조건) 시각화 
chart.Correlation(product, histogram = , pch = "+")



# 실습: 서열척도 대상 상관계수
cor(product, method = "spearman")
