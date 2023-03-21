# Chapter 05

# 실습: 세로 막대 차트 그리기
# 단계 1: 차트 작성을 위한 자료 만들기 
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names(chart_data) <- c("2018 1분기", "2019 1분기",
                       "2018 2분기", "2019 2분기", 
                       "2018 3분기", "2019 3분기", 
                       "2018 4분기", "2019 4분기")
str(chart_data)
chart_data

# 단계 2: 세로 막대 차트 그리기
barplot(chart_data, ylim = c(0, 600),
        col = rainbow(8),
        main = "2018년도 vs 2019년도 매출현항 비교")

# 실습: barplot() 함수 도움말 보기 
help("barplot")

# 실습: 막대 차트의 가로축과 세로축에 레이블 추가하기 
barplot(chart_data, ylim = c(0, 600),
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황",
        col = rainbow(8),
        main= "2018년도 vs 2019년도 매출현황 비교")

# 실습: 가로 막대 차트 그리기
barplot(chart_data, xlim = c(0, 600), horiz = T, 
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황", 
        col = rainbow(8), 
        main = "2018년도 vs 2019년도 매출현항 비교")

# 실습: 막대 차트에서 막대 사이의 간격 조정하기 
barplot(chart_data, xlim = c(0, 600), horiz = T, 
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황", 
        col = rainbow(8), space = 1, cex.names = 0.8,
        main = "2018년도 vs 2019년도 매출현항 비교")

# 실습: 막대 차트에서 막대의 색상 지정하기
barplot(chart_data, xlim = c(0, 600), horiz = T, 
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황", 
        space = 1, cex.names = 0.8,
        main = "2018년도 vs 2019년도 매출현항 비교",
        col = rep(c(2, 4), 4))

# 실습: 막대 차트에서 색상 이름을 사용하여 막대의 색상 지정하기 
barplot(chart_data, xlim = c(0, 600), horiz = T, 
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황", 
        space = 1, cex.names = 0.8,
        main = "2018년도 vs 2019년도 매출현항 비교",
        col = rep(c("red", "green"), 4))


# 실습: 누적 막대 차트 그리기 
# 단계 1: 메모리로 데이터 가져오기 
data("VADeaths")
VADeaths

# 단계 2: VADeaths 데이터 셋 구조 보기 
str(VADeaths)
class(VADeaths)
mode(VADeaths)

# 단계 3: 개별 차트와 누적 차트 그리기 
par(mfrow = c(1, 2))
barplot(VADeaths, beside = T, col = rainbow(5),
        main = "미국 버지니아주 하위계층 사망비율")
legend(19, 71, c("50-54", "55-59", "60-64", "65-69", "70-74"),
       cex = 0.8, fill = rainbow(5))

barplot(VADeaths, beside = F, col = rainbow(5))
title(main = "미국 버지니아주 하위계층 사망비율", font.main = 4)
legend(3.8, 200, c("50-54", "55-59", "60-64", "65-69", "70-74"),
       cex = 0.8, fill = rainbow(5))


# 실습: 점 차트(dotchart) 도움말 보기 
help(dotchart)

# 실습: 점 차트 사용하기 
par(mfrow = c(1, 1))
dotchart(chart_data, color = c("blue", "red"),
         lcolor = "black", pch = 1:2,
         labels = names(chart_data),
         xlab = "매출액", 
         main = "분기별 판매현황: 점차트 시각화",
         cex = 1.2)

# 실습: 원형 차트 도움말 보기 
help(pie)


#실습: 분기별 매출현황을 파이 차트로 시각화하기 
par(mfrow = c(1, 1))
pie(chart_data, labels = names(chart_data), col = rainbow(8), cex = 1.2)
title("2018~2019년도 분기별 매출현황")


# 실습: VADeaths 데이터 셋을 상자 그래프로 시각화하기
# 단계 1: "notch = FALSE"일 때
boxplot(VADeaths, range = 0)

# 단계 2: "notch = TRUE"일 때
boxplot(VADeaths, range = 0, notch = T)
abline(h = 37, lty = 3, col = "red")

# 실습: VADeaths 데이터 셋의 요약 통계량 보기 
summary(VADeaths)


# 실습: iris 데이터 셋 가져오기 
data(iris)
names(iris)
str(iris)
head(iris)


# 실습: iris 데이터 셋의 꽃받침 길이(Sepal.Length) 칼럼으로 히스토그램 시각화하기 
summary(iris$Sepal.Length)
hist(iris$Sepal.Length, xlab = "iris$Sepal.Length", col = "magenta", 
     main = "iris 꽃 받침 길이 Histogram", xlim = c(4.3, 7.9))

# 실습: iris 데이터 셋의 꽃받침 너비(Sepal.Width) 칼럼으로 히스토그램 시각화하기 
summary(iris$Sepal.Width)
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose", 
     main = "iris 꽃받침 너비 Histogram", xlim = c(2.0, 4.5))


# 실습: 히스토그램에서 빈도와 밀도 표현하기 
# 단계 1: 빈도수에 의해서 히스토그램 그리기
par(mfrow = c(1, 2))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", 
     col = "green", 
     main = "iris 꽃받침 너비 Histogram: 빈도수", xlim = c(2.0, 4.5))

# 단계 2: 확률 밀도에 의해서 히스토그램 그리기 
hist(iris$Sepal.Width, xlab = "iris.$Sepal.Width", 
     col = "mistyrose", freq = F, 
     main = "iris 꽃받침 너비 Histogram: 확률 밀도", xlim = c(2.0, 4.5))
lines(density(iris$Sepal.Width), col = "red")


# 실습: 정규분포 추정 곡선 나타내기 
# 단계 1: 계급을 밀도로 표현한 히스토그램 시각화 
par(mfrow = c(1, 1))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose", 
     freq = F, main = "iris 꽃받침 너비 Histogram", xlim = c(2.0, 4.5))

# 단계 2: 히스토그램에 밀도를 기준으로 분포곡선 추가 
lines(density(iris$Sepal.Width), col = "red")

# 단계 3: 히스토그램에 정규분포 추정 곡선 추가 
x <- seq(2.0, 4.5, 0.1)
curve(dnorm(x, mean = mean(iris$Sepal.Width),
             sd = sd(iris$Sepal.Width)),
       col = "blue", add = T)


# 실습: 산점도 그래프에 대각선과 텍스트 추가하기 
# 단계 1: 기본 산점도 시각화 
price <- runif(10, min = 1, max = 100)
plot(price, col = "red")

# 단계 2: 대각선 추가 
par(new = T)
line_chart = 1:100
plot(line_chart, type = "l", col = "red", axes = F, ann = F)

# 단계 3: 텍스트 추가 
text(70, 80, "대각선 추가", col = "blue")

# 실습: type 속성으로 산점도 그리기 
par(mfrow = c(2, 2))
plot(price, type = "l")
plot(price, type = "o")
plot(price, type = "h")
plot(price, type = "s")


# 실습: pch 속성으로 산점도 그리기 
par(mfrow = c(2, 2))
plot(price, type = "o", pch = 5)
plot(price, type = "o", pch = 15)
plot(price, type = "o", pch = 20, col = "blue")
plot(price, type = "o", pch = 20, col = "orange", cex = 1.5)
plot(price, type = "o", pch = 20, col = "green", cex = 2.0, lwd = 3)

# plot() 함수의 시각화 도구 목록 보기
methods("plot")


# plot() 함수에서 시계열 객체 사용하여 추세선 그리기 
data("WWWusage")
str(WWWusage)
plot(WWWusage)     # plot.ts(WWWusage)와 같다.

# 실습: 중복된 자료의 수 만큼 점의 크기 확대하기 
# 단계 1: 두 개의 벡터 객체 준비
x <- c(1, 2, 3, 4, 2, 4)
y <- rep( 2, 6)
x; y

# 단계 2: 교차 테이블 작성
table(x, y)

# 단계 3: 산점도 시각화 
plot(x, y)

# 단계 4: 교차 테이블로 데이터프레임 생성
xy.df <- as.data.frame(table(x, y))
xy.df

# 단계 5: 좌표에 중복딘 수 만큼 점을 확대하기 
plot(x, y, 
     pch = "@", col = "blue", cex = 0.5 * xy.df$Freq,
     xlab = "x 벡터의 원소", ylab = "y 벡터 원소")


# 실습: galton 데이터 셋을 대상으로 중복된 자료 시각화하기 
# 단계 1: galton 데이터셋 가져오기 
library(UsingR)
data(galton)

# 단계 2: 교차테이블을 작성하고, 데이프레임으로 변환
galtonData <- as.data.frame(table(galton$child, galton$parent))
head(galtonData)

# 단계 3: 칼럼 단위 추출
names(galtonData) = c("child", "parent", "freq")
head(galtonData)
parent <- as.numeric(galtonData$parent)
child <- as.numeric(galtonData$child)

# 단계 4: 점의 크기 확대 
par(mfrow = c(1, 1))
plot(parent, child, 
     pch = 21, col = "blue", bg = "green", 
     cex = 0.2 * galtonData$freq, 
     xlab = "parent", ylab = "child")

# 실습: iris 데이터 셋의 4개 변ㄴ수를 상호 비교
attributes(iris)
pairs(iris[iris$Species == "virginica", 1:4])
pairs(iris[iris$Species == "setosa", 1:4])


# 실습: 3차원으로 산점도 시각화하기 
# 단계 1: 3차원 산점도를 위한 scatterplot3d 패키지 설치 및 로딩
install.packages("scatterplot3d")
library(scatterplot3d)

# 단계 2: 꽃의 종류별 분류
iris_setosa = iris[iris$Species == 'setosa', ]
iris_versicolor = iris[iris$Species == 'versicolor', ]
iris_virginica = iris[iris$Species == 'virginica', ]

# 단계 3: 3차원 틀 생성하기 
d3 <- scatterplot3d(iris$Petal.Length, 
                    iris$Sepal.Length,
                    iris$Sepal.Width, 
                    type = 'n')

# 단계 4: 3차원 산점도 시각화
d3$points3d(iris_setosa$Petal.Length,
            iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width, 
            bg = 'orange', pch = 21)

d3$points3d(iris_versicolor$Petal.Length, 
            iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width,
            bg = 'blue', pch = 23)

d3$points3d(iris_virginica$Petal.Length, 
            iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width, 
            bg = 'green', pch = 25)

