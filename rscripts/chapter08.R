# Chapter 08

# 실습: lattice 패키지 사용 준비하기 
# 단계 1: lattice 패키지 설치하기 
install.packages("lattice")
library(lattice)

# 단계 2: 실습용 데이터 가져오기 
install.packages("mlmRev")
library(mlmRev)
data("Chem97")
str(Chem97)
head(Chem97, 30)
Chem97


# 실습: histogram()  함수를 이용하여 데잍 시각화하기 
histogram(~gcsescore, data = Chem97)


# 실습: score 변수를 조건변수로 지정하여 데이터 시각화하기 
histogram(~gcsescore | score, data = Chem97)
histogram(~gcsescore | factor(score), data = Chem97)


# 실습: densityplot() 함수를 사용하여 밀도 그래프 그리기
densityplot(~gcsescore | factor(score), data = Chem97,
            groups = gender, plot.Points = T, 
            auto.ley = T)

# 실습: barchart() 함수를 사용하여 막대 그래프 그리기
# 단계 1: 기본 데이터 셋 가져오기 
data(VADeaths)
VADeaths

# 단계 2: vadEATHS 데이터 셋 구조보기 
str(VADeaths)
class(VADeaths)
mode(VADeaths)

# 단계 3: 데이터 형식 변경(matrix 형식을 table 형식으로 변경)
dft <- as.data.frame.table(VADeaths)
str(dft)
dft

# 단계 4: 막대 그래프 그리기 
barchart(Var1 ~ Freq | Var2, data = dft, layout = c(4, 1))


# 단계 5: origin 속성을 사용하여 막대 그래프 그리기 
barchart(Var1 ~ Freq | Var2, data = dft, layout = c(4, 1), origin = 0)


# 실습: dotplot() 함수를 사용하여 점 그래프 그리기 
# 단계 1: layout() 속성이 없는 경우
dotplot(Var1 ~ Freq | Var2, dft)

# 단계 2: layout() 속성을 적용한 경우 
dotplot(Var1 ~ Freq | Var2, dft, layout = c(4, 1))


# 실습: 점을 선으로 연결하여 시각화하기 
dotplot(Var1 ~ Freq, data = dft,
        groups = Var2, type = "o",
        auto.key = list(space = "right", points = T, lines = T))



# 실습: airquality 데이터 셋으로 산점도 그래프 그리기
# 단계 1: airquality 데이터 셋 가져오기 
library(datasets)
str(airquality)

# 단계 2: xyplot() 함수를 사용하여 산점도 그리기 
xyplot(Ozone ~ Wind, data = airquality)

# 단계 3: 조건변수를 사용하는 xyplot() 함수로 산점도 그리기 
xyplot(Ozone ~ Wind | Month, data = airquality)

# 단계 4: 조건변수와 layout 속성을 사용하는 xyplot() 함수로 산점도 그리기 
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))

# 단계 5: Month 변수를 factor 타입으로 변환하여 산점도 그리기 
#         패널 제목에는 factor 값을 표시
convert <- transform(airquality, Month = factor(Month))
str(convert)

xyplot(Ozone ~ Wind | Month, data = convert)
#xyplot(Ozone ~ Wind | Month, data = convert, layout = c(5, 1))




# 싨브: quakes 데이터 셋으로 산점도 그래프 그리기
# 단계 1: quakes 데이터 셋 보기 
head(quakes)
str(quakes)

# 단계 2: 지진 발생 진앙지(위도와 경도) 산점도 그리기 
xyplot(lat ~ long, data = quakes, pch = ".")

# 단계 3: 산점도 그래프를 변수에 저장하고, 
#         제목 문자열 추가하기 
tplot <- xyplot(lat ~ long, data = quakes, pch = ".")
tplot <- update(tplot, main = "1964년 이후 태평양에서 발생한 지진 위치")
print(tplot)


# 실습: 이산형 변수를 조건으로 지정하여 산점도 그리기
# 단계 1: depth 변수의 범위 확인
range(quakes$depth)

# 단계 2: depth 변수의 리코딩: 6개의 볌주(100 단위)로 코딩 변경
quakes$depth2[quakes$depth >= 40 & quakes$depth <= 150] <- 1
quakes$depth2[quakes$depth >= 151 & quakes$depth <= 250] <- 2
quakes$depth2[quakes$depth >= 251 & quakes$depth <= 350] <- 3
quakes$depth2[quakes$depth >= 351 & quakes$depth <= 450] <- 4
quakes$depth2[quakes$depth >= 451 & quakes$depth <= 550] <- 5
quakes$depth2[quakes$depth >= 551 & quakes$depth <= 680] <- 6

# 단계 3: 리코딩된 변수(depth2)를 조건으로 산점도 그리기 
convert <- transform(quakes, depth2 = factor(depth2))
xyplot(lat ~ long | depth2, data = convert)



# 실습: 동일한 패널에 두 개의 변수값 표현하기 
xyplot(Ozone + Solar.R ~ Wind | factor(Month), 
       data = airquality, 
       col = c("blue", "red"),
       layout = c(5, 1))



# 실습: equal.count() 함수를 사용하여 이산형 변수 범주화하기 
# 단계 1: 1 ~ 150을 대상으로 겹치지 않게 4개 영역으로 범주화
numgroup <- equal.count(1:150, number = 4, overlap = 0)
numgroup

# 단계 2: 지진의깊이를 5개 영역으로 범주화 
depthgroup <- equal.count(quakes$depth, number = 5, overlap = 0)
depthgroup

# 단계 3: 범주화된 변수(depthgroup)를 조건으로 산점도 그리기
xyplot(lat ~ long | depthgroup, data = quakes, 
       main = "Fiji Earthquakes(depthgroup)",
       ylab = "latitude", xlab = "longitude", 
       pch = "@", col = "red")


# 싨브: 수심과 리히터 규모 변수를 동시에 적용하여 산점도 그리기
# 단계 1: 리히터 규모를 2개 영역으로 구분
magnitudegroup <- equal.count(quakes$mag, number = 2, overlap =0)
magnitudegroup

# 단계 2: magnitudegroup 변수를 기준으로 산점도 그리기 
xyplot(lat ~ long | magnitudegroup, data = quakes, 
       main = "Fiji Earthquakes(magnitude)",
       ylab = "latitude", xlab = "longitude", 
       pch = "@", col = "blue")

# 단계 3: 수심과 리히터 규모를 동시에 표현(2행 5열 패널 구현)
xyplot(lat ~ long | depthgroup * magnitudegroup, data = quakes, 
       main = "Fiji Earthquakes", 
       ylab = "latitude", xlab = "longitude", 
       pch = "@", col = c("red", "blue"))


# 실습: 이산형 변수를 리코딩한 뒤에 factor 형으로 변환하여 산점도 그리기 
# 단계 1: depth 변수 리코딩
quakes$depth3[quakes$depth >= 39.5 & quakes$depth <= 80.5] <- 'd1'
quakes$depth3[quakes$depth >= 79.5 & quakes$depth <= 186.5] <- 'd2'
quakes$depth3[quakes$depth >= 185.5 & quakes$depth <= 397.5] <- 'd3'
quakes$depth3[quakes$depth >= 396.5 & quakes$depth <= 562.5] <- 'd4'
quakes$depth3[quakes$depth >= 562.5 & quakes$depth <= 680.5] <- 'd5'

# 단계 2: mag 변수 리코딩
quakes$mag3[quakes$mag >= 3.95 & quakes$mag <= 4.65] <- 'm1'
quakes$mag3[quakes$mag >= 4.55 & quakes$mag <= 6.65] <- 'm2'

# 단계 3: factor 형으로 변환
convert <- transform(quakes,
                     depth3 = factor(depth3),
                     mag3 = factor(mag3))

# 단계 4: 산점도 그래프 그리기 
xyplot(lat ~ long | depth3 * mag3, data = convert,
       main = "Fiji Earthquakes", 
       ylab = "latitude", xlab = "longitude",
       pch = "@", col = c("red", "blue"))


# 실습: depth 조건에 의해서 위도와 경도의 조건 그래프 그리기
coplot(lat ~ long | depth, data = quakes)


# 실습: 조건의 구간 크기와 겹침 간격 적용 후 조건 그래프 그리기 
# 단계 1: 조건의 구간 막대가 0.1 단위로 겹쳐 범주화 
coplot(lat ~ long | depth, dat = quakes,
       overlap = 0.1)

# 단계 2: 조건 구간을 5개로 지정하고, 1행 5열으 패널로 조건 그래프 작성
coplot(lat ~ long | depth, data = quakes, 
       number = 5, row = 1)



# 실습: 패널과 조건 막대에 색을 적용하여 조건 그래프 그리기 
# 단계 1: 패널 영역에 부드러운 곡선 추가 
coplot(lat ~ long | depth, data = quakes,
       number = 5, row = 1, 
       panel = panel.smooth)

# 단계 2: 패널 영역과 조건 막대에 색상 적용
coplot(lat ~ long | depth, data = quakes, 
       number = 5, row = 1, 
       col = 'blue',
       bar.bg = c(num = 'green'))


# 실습: 위도, 경도, 길이를 이용하여 3차원 산점도그리기 
cloud(depth ~ lat * long, data = quakes, 
      zlim = rev(range(quakes$depth)),
      xlab = "경도", ylab = "위도", zlab = "깊이")



# 실습: ggplot 패키지 설치와 실습 데이터 가져오기 
install.packages("ggplot2")
library(ggplot2)
data(mpg)
str(mpg)
head(mpg)
summary(mpg)

table(mpg$drv)



# 실습: qplot() 함수의 fill과 binwidth 속성 적용하기 
# 단계 1: 도수분포를 세로 막대 그래프로 표현
qplot(hwy, data = mpg)

# 단계 2: fill 속성 적용
qplot(hwy, data = mpg, fill = drv)

# 단계 3: binwidth 속성 적용
qplot(hwy, data = mpg, fill = drv, binwidth = 2)


# 실습: facets 속성을 사용하여 drv 변수값으로 행/열 단위로 패널 생성하기 
# 단계 1: 열 단위 패널 생성
qplot(hwy, data = mpg, fill = drv, facets = . ~ drv, binwidth = 2)

# 단계 2: 행 단위 패널 생성
qplot(hwy, data = mpg, fill = drv, facets = drv ~ ., binwidth = 2)


# 실습: qplot() 함수에서 color 속성을 사용하여 두 변수 구분하기 
# 단계 1:  두 변ㄴ수로 displ과 hwy 변수 사용
qplot(displ, hwy, data = mpg)

# 단계 2: 두 변수로 displ과 hwp 변수 사용하며, drv 변수에 색상 적용
qplot(displ, hwy, data = mpg, color = drv)


# 실습: displ과 hwy 변수의 관계를 drv 변수로 구분하기 
qplot(displ, hwy, data = mpg, color = drv, facets = . ~ drv)


# 실습: mtcars 데이터 셋에 색상, 크기, 모양 적용하기 
# 단계 1: 실습용 데이터 셋 확인하기 
head(mtcars)

# 단계 2: 색상 적용
qplot(wt, mpg, data = mtcars, color = factor(carb))

# 단계 3: 크기 적용
qplot(wt, mpg, dat = mtcars, 
      size = qsec, color = factor(carb))

# 단계 4: 모양 적용
qplot(wt, mpg, data = mtcars, 
      size = qsec, color = factor(carb), shape = factor(cyl))



# 실습: diamonds 데이터 셋에 막대, 점, 선 레이아웃 적용하기 
# 단계 1: 실습용 데이터 셋 확인하기 
head(diamonds)

# 단계 2: geom 속성과 fill 속성 사용하기 
qplot(clarity, data = diamonds, fill = cut, geom = "bar")

# 단계 3: 테두리 색 적용
qplot(clarity, data = diamonds, colour = cut, geom = "bar")

# 단계 4: geom = "point" 속성으로 산점도 그래프 그리기
qplot(wt, mpg, data = mtcars, size = qsec, geom = "point")

# 단계 5: 산점도 그래프에 cyl 변수의 요인으로 포인트 크기 적용하고, 
#         carb 변수의 요인으로 포인트 색 적용하기 
qplot(wt, mpg, data = mtcars, size = factor(cyl),
      color = factor(carb), geom = "point")

# 단계 6: 산점도 그래프에 qsec 변수의 요인으로 포인트 크기 적용하고, 
#         cyl 변수의 요인으로 포인트 모양 적용하기 
qplot(wt, mpg, data = mtcars, size = qsec, 
      color = factor(carb),
      shape = factor(cyl), geom = "point")

# 단계 7: geom = "smooth" 속성으로 산점도 그래프에 평활 그리기
qplot(wt, mpg, data = mtcars, 
      geom = c("point", "smooth"))

# 단계 8: 산점도 그래프의 평활에 cyl 변수의 요인으로 색상 적용하기 
qplot(wt, mpg, dat = mtcars, color = factor(cyl), 
      geom = c("point", "smooth"))

# 단계 9: geom = "line" 속성으로 그래프 그리기 
qplot(mpg, wt, data = mtcars, 
      color = factor(cyl), geom = "line")

# 단계 10: geom = c("point", "line") 속성으로 그래프 그리기 
qplot(mpg, wt, data = mtcars, 
      color = factor(cyl), geom = c("point", "line"))


# 실습: aes() 함수 속성을 추가하여 미적 요소 맵핑하기 
# 단계 1: diamonds 데이터 셋에 미적 요소 맵핑
p <- ggplot(diamonds, aes(carat, price, color = cut))
p + geom_point()

# 단계 2: mtcars 데이터 셋에 미적 ㅓ요소 맵핑 
p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl)))
p + geom_point()


# 실습: geom_line()과 geom_point() 함수를 적용하여 레이어 추가하기 
# 단계 1: geom_line() 레이어 추가 
p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl)))
p + geom_line()

# 단계 2: geom_point() 레이어 추가 
p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl)))
p + geom_point()



# 실습: stat_bin() 함수를 사용하여 막대 그래프 그리기 
# 단계 1: 기본 미적 요소 맵핑 객체를 생성한 뒤에 stat_bin() 함수 사용
p <- ggplot(diamonds, aes(price))
p + stat_bin(aes(fill = cut), geom = "bar")

# 단계 2: price 빈도를 밀도(전체의 합 = 1)로 스케일링하여 stat_bin() 함수 사용
p + stat_bin(aes(fill = ..density..), geom = "bar")


# 실습: stat_bin() 함수 적용 영역과 산점도 그래프 그리기
# 단계 1: stat_bin() 함수 적용 영역 나타내기 
p <- ggplot(diamonds, aes(price))
p + stat_bin(aes(fill = cut), geom = "area")

# 단계 2: stat_bin() 함수로 산점도 그래프 그리기 
p + stat_bin(aes(color = cut, 
                 size = ..density..), geom = "point")


# 실습: 산점도에 회귀선 적용하기 
library(UsingR)
data("galton")
p <- ggplot(data = galton, aes(x = parent, y = child))
p + geom_count() + geom_smooth(method ="lm")


# 실습: 테마를 적용하여 그래프 외형 속성 설정하기 
# 단계 1: 제목을 설정한 산점도 그래프 
p <- ggplot(diamonds, aes(carat, price, color = cut))
p <- p + geom_point() + ggtitle("다이아몬드 무게와 가격의 상관관계")
print(p)

# 단계 2: theme() 함수를이용하여 그래프의 외형 속성 설저하기 
p + theme(
  title = element_text(color = "blue", size = 25), 
  axis.title = element_text(size = 14, face ="bold"),
  axis.title.x = element_text(color = "green"),
  axis.title.y = element_text(color = "green"),
  axis.text = element_text(size = 14),
  axis.text.y = element_text(color = "red"),
  axis.text.x = element_text(color = "purple"),
  legend.title = element_text(size = 20,
                              face = "bold",
                              color = "red"),
  legend.position = "bottom",
  legend.direction = "horizontal"
)


# 실습: 그래프를 이미지 파일로 저장하기 
# 단계 1: 저장할 그래프 그리기 
p <- ggplot(diamonds, aes(carat, price, color = cut))
p + geom_point()

# 단계 2: 가장 최근에 그려진 그래프 저장
ggsave(file = "C:/Rwork/output/diamond_price.pdf")
ggsave(file = "C:/Rwork/output.diamond_price.jpg", dp = 72)


# 실습: 벼수에 저장된 그래프를 이미지 파일로 저장하기 
p <- ggplot(diamonds, aes(clarity))
p <- p + geom_bar(aes(fill = cut), position = "fill")
ggsave(file = "C:/Rwork/output/bar.png",
       plot = p, width = 10, height = 5)




# 실습: 지도 관련 패키지 설치하기 
library(ggplot2)
install.packages("ggmap")
library(ggmap)


# 실습: 서울을 중심으로 지도 시각화하기
# 단계 1: 서울 지역의 중심 좌표 설정
seoul <- c(left = 126.77, bottom = 37.40, 
           right = 127.17, top = 37.70)

# 단계 2: zoom, maptype으로 정적 지도 이미지 가져오기 
map <- get_stamenmap(seoul, zoom = 12, maptype = 'terrain')
ggmap(map)


# 실습 : 2019년도 1월 대한민국 인구수를 기준으로 지역별 인구수 표시하기 
# 단계 1: 데이터 셋 가져오기 
pop <- read.csv(file.choose(), header = T)

library(stringr)

region <- pop$'지역명'
lon <- pop$LON
lat <- pop$LAT
tot_pop <- as.numeric(str_replace_all(pop$'총인구수', ',', ''))

df <- data.frame(region, lon, lat, tot_pop)
df
df <- df[1:17, ]
df

# 단게 2: 정적 지도 이미지 가져오기 
daegu <- c(left = 123.4423013, bottom = 32.8528306,
           right = 131.601445, top = 38.8714354)
map <- get_stamenmap(daegu, zoom = 7, maptype = 'watercolor')

# 단계 3: 지도 시각화하기 
layer1 <- ggmap(map)
layer1

# 단계 4: 포인트 추가 
layer2 <- layer1 + geom_point(data = df, 
                              aes(x = lon, y = lat,
                                  color = factor(tot_pop), 
                                  size = factor(tot_pop)))
layer2

# 단계 5: 텍스트 추가 
layer3 <- layer2 + geom_text(data = df, 
                             aes(x = lon + 0.01, y = lat + 0.08,
                                 label = region), size = 3)
layer3

# 단계 6: 크기를 지정하여 파일로 저장하기 
ggsave("pop201901.png", scale = 1, width = 10.24, height = 7.68)
