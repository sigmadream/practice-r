# Chapter 01

# 실습: R 패키지 보기 
dim(available.packages())

# 실습: R ㅍ키지 목록 보기 
available.packages()

# 실습: R Session 보기 
sessionInfo()

# 실습: stringr 패키지 설치 
install.packages(("stringr"))

# 실습: 설치된 패키지 확인
installed.packages()

# 실습: 패키지 로드 
library(stringr)
search()

library(blm)
require(blm)

# 실습: 패키지 제거
remove.packages("stringr")

# 실습: 기본 데이터 셋 보기
data()

# 실습: 기본 데이터 셋으로 히스토그램 그리기
# 단계 1: 빈도수를 기준으로 히슽로그램 그리기
hist(Nile)
# 단계 2: 밀도를 기준으로 히스토그램 그리기
hist(Nile, freq = F)
# 단계 3: 단계 2의 결과에 분포 곡선을 추가 
lines(density(Nile))

# 실습: 히스토그램을 파일로 저장하기 
par(mfrow = c(1, 1))
pdf("C:/Rwork/batch.pdf")
hist(rnorm(20))
dev.off()

# 실습: 변수 사용 예
var1 <- 0
var1
var1 <- 1
var1
var2 <- 2
var2
var3 <- 3
var3

# 실습: '변수.멤버' 형식의 변수 선언
goods.code <= 'a001'
goods.name <- '냉장고'
goods.price <- 850000
goods.des <- '최고 사양, 동급 최고 품질'


# 실습: 벡터 변수 사용 예
age <- 35
names <- c("홍길동", "이순신", "유관순")
age
names

# 스칼라 변수 사용 예
int <- 20
int
string <- "홍길동"
string
boolean <- TRUE
boolean
sum(10, 20, 20)
sum(10, 20, 20, NA)
sum(10, 20, 20, NA, na.rm = TRUE)
ls()

# 실습: 자료형 확인
is.character(string)

x <- is.numeric(int)
x

is.logical(boolean)
is.logical(x)
is.na(x)

# 실습: 문자 원소를 숫자 원소로 형 변환하기 
x <- c(1, 2, "3")
x

result <- x * 3
result <- as.numeric(x) * 3
#result <- as.integer(x) * 3
result

# 실습: 복소수 자료 생성과 형 변환
z <- 5.3 - 3i
Re(z)
Im(Z)
is.complex(x)
as.complex(5.3)


# 실습: 스칼라 변수의 자료형과 자료구조 확인
mode(int)
mode(string)
mode(boolean)

class(int)
class(string)
class(boolean)

# 실습: 문자 벡터와 그래프 생성
gender <- c("man", "woman", "woman", "man", "man")
plot(gender)

# 실습: as.factor() 함수를 이용하여 요인형 변환
Ngender <- as.factor(gender)
table(Ngender)

# 실습: Factor 형 변수로 차트 그리기 
plot(Ngender)
mode(Ngender)
class(Ngender)
is.factor(Ngender)


# 실습: Factor Nominal 변수 내용 보기 
Ngender


# 실습: factor() 함수를 이용하여 Factor 형 변환
args(factor)
Ogender <- factor(gender, levels = c("woman", "man"), ordered = T)
Ogender

# 순서가 없는 요인과 순서가 있는 요인형 변수로 차트 그리기 
par(mfrow = c(1, 2))
plot(Ngender)
plot(Ogender)


# 실습: as.Date() 함수를 이용한 날짜형 변환
as.Date("20/02/28", "%y/%m/%d")
class(as.Date("20/02/28", "%y/%m/%d"))
dates <- c("02/28/20", "02/30/20", "03/01/20")
as.Date(dates, "%m%d%y")


# 실습: 시스템 로케일 정보 확인
Sys.getlocale(category = "LC_ALL")
Sys.getlocale(category = "LC_COLLATE")


# 실습: 현재 날짜와 시간 확인
Sys.time()


# 실습: strptime() 함수를 이용한 날짜형 변환
sdate <- "2019-11-11 12:47:5"
class(sdate)

today <- strptime(sdate, format = "%Y-%m-%d %H:%M:%S")
class(today)

# 실습: 4자리 연도와 2자리 연도 표기의 예
strptime("30-11-2019", format = ("%d-%m-%Y"))
strptime("30-11-19", format = ("%d-%m-%y"))


# 실습: 국가별 로케일 설정
Sys.setlocale(category = "LC_ALL", locale = "")

Sys.setlocale(category = "LC_ALL", locale = "Korean_Korea")

Sys.setlocale(category = "LC_ALL", locale = "English_US")

Sys.setlocale(category = "LC_ALL", locale = "Japanese_Japan")

Sys.getlocale()

# 실습: 미국식 날짜 표현을 한국식 날짜 표현으로 변환
strptime("01-nov-19", format = "%d-%b-%y")

Sys.setlocale(category = "LC_ALL", locale = "English_US")

strptime("01-nov-19", format = "%d-%b-%y")
day <- strptime("tuesday, 19 nov 2019", format = "%A,%d %b %Y")
day <- strptime("Tue, 19 nov 2019", format = "%a,%d %b %Y")
weekdays(day)
strptime("19 Nov 19", format = "%d %b %y")
day <- c("1may99", "2jun01", "28jul15")
strptime(day, format = "%d%b%y")


# 실습: 함수 파라미터 보기 
args(max)   # max 함수의 파라미터 확인
max(10, 20, NA, 30)

# 실습: g함수 사용 예를 보여주는 example() 함수 
example(seq)

# 실습: 평균을 구해주는 mean() 함수 사용 예 
example(mean)

mean(10:20)

x <- c(0:10, 50)
mean(x)


# 현재 작업 공간 보기(기본함수)
getwd()

# 작업공간 변경 
setwd("C:/Rwork/Part-I")
data <- read.csv("test.csv", header = T)
data

