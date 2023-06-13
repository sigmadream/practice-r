# Chapter 02

# 벡터
## 실습: c() 함수를 이용한 벡터 객체 생성
1:20
c(1:20)
c(1, 2, 3, 4, 5)

## 실습: seq() 함수를 이용한 벡터 객체 생성
seq(1, 10, 2)

## rep() 함수를 이용한 벡터 생성
rep(1:3, 3)
rep(1:3, each = 3)

## 실습: union(), setdiff() 그리고 intersect() 함수를 이용한 벡터 자료 처리 
x <- c(1, 3, 5, 7)
y <- c(3, 5)
union(x, y)
setdiff(x, y)
intersect(x, y)

## 실습: 숫자형, 문자형 논리형 벡터 생성
v1 <- c(33, -5, 20:23, 12, -2:3)
v2 <- c("홍길동", "이순신", "유관순")
v3 <- c(T, TRUE, FALSE, T, TRUE, F, T)
v1; v2; v3

# 실습: 자료형이 혼합된 경우 
v4 <- c(33, 05, 20:23, 12, "4")
v4

## 실습: 한 줄에 여러 개의 스크립트 명령문 사용 
v1; mode(v1); class(v1)
v2; mode(v2); class(v2)
v3; mode(v3); class(v3)

## 실습: 벡터 객체의 값에 칼럼명 지정
age <- c(30, 35, 40)
names(age) <- c("홍길동", "이순신", "강감찬")
age

## 실습: c() 함수에서 콤마 사용 예
v1 <- c(13, -5, 20:23, 12, -2:3)
v1[1]
v1[c(2, 4)]
v1[c(3:5)]
v1[c(4, 5:8, 7)]

## 실습: 음수 값으로 벡터 자료의 첨자를 사용하는 예
v1[-1]; v1[-c(2, 4)]; v1[-c(2:5)]; v1[-c(2, 5:10, 1)]

# 행렬

## 실습: 벡터를 이용한 행렬 객체 생성
m <- matrix(c(1:5))

## 실습: 벡터의 열 우선으로 행렬 객체 생성하기 
m <- matrix(c(1:10), nrow = 2)
m


## 실습: 행과 열의 수가 일치하지 않는 경우
m <- matrix(c(1:11), nrow = 2)
m


## 실습: 벡터의 행 우선으로 행렬 객체 생성하기 
m <- matrix(c(1:10), nrow = 2, byrow = T)
m


## 실습: 행 묶음으로 행렬 객체 생성하기 
x1 <- c(m, 40, 50:52)
x2 <- c(30, 5, 6:8)
mr <- rbind(x1, x2)
mr


## 실습: 열 묶음으로 행렬 객체 생성하기 
mc <- cbind(x1, x2)
mc

## 실습: 2행으로 행렬 객체 생성하기 
m3 <- matrix(10:19, 2)
m4 <- matrix(10:20, 2)
m3  
mode(m3); class(m3)  
  
## 실습: 첨자를 사용하여 행렬 객체에 접근하기 
m3[1, ]
m3[ , 5]
m3[2, 3]
m3[1, c(2:5)]

## 실습: 3행 3열의 행렬 객체 생성하기 
x <- matrix(c(1:9), nrow = 3, ncol = 3)
x


## 실습: 자료의 개수 보기 
length(x)
ncol(x)

## 실습: apply() 함수 적용하기 
apply(x, 1, max)
apply(x, 1, min)
apply(x, 2, mean)

## 실습: 사용자 정의 함수 적용하기 
f <- function(x) {
  x * c(1, 2, 3)
}
result <- apply(x, 1, f)
result


## 실습: 열 우선 순서로 사용자 정의 함수 적용하기 
result <- apply(x, 2, f)
result


## 실습: 행렬 객체에 칼럼명 지정하기 
colnames(x) <- c("one", "two", "three")
x

# 다차원 행렬
## 실습: 배열 객체 생성하기 
vec <- c(1:12)
arr <- array(vec, c(3, 2, 2))
arr


## 실습: 배열 객체의 자료 조회하기 
arr[ , , 1]
arr[ , , 2]
mode(arr); class(arr)

# 데이터프레임
## 실습: 벡터를 이용한 데이터프레임 객체 생성하기
no <- c(1, 2, 3)
name <- c("hong", "lee", "kim")
pay <- c(150, 250, 300)
vemp <- data.frame(No = no, Name = name, Pay = pay)
vemp


## 실습: matrix를 이용한 데이터프레임 객체 생성하기 
m <- matrix(
  c(1, "hong", 150,
    2, "lee", 250,
    3, "kim", 300), 3, by = T)
memp <- data.frame(m)
memp


## 실습: 텍스트 파일을 이용한 데이터프레임 객체 생성하기 
getwd()
txtemp <- read.table('data/emp.txt', header = 1, sep = "", fileEncoding = "euc-kr")
txtemp


## 실습: csv 파일을 이용한 데이터프레임 객체 생성하기 
csvtemp <- read.csv('data/emp.csv', header = T)
csvtemp
help(read.csv)
name <- c("사번", "이름", "급여")
read.csv('data/emp2.csv', header = F, col.names = name, fileEncoding = "euc-kr")

## 실습: 데이터프레임 만들기 
df <- data.frame(x = c(1:5), y = seq(2, 10, 2), z = c('a', 'b', 'c', 'd', 'e'))
df$x
str(df) # 자료구조
names(df) # 컬럼명
ncol(df)
nrow(df)
df[c(2:3), 1]

## 실습: 요약 통계량 보기 
summary(df)


## 실습: 데이터프레임 자료에 함수 적용하기 
apply(df[ , c(1, 2)], 2, sum)


## 실습: 데이터프레임의 부분 객체 만들기 
x1 <- subset(df, x >= 3)
y1 <- subset(df, y <= 8)
xyand <- subset(df, x >= 2 & y <= 6)
xyor <- subset(df, x >= 2 | y <= 6)

## 실습: 두 개 이상의 데이터프레임 병합하기 
### 단계 1: 병합할 데이터프레임 생성
height <- data.frame(id = c(1, 2), h = c(180, 175))
weight <- data.frame(id = c(1, 2), w = c(80, 75))

### 단계 2: 데이터프레임 병합하기 
user <- merge(height, weight, by.x = "id", by.y = "id")
user

# 딕셔너리
## 실습: key를 생략한 list 생성하기
list <- list("lee", "이순신", 95)
list

## 실습: 리스트를 벡터 구조로 변경하기 
unlist <- unlist(list)
unlist

## 실습: 1개 이상의 값을 갖는 리스트 객체 생성하기 
num <- list(c(1:5), c(6, 10))
num

## 실습: key와 value 형식으로 리스트 객체 생성하기 
member <- list(name = c("홍길동", "유관순"), age = c(35, 25),
               address = c("한양", "충남"), gender = c("남자", "여자"),
               htype = c("아파트", "오피스텔"))
member$name
member$name[1]
member$name[2]

## 실습: key를 이용하여 value에 접근하기 
member$age[1] <- 45
member$id <- "hong"
member$pwd <- "1234"
member

length(member)
mode(member); class(member)

## 실습: 리스트 객체에 함수 적용하기 
a <- list(c(1:5))
b <- list(c(6:10))
lapply(c(a, b), max)


## 실습: 리스트 형식을 벡터 형식으로 반환하기 
sapply(c(a, b), max) 


## 실습: 다차원 리스트 객체 생성하기 
multi_list <- list(c1 = list(1, 2, 3),
                   c2 = list(10, 20, 30), 
                   c3 = list(100, 200, 300))
multi_list$c1; multi_list$c2; multi_list$c3

## 실습: 다차원 리스트를 열 단위로 바인딩하기 
do.call(cbind, multi_list)
class(do.call(cbind, multi_list))


## 실습: 문자열 추출하기 
install.packages("stringr")
library(stringr)
str_extract("홍길동35이순신45유관순25", "[1-9]{2}")
str_extract_all("홍길동35이순신45유관순25", "[1-9]{2}")

## 실습: 반복 수를 지정하여 영문자 추출하기 
string <- "hongkd105leess1002you25강감찬2005"
str_extract_all(string, "[a-z]{3}")
str_extract_all(string, "[a-z]{3,}")
str_extract_all(string, "[a-z]{3,5}")

## 실습: 문자열에서 한글, 영문자, 숫자 추출하기 
str_extract_all(string, "hong")
str_extract_all(string, "25")
str_extract_all(string, "[가-힣]{3}")
str_extract_all(string, "[a-z]{3}")
str_extract_all(string, "[0-9]{4}")

## 실습: 문자열에서 한글, 영문자, 숫자를 제외한 나머지 추출하기 
str_extract_all(string, "[^a-z]")
str_extract_all(string, "[^a-z]{4}")
str_extract_all(string, "[^가-힣]{5}")
str_extract_all(string, "[^0-9]{3}")


## 실습: 주민등록번호 검사하기 
jumin <- "123456-1234567"
str_extract(jumin, "[0-9]{6}-[1234][0-9]{6}")
str_extract_all(jumin, "\\d{6}-[1234]\\d{6}")


## 실습: 지정된 길이의 단어 추출하기 
name <- "홍길동1234,이순신5678,강감찬1012"
str_extract_all(name, "\\w{7,}")


## 실습: 문자열의 길이 구하기 
string <- "hongkd105leess1002you25강감찬2005"
len <- str_length(string)
len

## 실습: 문자열 내에서 특정 문자열의 위치(index) 구하기 
string <- "hongkd105leess1002you25강감찬2005"
str_locate(string, "강감찬")


## 실습: 부분 문자열 만들기 
string_sub <- str_sub(string, 1, len - 7)
string_sub
string_sub <- str_sub(string, 1, 23)
string_sub


## 실습: 대문자, 소문자 변경하기 
ustr <- str_to_upper(string_sub); ustr
str_to_lower(ustr)

## 실습: 문자열 교체하기 
string_sub
string_rep <- str_replace(string_sub, "hongkd105", "홍길동35,")
string_rep <- str_replace(string_rep, "leess1002", "이순신45,")
string_rep <- str_replace(string_rep, "you25", "유관순25,")
string_rep


## 실습: 문자열 결합하기 
string_rep
string_c <- str_c(string_rep, "강감찬55")
string_c

## 실습: 문자열 분리하기 
string_c
string_sp <- str_split(string_c, ",")
string_sp


## 실습: 문자열 합치기
### 단계 1: 문자열 벡터 만들기 
string_vec <- c("홍길동35", "이순신45", "유관순25", "강감찬55")
string_vec

### 단계 2: 콤마를 기준으로 문자열 벡터 합치기 
string_join <- paste(string_vec, collapse = ",")
string_join