# Chapter 04

# 실습: 산술연산자 사용
num1 <- 100
num2 <- 20
result <- num1 + num2
result
result <- num1 - num2
result
result <- num1 * num2
result
result <- num1 / num2
result

result <- num1 %% num2
result

result <- num1 ^ 2
result
result <- num1 ^ num2
result

# 실습: 관계연산자 사용
boolean <- num1 == num2
boolean
boolean <- num1 != num2
boolean

boolean <- num1 > num2
boolean
boolean <- num1 >= num2
boolean
boolean <- num1 < num2
boolean
boolean <- num1 <= num2
boolean

# 실습: 논리연ㅅ나자 사용
logical <- num1 >= 50 & num2 <= 10
logical
logical <- num1 >= 50 | num2 <= 10
logical

logical <- num1 >= 50
logical

logical <= !(num1 >= 50)
logical

x <- TRUE; y <- FALSE
xor(x, y)


# 실습: if() 함수 사용하기 
x <- 50; y <- 4; z <- x * y
if(x * y >= 40) {
  cat("x * y의 결과는 40이상입니다.\n")
  cat("x * y = ", z)
} else {
  cat("x * y의 결과는 40미만입니다. x * y = ", z, "\n")
}

# 실습: if() 함수 사용으로 입력된 점수의 학점 구하기 
score <- scan()

score
result <- "노력"
if(score >= 80) {
  result <- "우수"
}
cat("당신의 학점은 ", result, score)


# 실습: if~else if 형식으로 학점 구하기 
score <- scan()
if(score >= 90) {
  result = "A학점"
} else if(score >= 80) {
  result = "B학점"
} else if(score >= 70) {
  result = "C학점"
} else if(score >= 60) {
  result = "D학점"
} else {
  result = "F학점"
}
cat("당신의 학점은", result)
print(result)


# 실습: ifelse() 함수 사용하기 
score <- scan()

ifelse(score >= 80, "우수", "노력")
ifelse(score <= 80, "우수", "노력")

# 실습: ifelse() 함수 응용하기 
excel <- read.csv("C:/Rwork/Part-I/excel.csv", header = T)
q1 <- excel$q1
q1
ifelse(q1 >= 3, sqrt(q1), q1)

# 실습: ifelse() 함수에서 논리연산자 사용하기 
ifelse(q1 >= 2 & q1 <= 4, q1 ^ 2, q1)


# switch() 함수 사용 예 
switch("name", id = "hong", pwd = "1234", age = 105, name = "홍길동")

# 실습: switch() 함수를 사용하여 사원명으로 급여저보 보기 
empname <- scan(what = "")
empname
switch(empname, 
       hong = 250, 
       lee = 350,
       kim = 200,
       kang = 400
)

# 실습: 벡터에서 which() 함수 사용: index 값을 반환
name <- c("kim", "lee", "choi", "park")
which(name == "choi")


# 실습: 데이터프레임에서 which() 함수 사용
# 단계 1: 벡터 생성과 데이터프레임 생성
no <- c(1:5)
name <- c("홍길동", "이순신", "강감찬", "유관순", "김유신")
score <- c(85, 78, 89, 90, 74)
exam <- data.frame(학번 = no, 이름 = name, 성적 = score)
exam

# 단계 2: 일치하는 이름의 위치(인덱스) 반환
which(exam$이름 == "유관순")
exam[4, ]


# 실습: for() 함수 사용 기본
i <- c(1:10)
for(n in i) {
  print(n * 10)
  print(n)
}

# 실습: 짝수 값만 출력하기 
i <- c(1:10)
for(n in i)
  if(n %% 2 == 0) print(n)

# 실습: 짝수이면 넘기고, 홀수 값만 출력하기 
i <- c(1:10)
for(n in i) {
  if(n %% 2 == 0) {
    next
  } else {
    print(n)
  }
}


# 실습: 변수의 칼럼명 출력하기 
name <- c(names(exam))
for(n in name) {
  print(n)
}

# 실습: 벡터 데이터 사용하기 
score <- c(85, 95, 98)
name <- c("홍길동", "이순신", "강감찬")

i <- 1
for(s in score) {
  cat(name[i], " -> ", s, "\n")
  i <- i + 1
}


# 실습: while() 함수 사용하기 
i = 0
while(i < 10) {
  i <- i + 1
  print(i)
}


# 실습 : 매개변수가 없는 사용자 함수 정의하기 
f1 <- function() {
  cat("매개변수가 없는 함수")
}

f1()

# 실습: 결과를 반환하는 사용자 함수 정의하기 
f3 <- function(x, y) {
  add <- x + y
  return(add)
}

add <- f3(10, 20)


# 실습: 기본 함수를 사용하여 요약 통계량과 빈도수 구하기 
# 단계 1: 파일 불러오기 
setwd("C:/Rwork/Part-I")
test <- read.csv("test.csv", header = TRUE)
head(test)

# 단계 2: 요약 통계량 구하기 
summary(test)

# 단계 3: 특정 변수의 빈도수 구하기 
table(test$A)

# 단계 4: 각 칼럼 단위의 비도수와 최대값, 최소값 계산을 위한 사용자 함수 정의하기 
data_pro <- function(x) {
  for(idx in 1:length(x)) {
    cat(idx, "번째 칼럼의 빈도 분석 결과")
    print(table(x[idx]))
    cat("\n")
  }
  
  for(idx in 1:length(x)) {
    f <- table(x[idx])
    cat(idx, "번째 칼럼의 최대값/최소값\n")
    cat("max = ", max(f), "min = ", min(f), "\n")
  }
}

data_pro(test)

# 실습: 분산과 표준편차를 구하는 사용자 함수 정의 

x <- c(7, 5, 12, 9, 15, 6)

var_sd <- function(x) {
  var <- sum(x - mean(x) / 2) / (length(x) - 1)
  sd <- sqrt(var)
  cat("표본분산: ", var, "\n")
  cat("표본표준편차: ",sd)
}

var_sd(x)


# 실습: 피타고라스식 정의 함수 만들기 
pytha <- function(s, t) {
  a <- s ^ 2 - t ^ 2
  b <- 2 * s * t
  c <- s ^ 2 + t ^ 2
  cat("피타고라스 정리: 3개의 변수: ", a, b, c)
}

pytha(2, 1)

# 실습: 구구단 출력 함수 만들기 
gugu <- function(i, j) {
  for(x in i) {
    cat("**", x, "단**\n")
    for(y in j) {
      cat(x, " * ", y, " = ", x * y, "\n")
    }
    cat("\n")
  }
}

i <- c(2:9)
j <- (1:9)

gugu(i, j)


# 실습: 결측치를 포함하는 자료를 대상으로 평균 구하기 
# 단계 1: 결측치(NA)를 포함하는 데이터 생성
data <- c(10, 20, 5, 4, 40, 7, NA, 6, 3, NA, 2, NA)

# 단계 2: 결측치 데이터를 처리하는 함수 정의 
na <- function(x) {
  # 1차: NA 제거 
  print(x)
  print(mean(x, na.rm = T))
  
  # 2차: NA를 0으로 대체 
  data = ifelse(!is.na(x), x, 0)
  print(data)
  print(mean(data))
  
  # 3차: NA를 평균으로 대체 
  data2 = ifelse(!is.na(x), x, round(mean(x, na.rm = TRUE), 2))
  print(data2)
  print(mean(data2))
}

# 단계 3: 결측치 처리를 위한 사용자 함수 호출
na(data)


# 실습: 동전 앞면과 뒷면에 대한 난수 확률분포의 기대확률 모의 시험
# 단계 1: 동전 앞면과 뒷면의 난수 확률분포 함수 정의
coin <- function(n) {
  r <- runif(n, min = 0, max = 1)
  result <- numeric()
  for(i in 1:n) {
    if(r[i] <= 0.5)
      result[i] <- 0
    else
      result[i] <- 1
  }
  return(result)
}

# 단계2: 동전 던지기 횟수가 10회인 경우 앞면(0)과 뒷면(1)이 나오는 vector 값
coin(10)

# 단계 3: 몬테카를로 시뮬레이션 함수 정의 
montaCoin <- function(n) {
  cnt <- 0
  for(i in 1:n) {
    cnt <- cnt + coin(1)
  }
  
  result <- cnt / n
  return(result)
}

# 단계 4: 몬테카를로 시뮬레이션 함수 호출
montaCoin(10)
montaCoin(30)
montaCoin(100)
montaCoin(1000)
montaCoin(10000)


# 실습: 행/칼럼 단위의 합계와 평균 구하기 
library(RSADBE)
data("Bug_Metrics_Software")
Bug_Metrics_Software[ , , 1]

# 단계 2: 행 단위 합계와 평균 구하기 
rowSums(Bug_Metrics_Software[ , , 1])
rowMeans(Bug_Metrics_Software[ , , 1])


# 단계 3: 열 단위 합계와 평균 구하기 
colSums(Bug_Metrics_Software[ , , 1])
colMeans(Bug_Metrics_Software[ , , 1])


# 실습: 기술 통계량 관련 내장함수 사용하기 
seq(-2, 2, by = .2)
vec <- 1:10
min(vec)
max(vec)
range(vec)
mean(vec)
median(vec)
sum(vec)
sd(rnorm(10))
table(vec)


# 실습: 정규분포(연속형)의 난수 생성하기 
n <- 1000
rnorm(n, mean = 0, sd = 1)
hist(rnorm(n, mean = 0, sd = 1))


# 실습: 균등분포(연속형)의 난수 생성하기 
n <- 1000
runif(n, min = 0, max = 10)
hist(runif(n, min = 0, max = 10))


# 실습: 이항분포(이산형)의 난수 생성하기 
n <- 20

rbinom(n, 1, prob = 1 /2 )
rbinom(n, 2, 0.5)
rbinom(n, 10, 0.5)
n <- 1000
rbinom(n, 5, prob = 1 / 6)


# 실습: 종자값으로 동일한 난수 생성하기 
rnorm(5, mean = 0, sd = 1)
set.seed(123)
rnorm(5, mean = 0, sd = 1)        
set.seed(123)
rnorm(5, mean = 0, sd = 1)
set.seed(345)
rnorm(5, mean = 0, sd = 1)


# 실습: 수학 관련 내장함수 사용하기 
vec <- 1:10
proc(vec)
prod(vec)
factorial(5)
abs(-5)
sqrt(16)
vec
cumsum(vec)
log(10)
log10(10)


# 실습: 행렬연산 내장함수 사용하기 
x <- matrix(1:9, nrow = 3, ncol = 3, byrow = T)
y <- matrix(1:3, nrow = 3)
ncol(x)
nrow(x)
t(x)
cbind(x, 1:3)
rbind(x, 10:12)
diag(x)
det(x)
apply(x, 1, sum)
apply(x, 2, mean)
svd(x)
eigen(x)
x %*% y


# 실습: 집합연산 관련 내장함수 사용하기 
x <- c(1, 3, 5, 7, 9)
y <- c(3, 7)
union(x, y)
setequal(x, y)
intersect(x, y)
setdiff(x, y)
setdiff(y, x)
5 %in% y
