# Chapter 03

## 추천하지 않는 방법
### 실습: 키보드로 숫자 입력하기 (사용하지 마세요)
num <- scan()
num
sum(num)

### 실습: 키보드로 문자 입력하기 (사용하지 마세요)
name <- scan(what = character())
name

### 실습: 편집기를 이용한 데이터프레임 만들기 (사용하지 마세요)
df = data.frame()
df = edit(df)
df

### 실습: 탐색기를 통해서 파일 선택하기 (이것도 사용하지 마세요)
student1 <- read.table(file.choose(), header = TRUE)

# 데이터 프레임 만들기

## 실습: 칼럼명이 없는 파일 불러오기 
student <- read.table(file = "./data/student.txt")
names(student) <- c("번호", "이름", "키", "몸무게")
student

## 실습: 구분자가 있는 경우 
student2 <- read.table(file = "./data/student2.txt", sep = ";", header = TRUE, fileEncoding = "euc-kr")

## 실습: 결측치를 처리하여 파일 불러오기 
student3 <- read.table(file = "./data/student3.txt", header = TRUE, na.strings = "-", fileEncoding = "euc-kr")

## 실습: CSV 파일 형식 불러오기 
student4 <- read.csv(file = "./data/student4.txt", sep = ",", na.strings = "-", fileEncoding = "euc-kr")

## 실습: xlsx 패키지 설치와 Java 실행화경 설정
#install.packages("xlsx")
#install.packages("rJava")

## 실습: xlsx 관련 패키지를 메모리에 로드 
library(rJava)
library(xlsx)

## 실습: 엑셀 파일 가져오기 
studentx <- read.xlsx("./data/studentexcel.xlsx", sheetIndex = 1, encoding = "UTF-8")
studentx

## 실습: 인터넷에서 파일을 가져와 시각화하기 
### 단계 1: 깃허브에서 URL을 사용하여 타이타닉(titanic) 자료 가져오기 
titanic <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/COUNT/titanic.csv")
titanic

### 단계 2: 인터넷(깃허브)에서 가져온 자료의 차원 정보와 자료구조 보기 및 범주의 빈도수 확인
dim(titanic)
str(titanic)

table(titanic$age)
table(titanic$sex)
table(titanic$survived)

### 단계 3: 관측치 살펴보기 
head(titanic)
tail(titanic)

### 단계 4: 교차 분할표 작성하기 
tab <- table(titanic$survived, titanic$sex)
tab

### 단계 5: 범주의 시각화 - 막대 차트 그리기
barplot(tab, col = rainbow(2), main = "성별에 따른 생존 여부")

## 실습: cat() 함수 이용 변수 출력하기 
x <- 10
y <- 20
z <- x * y
cat("x * y의 결과는 ", z, "입니다.\n")
cat("x * y = ", z)

## 실습: print() 함수 이용 변수 출력하기 
print(z)

## 실습: write.csv() 함수를 이용한 파일 저장하기 
st.df <- studentx
write.csv(st.df, "stdf.csv", row.names = F, quote = F)

## 실습: writexl 패키지 설치와 로드 
install.packages("writexl")
library(writexl)

## 실습: 엑셀로 저장하기 
write_xlsx(x = st.df, path = "st_excel.xlsx", col_names = TRUE)
