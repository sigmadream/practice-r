# Chapter 12

# 실습: 변수 리코딩과 데이터프레임 생성하기 
# 단계 1: 실습 파일 가져오기 
setwd("C:/Rwork/Part-III")
data <- read.csv("cleanDescriptive.csv", header = TRUE)

head(data)

# 단계 2: 변수 리코딩
x <- data$level2
y <- data$pass2

# 단계 3: 데이터프레임 생성
result <- data.frame(Level = x, Pass = y)
dim(result)



# 실습: 교차 분할표 작성
# 단계 1: 기본함수를 이용한 교차 분할표 작성
table(result)

# 단계 2: 교차 분할표 작성을 위한 패키지 설치
install.packages("gmodels")
library(gmodels)
install.packages("ggplot2")
library(ggplot2)


# 단계 3: 패키지를 이용한 교차 분할표 작성
CrossTable(x = diamonds$color, y = diamonds$cut)



# 실습: 패키지를 이용한 교차 분할표 작성: 부모의 학력수준과 자녀 대학 진학여부
x <- data$level2
y <- data$pass2

CrossTable(x, y)



# 실습: CrossTable() 함수를 이용한 카이제곱 검정
CrossTable(x = diamonds$cut, 
           y = diamonds$color, chisq = TRUE)


# 실습: 주사위 적합도 검정
chisq.test(c(4, 6, 17, 16, 8, 9))


# 실습: 5개의 스포츠음료에 대한 선호도에 차이가 있는지 검정
data <- textConnection(
  "스포츠음료종류 관측도수
  1 41
  2 30
  3 51
  4 71
  5 61
  ")
x <- read.table(data, header = T)
x

chisq.test(x$관측도수)



# 실습: 부모의 학력수준과 자녀의 대학 진학여부의 독립성(관련성) 검정
setwd("C:/Rwork/Part-III")
data <- read.csv("cleanDescriptive.csv", header = TRUE)
x <- data$level2
y <- data$pass2
CrossTable(x, y, chisq = TRUE)


# 실습: 교육센터에서 교육방법에 따라 교육생들의 만족도에 차이가 있는지 검정
# 단계 1: 데이터 가져오기 
setwd("C:/Rwork/Part-III")
data <- read.csv("homogenity.csv")
head(data)

data <- subset(data, !is.na(survey), c(method, survey))

# 단계 2: 코딩 변경(변수 리코딩)
data$method2[data$method == 1] <- "방법1"
data$method2[data$method == 2] <- "방법2"
data$method2[data$method == 3] <- "방법3"

data$survey2[data$survey == 1] <- "1.매우만족"
data$survey2[data$survey == 2] <- "2.만족"
data$survey2[data$survey == 3] <- "3.보통"
data$survey2[data$survey == 4] <- "4.불만족"
data$survey2[data$survey == 5] <- "5.매우불만족"

# 단계 3: 교차 분할표 작성
table(data$method2, data$survey2)


# 단계 4: 동질성 검정 - 모든 특성치에 대한 추론검정
chisq.test(data$method2, data$survey2)
