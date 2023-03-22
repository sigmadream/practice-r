install.packages("foreign")  # foreign 패키지 설치
library(foreign)             # SPSS 파일 로드
library(dplyr)               # 전처리
library(ggplot2)             # 시각화
library(readxl)              # 엑셀 파일 불러오기
# 1. 데이터 불러오기
raw_welfare <- read.spss(file = "./practice/Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

# 1. 데이터 확인
welfare <- raw_welfare
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

# 1. 데이터 정리
welfare <- rename(welfare,
                  sex = h10_g3,            # 성별
                  birth = h10_g4,          # 태어난 연도
                  marriage = h10_g10,      # 혼인 상태
                  religion = h10_g11,      # 종교
                  income = p1002_8aq1,     # 월급
                  code_job = h10_eco9,     # 직종 코드
                  code_region = h10_reg7)  # 지역 코드

# 2. 결측치(1)
class(welfare$sex)
table(welfare$sex)

## 이상치 결측 처리
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)

## 결측치 확인
table(is.na(welfare$sex))

## 성별 항목 이름 부여
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

# 2. 결측치(2)
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)

## 이상치 확인
summary(welfare$income)

## 이상치 결측 처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

## 결측치 확인
table(is.na(welfare$income))

## 간단한 데이터 확인
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

# 2. 결측치(3)
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

## 이상치 확인
summary(welfare$birth)

## 결측치 확인
table(is.na(welfare$birth))

## 이상치 결측 처리
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

## 결측치 확인
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

## 간단한 데이터 확인
age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))

head(age_income)

ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()