# Chapter 01

## R 패키지 관련
available.packages()
dim(available.packages())
installed.packages()
colnames(installed.packages())
library()
install.packages("stringr")
library(stringr) # ERROR!
require(stringr) # WARNING
search() # 메모리에 적재된 패키지
remove.packages("stringr")

## R Session
sessionInfo()

## 기본 데이터 셋 보기
data()

## 간단한 시각화
hist(Nile)
hist(Nile, freq = F)
lines(density(Nile))

# 현재 작업 공간 보기(기본함수)
getwd()

# 작업공간 변경 
data <- read.csv("data/test.csv", header = T)
data

