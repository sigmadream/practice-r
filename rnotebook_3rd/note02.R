# 첫번째 도전

## 1. 파일 읽기
df <- read.csv("data/시군구_성_월별_출생_2021.csv", fileEncoding = "euc-kr")

## 2. 컬럼명 변경
colnames(df) # 뭘 어떻게??

f <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  if (length(n) == 1) {
    return(x)
  } else if (length(n) == 2) {
    return(gsub("X", "", paste(n[1], n[2], "전체", sep = ".")))
  } else {
    # if (n[3] == 1) {
    if (identical(n[3],"1")) {
      return(gsub("X", "", paste(n[1], n[2], "남자", sep = ".")))
    } else {
      return(gsub("X", "", paste(n[1], n[2], "여자", sep = ".")))
    }
  }
}

names(df) <- lapply(colnames(df), f)
names(df)

## 3. 잘못된 형태의 데이터 구조

library(reshape2)
melt_data <- melt(df, id = "시군구별")
melt_data[melt_data["시군구별"] == "시군구별"]

df2 <- melt_data[!(melt_data$시군구별=="시군구별" | melt_data$시군구별=="전국"),]
df2

# 두번쨰 도전
df <- read.csv("data/시군구_성_월별_출생_2021_Part2.csv", fileEncoding = "euc-kr")
df1 <- df[, -c(grep(".[1-9]", colnames(df)))]
df1 <- df1[, -which(names(df1) %in% c("전국"))]
df1 <- df1[-1,]
df2 <- df1[c("시점", "부산광역시")]

df2$시점 <- as.numeric(as.character(df2$시점))
df2$부산광역시 <- as.numeric(as.character(df2$부산광역시))
str(df2)

plot(df2$시점, 
     df2$부산광역시, 
     type="h", 
     xlab = "시점",
     ylab = "인구",
     main="부산시 출생아수")

# 시사점
## 주어진 데이터의 구조를 자유롭게 변형할 수 있는가?
## 행과 열을 조작할 수 있는가?
## 언제나 인덱스 조작은 중요하다.
## 그러나, 이거 생각보다 불편하다!
