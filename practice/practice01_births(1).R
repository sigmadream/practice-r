# 통계청 데이터(1)
## 1. 파일 읽어오기
df <- read.csv("data/시군구_성_월별_출생_2021_part1.csv", fileEncoding = "euc-kr")

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
unique(melt_data$시군구별)
df2 <- melt_data[!(melt_data$시군구별=="시군구별"),]
df2
unique(df2$시군구별)

## 4. 데이터 정리
f1 <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  return(n[1])
}
f2 <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  return(n[2])
}
f3 <- function(x) {
  n <- unlist(strsplit(x, "\\."))
  return(n[3])
}
df2$연도 <- apply(as.matrix(df2$variable), 1, f1)
df2$월 <- apply(as.matrix(df2$variable), 1, f2)
df2$성별 <- apply(as.matrix(df2$variable), 1, f3)
head(df2)
unique(df2$성별)
table(df2$성별)
colnames(df2)[2] <- "기간 및 성별"
colnames(df2)[3] <- "출생아수"
head(df2)

## 5. 데이터 추출
df_all = df2[((df2$시군구별 == "전국") & (df2$성별 == "전체")),]
df_all = df_all[,c("출생아수", "연도", "월")]
df_all

## 6. 시각화를 통한 확인
sum_agg=aggregate(as.integer(df_all$출생아수)~as.integer(df_all$연도), FUN=sum)
colnames(sum_agg)[1] <- "years"
colnames(sum_agg)[2] <- "total"
plot(sum_agg$years, sum_agg$total, type="b")


# 개선된 통계청 데이터
df <- read.csv("data/시군구_성_월별_출생_2021_Part2.csv", fileEncoding = "euc-kr")
df1 <- df[, -c(grep(".[1-9]", colnames(df)))]
df1 <- df1[-1,]
df2 <- df1[c("시점", "전국")]
sum_agg=aggregate(as.integer(df2$전국)~as.integer(df2$시점), FUN=sum)
colnames(sum_agg)[1] <- "years"
colnames(sum_agg)[2] <- "total"
plot(sum_agg$years,
     sum_agg$total,
     type="b", 
     xlab = "시점",
     ylab = "인구",
     main="전국 출생아수")