# 필수 라이브러리
library(tidyverse)
library(randomForest)

# 1-1. 데이터 읽어오기
df <- iris
str(df)
summary(df)
sum(is.na(df))

# 1-2. 시각화
par(mfrow=c(1,5))
colnames(df)
ggplot(data = melt(df), mapping = aes(x = value)) + geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')

# 1-3. 시각화(기타)
plot(df)

# 1-4. 데이터 분리
set.seed(42)
training_sampling <- sort(sample(1:nrow(df),nrow(df)*0.7))
test_sampling <- setdiff(1:nrow(df), training_sampling)
train <- df[training_sampling,]
test <- df[test_sampling,]

# 1-5. 학습 및 검증
rf_m <- randomForest(Species ~ Petal.Length + Petal.Width, data = train)
rf_m

rf_p <- predict(rf_m, newdata = test, type = "class")
rf_p

table(rf_p, test$Species)
