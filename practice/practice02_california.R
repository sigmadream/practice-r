# 필수 라이브러리
library(tidyverse)
library(reshape2)
library(boot)
library(randomForest)

# 1-1. 데이터 읽어오기
housing = read.csv('./data/housing.csv')
head(housing)
summary(housing)

# 1-2. 시각화
par(mfrow=c(2,5))
colnames(housing)
ggplot(data = melt(housing), mapping = aes(x = value)) + geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')

# 1-3. 데이터 정리
## NA 값
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median(housing$total_bedrooms , na.rm = TRUE)
housing$mean_bedrooms = housing$total_bedrooms/housing$households
housing$mean_rooms = housing$total_rooms/housing$households
drops = c('total_bedrooms', 'total_rooms')
housing = housing[ , !(names(housing) %in% drops)]
head(housing)

## 1-4. 카테고리 정리
categories <- unique(housing$ocean_proximity)
cat_housing <- data.frame(ocean_proximity = housing$ocean_proximity)

for(cat in categories){
  cat_housing[,cat] = rep(0, times= nrow(cat_housing))
}
head(cat_housing)

for(i in 1:length(cat_housing$ocean_proximity)){
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] = 1
}
head(cat_housing)

cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing,one_of(keep_columns))
tail(cat_housing)

colnames(housing)
drops = c('ocean_proximity','median_house_value')
housing_num =  housing[ , !(names(housing) %in% drops)]
head(housing_num)

## 1-5. 정규화
scaled_housing_num = scale(housing_num)
head(scaled_housing_num)

# 1-4. 데이터 분리
cleaned_housing = cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)
head(cleaned_housing)

set.seed(42)
sample = sample.int(n = nrow(cleaned_housing), size = floor(.8*nrow(cleaned_housing)), replace = F)
train = cleaned_housing[sample, ] #just the samples
test  = cleaned_housing[-sample, ] #everything but the samples

head(train)
nrow(train) + nrow(test) == nrow(cleaned_housing)

# 1-5. 학습 및 검증
glm_house = glm(median_house_value~median_income+mean_rooms+population, data=cleaned_housing)
k_fold_cv_error = cv.glm(cleaned_housing , glm_house, K=5)
k_fold_cv_error$delta

glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse

names(glm_house)
glm_house$coefficients

# 2-1. 개선 - 랜덤 포레스트
names(train)
set.seed(42)
train_y = train[,'median_house_value']
train_x = train[, names(train) !='median_house_value']
head(train_y)
head(train_x)

rf_model = randomForest(train_x, y = train_y , ntree = 500, importance = TRUE)
names(rf_model)
rf_model$importance

oob_prediction = predict(rf_model)
train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse

test_y = test[,'median_house_value']
test_x = test[, names(test) !='median_house_value']

y_pred = predict(rf_model , test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse
