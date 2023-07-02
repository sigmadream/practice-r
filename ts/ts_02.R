# Forecasting Part I

library(zoo)
library(xts)
library(dplyr)
library(tsibble)
library(readxl)
library(ggplot2)
library(ggrepel)
library(forecast)
library(feasts)
library(timetk)
library(lubridate)
library(tibbletime)
library(urca)

## 데이터 불러오기

students.all <- read_excel("data/ts_students.xlsx", skip = 16, na = '-', sheet = 1, col_types = c('text', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric','numeric', 'numeric', 'numeric'))

students <- 
  students.all %>%
  filter(지역규모 == '계') %>% 
  select(-지역규모)

students$연도 <- as.Date(paste0(students$연도,'-01-01'))
students.ts <- ts(students, frequency = 1, start = 1999)
students.xts <- as.xts(students[,-1], order.by = students$연도)
students.tsibble <- 
  students %>%
    mutate(연도 = yearmonth(paste0(students$연도, '-01-01')))

students.tsibble <- as_tsibble(students.tsibble, index = 연도)

## 지연과 차분

students %>%
  mutate(lag1 = lag(학생수계, 1),
         lag3 = lag(학생수계, 3)) %>%
  select(연도, 학생수계, lag1, lag3) %>%
  head(10)

students %>%
  mutate(lag1 = lag_vec(학생수계, lag = 1),
         lag3 = lag_vec(학생수계, lag = 3)) %>%
  select(연도, 학생수계, lag1, lag3) %>%
  head(10)

stats::lag(students.xts$학생수계, 1) %>% head(10)

students %>%
  mutate(lag1 = lag(학생수계, 1),
         lag3 = lag(학생수계, 3),
         diff1 = c(NA, diff(학생수계, lag = 1)),
         diff3 = c(NA, NA, NA, diff(학생수계, lag = 3))) %>%
  select(연도, 학생수계, lag1, diff1, lag3, diff3) %>%
  head(10)

students %>%
  mutate(diff1 = diff_vec(학생수계, lag = 1),
         diff3 = diff_vec(학생수계, lag = 3)) %>%
  select(연도, 학생수계, diff1, diff3) %>%
  head(10)

diff(students.xts$학생수계, 1) %>% head(10)

## ACF와 PACF

### stats 패키지의 acf plot 생성
acf(students$학생수계)

# stats 패키지의 acf 수치 산출
acf(students$학생수계, plot = FALSE)

# forecast 패키지의 Acf plot
students %>%
  select(학생수계) %>%
  forecast::Acf()

# forecast 패키지의 Acf 수치
students %>%
  select(학생수계) %>%
  forecast::Acf(plot = FALSE)

# forecast 패키지의 ggAcf 플롯
students %>%
  select(학생수계) %>%
  forecast::ggAcf()

# forecast 패키지의 ggAcf 수치
students %>%
  select(학생수계) %>%
  forecast::ggAcf(plot = FALSE)

### timetk 패키지의 plot_acf_diagnostics plot
students %>%
  select(연도, 학생수계) %>%
  timetk::plot_acf_diagnostics(.date_var = 연도, .value = 학생수계, .lag = 14, .show_white_noise_bars = TRUE)

# stats 패키지의 pacf plot
students %>%
  select(학생수계) %>%
  stats::pacf()

# stats 패키지의 pacf 수치
students %>%
  select(학생수계) %>%
  stats::pacf(plot = FALSE)

# forecast 패키지의 Pacf plot
students %>%
  select(학생수계) %>%
  forecast::Pacf()

# forecast 패키지의 Pacf 수치
students %>%
  select(학생수계) %>%
  forecast::Pacf(plot = FALSE)

# forecast 패키지의 ggPacf plot
students %>%
  select(학생수계) %>%
  forecast::ggPacf()

# forecast 패키지의 ggPacf 수치
students %>%
  select(학생수계) %>%
  forecast::ggPacf(plot = FALSE)

## 적합값과 잔차

student.ts.lm <- forecast::tslm(students.ts[,2] ~ trend, data = students.ts)

# 전체 학생수 선형 회귀 모델의 적합값 산출
fitted(student.ts.lm)

# 전체 학생수 선형 회귀 모델의 잔차 산출
residuals(student.ts.lm)

## 백색잡음
data(goog200, package = 'fpp2')
checkresiduals(naive(goog200))

## 시계열 분해

# 학생수계는 연별 데이터이기 때문에 계절성을 추출할 수 없음 -> 에러나는게 정상입니다.
students.ts[, 2] %>%
  decompose() %>% autoplot()

# 취업자수를 stl()을 사용하여 분해
employees.ts[,2] %>%
  stl(s.window = 'periodic') %>% autoplot()

## 정상성 테스트
employees.ts[,2] %>% ur.kpss() %>% summary()

# nsdiffs()로 몇 회의 차분이 필요한지 검사 - 1회의 차분이 필요함
forecast::nsdiffs(employees.ts[,2], alpha = 0.05)

# 1회 차분한 결과에 대한 KPSS 테스트 시행
diff(employees.ts[,2]) %>% ur.kpss() %>% summary()

### 계절성 검정
library(seastests)
# 총학생수계는 연별 데이터이므로 계절성이 존재할 수 없다.
summary(wo(students.ts[,2]))

# 총취업자수는 계절성이 존재하는지 검사
summary(wo(employees.ts[,2]))        

forecast::nsdiffs(employees.ts[,2]) ### seasonality를 제거하기 위해 필요한 차분수

# 교육서비스업 취업자수의 계절성 검사
summary(wo(employees.ts[,3]))

employees.ts[,2] %>% decompose() %>% seasadj() %>% autoplot()

ggseasonplot(employees.ts[,2], main = '연도별 월간 플롯', ylab = '취업자수', xlab = '월',
             year.labels = T)

ggsubseriesplot(employees.ts[,2], main = '월별 연간 플롯', ylab = '취업자수', xlab = '월')

######################################
###  6장 시계열 forecasting Part II - 시계열 예측 모델

###  6.1 평균 모델

library(forecast)

# meanf()를 사용하여 ts 객체의 학생수계 열에 대한 평균 모델을 생성하고 summary()로 상세 내용을 출력
summary(meanf(students.ts[,2]))

# autoplot()을 사용하여 ts 객체의 학생수계 열에 대한 평균 모델을 시각화
autoplot(meanf(students.ts[,2]), main = '학생수 평균 모델 플롯', xlab = '연도', ylab = '학생수')

# autoplot()을 사용하여 ts 객체의 학생수계 열에 대한 평균 모델(예측구간 산출에 boosted 방법을 사용)을 시각화
autoplot(meanf(students.ts[,2], bootstrap = TRUE), main = '학생수의 평균 모델 플롯(부트스트랩)', xlab = '연도', ylab = '학생수')

# 전체 취업자수에 대한 평균 모델 시각화
autoplot(meanf(employees.ts[,2]), main = '신규 취업자수 평균 모델 플롯', xlab = '연도', ylab = '취업자수')

# 코로나 19 확진자에 대한 평균 모델(예측구간 산출에 boosted 방법을 사용) 시각화
autoplot(meanf(covid19.ts[,2], bootstrap = TRUE), main = '코로나 확진자(0-9세) 평균 모델 플롯(부트스트랩)', xlab = '기간', ylab = '확진자수')

###  6.2 단순 모델

# 학생수계 열에 대한 단순(Naïve) 모델의 상세 정보와 플롯
students.ts[, 2] %>% naive() %>% summary()

students.ts[, 2] %>% naive() %>% autoplot(main = '전체 학생수 단순 모델 플롯', xlab = '연도', ylab = '학생수')
# 취업자수 열에 대한 단순(Naive) 모델의 상세 정보와 플롯
employees.ts[,2] %>% naive() %>% summary()

employees.ts[,2] %>% naive() %>% autoplot(main = '신규 취업자수 단순 모델 플롯', xlab = '연도', ylab = '취업자수')

# 0-9세 코로나 확진자수에 대한 단순 모델의 상세 정보와 플롯
covid19.ts[,2] %>% naive() %>% summary()

covid19.ts[,2] %>% naive() %>% autoplot(main = '코로나19 확진자(0-9세)의 단순 모델 플롯', xlab = '기간', ylab = '확진자수')

###  6.3 계절성 단순 모델

# 학생수계 열에 대한 계절성 단순 모델의 상세 정보와 플롯
students.ts[,2] %>% snaive(10) %>% summary()

students.ts[,2] %>% snaive(10) %>% autoplot(main = '전체 학생수 계절성 단순 모델 플롯', xlab = '연도', ylab = '학생수')

# 취업자수 열에 대한 계절성 단순 모델의 상세 정보와 플롯
employees.ts[,2] %>% snaive(10) %>% summary()

employees.ts[,2] %>% snaive(10) %>% autoplot(main = '신규 취업자수 계절성 단순 모델 플롯', xlab = '연도', ylab = '취업자수')


###  6.4 랜덤워크 모델

# 학생수계 열에 대한 랜덤워크 모델의 상세 정보와 플롯
students.ts[,2] %>% rwf() %>% summary()

# 학생수계 열에 대한 드리프트가 있는 랜덤워크 모델의 상세 정보와 플롯
students.ts[,2] %>% rwf(drift = T) %>% summary()

students.ts[,2] %>% rwf(drift = T) %>% autoplot(main = '드리프트가 있는 전체 학생수 랜덤워크 모델 플롯', xlab = '연도', ylab = '학생수')

# 취업자수 열에 대한 랜덤워크 모델의 상세 정보와 플롯
employees.ts[,2] %>% rwf() %>% summary()

employees.ts[,2] %>% rwf() %>% autoplot(main = '신규 취업자수 랜덤워크 모델 플롯', xlab = '연도', ylab = '취업자수')

# 취업자수 열에 대한 드리프트가 있는 랜덤워크 모델의 상세 정보와 플롯
employees.ts[,2] %>% rwf(drift = T) %>% summary()

employees.ts[,2] %>% rwf(drift = T) %>% autoplot(main = '드리프트가 있는 신규 취업자수 랜덤워크 모델 플롯', xlab = '연도', ylab = '취업자수')

# 0-9세 코로나 확진자수에 대한 계절성 랜덤워크 모델의 상세 정보와 플롯
covid19.ts[,2] %>% rwf(30) %>% summary()

covid19.ts[,2] %>% rwf(30) %>% autoplot(main = '코로나19 확진자(0-9세)의 랜덤워크 모델 플롯', xlab = '기간', ylab = '확진자수')

# 취업자수 열에 대한 드리프트가 있는 랜덤워크 모델의 상세 정보와 플롯
covid19.ts[,2] %>% rwf(30, drift = T) %>% summary()

covid19.ts[,2] %>% rwf(30, drift = T) %>% autoplot(main = '드리프트가 있는 코로나19 확진자(0-9세)의 랜덤워크 모델 플롯', xlab = '기간', ylab = '확진자수')

set.seed(345)

# 백색잡음 시뮬레이션 데이터 생성
whitenoise <- ts(rnorm(100), start = 1)

ts.plot(whitenoise, ylab = '')

# 백색잡음 데이터로 랜덤워크 생성
whitenoise.to.randomwalk <- cumsum(whitenoise)

ts.plot(whitenoise.to.randomwalk, ylab = '')

# 랜덤워크에서 백색잡음 생성
randomwalk.to.whitenoise <- diff(whitenoise.to.randomwalk)

ts.plot(randomwalk.to.whitenoise, ylab = '')

# 학생수계의 평균, 단순, 계절성 단순, 랜덤워크 모델의 예측값 플롯
autoplot(students.ts[,2]) +
  autolayer(meanf(students.ts[,2], h = 10), PI = FALSE, series = '평균') +
  autolayer(naive(students.ts[,2], h = 10), PI = FALSE, series = '단순') +
  autolayer(snaive(students.ts[,2], h = 10), PI = FALSE, series = '계절성 단순') +
  autolayer(rwf(students.ts[,2], h = 10), PI = FALSE, series = '랜덤워크') +
  autolayer(rwf(students.ts[,2], h = 10, drift = TRUE), PI = FALSE, series = '랜덤워크-드리프트') +
  labs(title = '전체 학생수의 평균, 단순, 계절성 단순, 랜덤워크 모델 예측값', x = '연도', y = '학생수')

# 취업자수의 평균, 단순, 계절성 단순, 랜덤워크 모델의 예측값 플롯
autoplot(employees.ts[,2]) +
  autolayer(meanf(employees.ts[,2], h = 10), PI = FALSE, series = '평균') +
  autolayer(naive(employees.ts[,2], h = 10), PI = FALSE, series = '단순') +
  autolayer(snaive(employees.ts[,2], h = 10), PI = FALSE, series = '계절성 단순') +
  autolayer(rwf(employees.ts[,2], h = 10), PI = FALSE, series = '랜덤워크') +
  autolayer(rwf(employees.ts[,2], h = 10, drift = TRUE), PI = FALSE, series = '랜덤워크-드리프트') +
  labs(title = '신규 취업자수의 평균, 단순, 계절성 단순, 랜덤워크 모델 예측값', x = '연도', y = '취업자수')

# 0-9세 코로나 확진자의 평균, 단순, 계절성 단순, 랜덤워크 모델의 예측값 플롯
autoplot(covid19.ts[,2]) +
  autolayer(meanf(covid19.ts[,2], h = 30), PI = FALSE, series = '평균') +
  autolayer(naive(covid19.ts[,2], h = 30), PI = FALSE, series = '단순') +
  autolayer(snaive(covid19.ts[,2], h = 30), PI = FALSE, series = '계절성 단순') +
  autolayer(rwf(covid19.ts[,2], h = 30), PI = FALSE, series = '랜덤워크') +
  autolayer(rwf(covid19.ts[,2], h = 30, drift = TRUE), PI = FALSE, series = '랜덤워크-드리프트') +
  labs(title = '코로나 확진자(0-9세)의 평균, 단순, 계절성 단순, 랜덤워크 모델 예측값', x = '시간', y
       = '확진자수')


###  6.5 회귀 모델

# 전체 학생수 예측 모델을 추세를 반영하여 생성
student.ts.lm <- tslm(students.ts[,2] ~ trend, data = students.ts)

summary(student.ts.lm)

# 전체 학생수 예측 모델을 forecast()를 사용해 예측값을 산출하고 autoplot()으로 플롯 생성
student.ts.lm %>% forecast() %>% autoplot() + labs(title = '전체 학생수에 대한 시계열 선형 회귀 예측 결과', x = '연도', y = '학생수')

# 초등학생 학생수의 예측 모델을 생성하는 데 유치원 학생수와 trend를 사용하는 선형 모델을 생성
student.ts.lm <- tslm(students.ts[,4] ~ students.ts[,3] + trend, data = students.ts)

# forecast()로 생성된 모델에 대한 미래 예측 데이터를 만들고 autoplot()으로 플롯 생성
student.ts.lm %>% forecast(h = 22) %>% autoplot(main = '유치원 학생수와 추세를 활용한 초등학생수 시계열 선형 회귀 예측 결과', xlab = '연도', ylab = '학생수')

## 에러나는게 정상입니다
student.ts.lm <- tslm(students.ts[,2] ~ trend + season, data = students.ts)

# 전체 취업자수를 추세(trend)만으로 선형 회귀 분석
employee.total.ts.lm <- tslm(employees.ts[,2] ~ trend, data = employees.ts)

# y절편이 25430, 기울기가 20.39인 선형 회귀 모델 생성
summary(employee.total.ts.lm)

# 전체 취업자수에 대한 선형 회귀 모델의 예측값에 대한 플롯 생성
employee.total.ts.lm %>% forecast() %>% autoplot() + labs(title = '신규 취업자수에 대한 시계열 선형 회귀 예측 결과', x = '시간', y = '취업자수')

# 전체 취업자수를 추세(trend)와 계절성(season)으로 선형 회귀 분석
employee.total.ts.lm <- tslm(employees.ts[,2] ~ trend + season, data = employees.ts)

summary(employee.total.ts.lm)

employee.total.ts.lm %>% forecast() %>% autoplot() + labs(title = '신규 취업자수에 대한 시계열 계절성 선형 회귀 예측 결과', x = '시간', y = '취업자수')

checkresiduals(tslm(students.ts[,2] ~ trend, data = students.ts))

library(timetk)

library(lubridate)

plot_time_series_regression(.data = students,
                            .date_var = 연도,
                            .formula = 학생수계 ~ 연도,
                            .interactive = FALSE,
                            .show_summary = TRUE) +
  labs(title = 'timetk를 사용한 전체 학생수 시계열 회귀 모델', x = '연도', y = '학생수')

# 계절성 반영을 위해
employees$date <- as.yearmon(employees$time, "%Y. %m")

# plot_time_series_regression에 trend만 반영 시
plot_time_series_regression(.data = employees,
                            .date_var = time,
                            .formula = total ~ as.numeric(yearmonth(date)),
                            .interactive = FALSE) +
  labs(title = 'timetk를 사용한 신규 취업자수 시계열 회귀 모델', x = '연도', y = '취업자수')

# plot_time_series_regression에 trend, season(월)까지 반영
plot_time_series_regression(.data = employees,
                            .date_var = time,
                            .formula = total ~ year(date) +
                              month(date, label = TRUE),
                            .interactive = FALSE) +
  labs(title = 'timetk를 사용한 신규 취업자수 계절성 시계열 회귀 모델', x = '연도', y = '취업자수')


###  6.6 지수 평활 모델

# 전체 학생수에 대한 단순 지수 평활 모델
ses(students.ts[,2]) %>% summary()

autoplot(students.ts[,2]) +
  autolayer(fitted(ses(students.ts[,2])), series = '적합값') +
  autolayer(ses(students.ts[,2])) +
  labs(title = '전체 학생수에 대한 단순 지수 평활 모델', x = '연도', y = '학생수')

autoplot(students.ts[,2]) +
  autolayer(fitted(ses(students.ts[,2])), series = '적합값') +
  autolayer(ses(students.ts[,2]), PI = F, series = '0.99') +
  autolayer(ses(students.ts[,2], alpha = 0.5), PI = F, series = '0.5') +
  autolayer(ses(students.ts[,2], alpha = 0.3), PI = F, series = '0.3') +
  labs(title = 'alpha값에 따른 단순 지수 평활 모델(전체 학생수)', x = '연도', y = '학생수', color
       = 'alpha')

# 전체 취업자수에 대한 Simple Exponential Smoothing
autoplot(employees.ts[,2]) +
  autolayer(fitted(ses(employees.ts[,2])), series = '적합값') +
  autolayer(ses(employees.ts[,2]), series = 'auto', PI = F) +
  autolayer(ses(employees.ts[,2], alpha = 0.3), series = '0.3', PI = F) +
  autolayer(ses(employees.ts[,2], alpha = 0.5), series = '0.5', PI = F) +
  autolayer(ses(employees.ts[,2], alpha = 0.7), series = '0.7', PI = F) +
  labs(title = 'alpha값에 따른 단순 지수 평활 모델(신규 취업자수)', x = '연도', y = '취업자수',
       color = 'alpha')

# 코로나 신규 확진자수(0-9세)에 대한 Simple Exponential Smoothing
autoplot(covid19.ts[,2]) +
  autolayer(fitted(ses(covid19.ts[,2])), series = '적합값') +
  autolayer(ses(covid19.ts[,2], h = 30), series = 'auto', PI = F) +
  autolayer(ses(covid19.ts[,2], alpha = 0.3, h = 30), series = '0.3', PI = F) +
  autolayer(ses(covid19.ts[,2], alpha = 0.5, h = 30), series = '0.5', PI = F) +
  autolayer(ses(covid19.ts[,2], alpha = 0.7, h = 30), series = '0.7', PI = F) +
  labs(title = 'alpha값에 따른 단순 지수 평활 모델(신규 확진자수)', x = '연도', y = '확진자수',
       color = 'alpha')

# 전체 학생수의 alpha 값에 따른 적합치와 예측값의 변화
autoplot(students.ts[,2], color = 'black') +
  autolayer(fitted(ses(students.ts[,2], alpha = 0.1)), series = '0.1') +
  autolayer(ses(students.ts[,2], alpha = 0.1, PI = FALSE), series = '0.1') +
  autolayer(fitted(ses(students.ts[,2], alpha = 0.2)), series = '0.2') +
  autolayer(ses(students.ts[,2], alpha = 0.2, PI = FALSE), series = '0.2') +
  autolayer(fitted(ses(students.ts[,2], alpha = 0.3)), series = '0.3') +
  autolayer(ses(students.ts[,2], alpha = 0.3, PI = FALSE), series = '0.3') +
  autolayer(fitted(ses(students.ts[,2], alpha = 0.4)), series = '0.4') +
  autolayer(ses(students.ts[,2], alpha = 0.4, PI = FALSE), series = '0.4') +
  autolayer(fitted(ses(students.ts[,2], alpha = 0.5)), series = '0.5') +
  autolayer(ses(students.ts[,2], alpha = 0.5, PI = FALSE), series = '0.5') +
  autolayer(fitted(ses(students.ts[,2], alpha = 0.6)), series = '0.6') +
  autolayer(ses(students.ts[,2], alpha = 0.6, PI = FALSE), series = '0.6') +
  autolayer(fitted(ses(students.ts[,2], alpha = 0.7)), series = '0.7') +
  autolayer(ses(students.ts[,2], alpha = 0.7, PI = FALSE), series = '0.7') +
  autolayer(fitted(ses(students.ts[,2], alpha = 0.8)), series = '0.8') +
  autolayer(ses(students.ts[,2], alpha = 0.8, PI = FALSE), series = '0.8') +
  autolayer(fitted(ses(students.ts[,2], alpha = 0.9)), series = '0.9') +
  autolayer(ses(students.ts[,2], alpha = 0.9, PI = FALSE), series = '0.9') +
  labs(title = 'alpha값에 따른 단순 지수 평활 모델', x = '연도', y = '학생수', color = 'alpha')

# 전체 학생수에 대한 홀트 모델링
summary(holt(students.ts[,2]))

# 전체 학생수에 대한 홀트 모델링
autoplot(students.ts[,2]) +
  autolayer(fitted(holt(students.ts[,2])), series = '적합값') +
  autolayer(holt(students.ts[,2]), series = '예측값') +
  labs(title = '전체 학생수에 대한 홀트 지수 평활 모델', x = '연도', y = '학생수')

# 전체 취업자수에 대한 Holt modeling
autoplot(employees.ts[,2]) +
  autolayer(fitted(holt(employees.ts[,2])), series = '적합값') +
  autolayer(holt(employees.ts[,2]), series = '예측값') +
  labs(title = '신규 취업자수에 대한 홀트 지수 평활 모델', x = '연도', y = '취업자수')

# 코로나 신규확진자수(0-9세)에 대한 홀트 모델링
autoplot(covid19.ts[,2]) +
  autolayer(fitted(holt(covid19.ts[,2])), series = '적합값') +
  autolayer(holt(covid19.ts[,2], h = 30), series = '예측값') +
  labs(title = '코로나 확진자수(0-9세)에 대한 홀트 지수 평활 모델', x = '연도', y = '확진자수')

# 전체 학생수에 대한 홀트 모델링 비교
autoplot(students.ts[,2]) +
  autolayer(fitted(holt(students.ts[,2])), series = '홀트 적합') +
  autolayer(fitted(holt(students.ts[,2], damped = TRUE)), series = ' 감쇄 적합') +
  autolayer(holt(students.ts[,2]), series = '홀트 예측', PI = FALSE) +
  autolayer(holt(students.ts[,2], damped = TRUE), series = '감쇄 예측', PI = FALSE) +
  labs(title = '전체 학생수에 대한 감쇄 홀트 지수 평활 모델', x = '연도', y = '학생수')

# 전체 취업자수에 대한 홀트 모델링 비교
autoplot(employees.ts[,2]) +
  autolayer(fitted(holt(employees.ts[,2])), series = '홀트 적합') +
  autolayer(fitted(holt(employees.ts[,2], damped = TRUE)), series = '감쇄 적합') +
  autolayer(holt(employees.ts[,2]), series = '홀트 예측', PI = FALSE) +
  autolayer(holt(employees.ts[,2], damped = TRUE), series = '감쇄 예측', PI = FALSE) +
  labs(title = '신규 취업자수 감쇄 홀트 지수 평활 모델', x = '연도', y = '취업자수')

# 코로나 신규확진자수(0-9세)에 대한 홀트 모델링 비교
autoplot(covid19.ts[,2]) +
  autolayer(fitted(holt(covid19.ts[,2])), series = '홀트 적합') +
  autolayer(fitted(holt(covid19.ts[,2], damped = TRUE)), series = '감쇄 적합') +
  autolayer(holt(covid19.ts[,2], h = 30), series = '홀트 예측', PI = FALSE) +
  autolayer(holt(covid19.ts[,2], h = 30, damped = TRUE), series = '감쇄 예측', PI = FALSE) +
  labs(title = '코로나 확진자수(0-9세) 감쇄 홀트 지수 평활 모델', x = '연도', y = '확진자수')

autoplot(students.ts[,2]) +
  autolayer(fitted(holt(students.ts[,2], beta = 0.1)), series = '0.1') +
  autolayer(holt(students.ts[,2], beta = 0.1, PI = F), series = '0.1') +
  autolayer(fitted(holt(students.ts[,2], beta = 0.2)), series = '0.2')+
  autolayer(holt(students.ts[,2], beta = 0.2, PI = F), series = '0.2') +
  autolayer(fitted(holt(students.ts[,2], beta = 0.3)), series = '0.3')+
  autolayer(holt(students.ts[,2], beta = 0.3, PI = F), series = '0.3') +
  autolayer(fitted(holt(students.ts[,2], beta = 0.4)), series = '0.4')+
  autolayer(holt(students.ts[,2], beta = 0.4, PI = F), series = '0.4') +
  autolayer(fitted(holt(students.ts[,2], beta = 0.5)), series = '0.5')+
  autolayer(holt(students.ts[,2], beta = 0.5, PI = F), series = '0.5') +
  autolayer(fitted(holt(students.ts[,2], beta = 0.6)), series = '0.6')+
  autolayer(holt(students.ts[,2], beta = 0.6, PI = F), series = '0.6') +
  autolayer(fitted(holt(students.ts[,2], beta = 0.7)), series = '0.7')+
  autolayer(holt(students.ts[,2], beta = 0.7, PI = F), series = '0.7') +
  autolayer(fitted(holt(students.ts[,2], beta = 0.8)), series = '0.8')+
  autolayer(holt(students.ts[,2], beta = 0.8, PI = F), series = '0.8') +
  autolayer(fitted(holt(students.ts[,2], beta = 0.9)), series = '0.9')+
  autolayer(holt(students.ts[,2], beta = 0.9, PI = F), series = '0.9') +
  labs(title = '추세 기울기(beta) 값에 따른 홀트 지수 평활 모델 변화', x = '연도', y = '학생수',
       color = 'beta')

## 에러나는게 정상입니다.
hw(students.ts[,2])

autoplot(employees.ts[,2]) +
  autolayer(fitted(hw(employees.ts[,2])), series = '홀트 윈터 적합값') +
  autolayer(hw(employees.ts[,2], seasonal = 'additive'), PI = FALSE, series = '덧셈방법') +
  autolayer(hw(employees.ts[,2], seasonal = 'multiplicative'), PI = FALSE, series = '곱셈방법') +
  labs(title = '신규 취업자수에 대한 홀트 윈터 지수 평활 모델', x = '연도', y = '취업자수')

# 총학생수에 대한 ets 모델
ets(students.ts[,2]) %>% summary

# 전체 학생수에 대한 ets 모델 ploting
ets(students.ts[,2]) %>% autoplot()

# 전체 학생수에 대한 예측값 ploting
ets(students.ts[,2]) %>% forecast() %>%
  autoplot() +
  labs(title = '전체 학생수에 대한 ets(A, Ad, N) 모델 예측 결과', x = '연도', y = '학생수')

# ETS(M,Ad,A)로 모델 선정
ets(employees.ts[,2])

ets(employees.ts[,2]) %>% autoplot()

ets(employees.ts[,2]) %>% forecast() %>%
  autoplot() +
  labs(title = '신규 취업자에 대한 ets(M, Ad, A) 예측 결과', x = '연도', y = '취업자수')


###  6.7 ARIMA 모델

library(tseries)

set.seed(345)

arima100 <- arima.sim(model = list(order = c(1, 0, 0), ar = 0.9), n = 200)

arima100 %>% autoplot(main = 'AR(1) 모델')

urca::ur.kpss(arima100) %>% urca::summary()

ndiffs(arima100, test = 'kpss')

set.seed(345)

arima110 <- arima.sim(model = list(order = c(1, 1, 0), ar = 0.9), n = 200)

arima110 %>% autoplot(main = 'AR(1), 차분 1 모델')

urca::ur.kpss(arima110) %>% urca::summary()

ndiffs(arima110, test = 'kpss')

urca::ur.kpss(diff(arima110)) %>% urca::summary()

ndiffs(diff(arima110), test = 'kpss')

arima100 %>% ggtsdisplay()

set.seed(345)

# ARIMA(0,0,1)에 MA(1)의 회귀 계수가 0.9인 데이터 200개 생성
arima001 <- arima.sim(model = list(order = c(0, 0, 1), ma = 0.9), n = 200)

arima001 %>% autoplot(main = 'MA(1) 모델')

# kpss 테스트를 통해 생성된 데이터가 정상성인지 테스트 - 0.05보다 크므로 정상성, 차분 불필요
urca::ur.kpss(arima001) %>% urca::summary()

# 비정상 제거를 위한 차분수 - 0이 나오므로 차분 불필요
ndiffs(arima001, test = 'kpss')

set.seed(345)

arima011 <- arima.sim(model = list(order = c(0, 1, 1), ma = 0.9), n = 200)

arima011 %>% autoplot(main = 'MA(1), 차분 1 모델')

# kpss 테스트를 통해 생성된 데이터가 정상성인지 테스트 - 0.05보다 작으므로 정상성, 차분 필요
urca::ur.kpss(arima011) %>% urca::summary()

# 비정상성을 제거하기 위해 필요한 차분수
ndiffs(arima011, test = 'kpss')

urca::ur.kpss(diff(arima011)) %>% urca::summary()

ndiffs(diff(arima011), test = 'kpss')

arima001 %>% ggtsdisplay()

set.seed(345)

# ARIMA(1,0,0)에 AR(1)의 회귀 계수가 0.9인 데이터 200개 생성
arima101 <- arima.sim(model = list(order = c(1, 0, 1), ar = 0.9, ma = 0.9), n = 200)

arima101 %>% autoplot(main = 'AR(1), MA(1) 모델')

arima101 %>% ggtsdisplay()

library(astsa)

sarima(arima101, p = 1, d = 0, q = 1)

students.ts[,2] %>% ggtsdisplay()

urca::ur.kpss(students.ts[,2]) %>% urca::summary()

ndiffs(students.ts[,2], test = 'kpss')

sarima(students.ts[,2], p = 1, d = 2, q = 0)

Arima(students.ts[,2], order=c(1,2,0))

Arima(students.ts[,2], order=c(1,2,0)) %>% forecast() %>%
  autoplot() +
  labs(title = '전체 학생수에 대한 ARIMA(1, 2, 0) 모델 예측 결과', x = '연도', y = '학생수')

sarima(students.ts[,2], p = 1, d = 2, q = 0)

auto.arima(students.ts[,2])

auto.arima(students.ts[,2]) %>% forecast() %>%
  autoplot() +
  labs(title = '전체 학생수에 대한 ARIMA(1, 2, 0) 모델 예측 결과', x = '연도', y = '학생수')

urca::ur.kpss(covid19.ts[,2]) %>% urca::summary()

# 비정상성을 제거하기 위해 필요한 차분수가 1
ndiffs(covid19.ts[,2], test = 'kpss')

# ACF, PACF 모두 절단(cut off)이므로 ARMA(p, q) 모델
diff(covid19.ts[,2]) %>% ggtsdisplay()

# 0-9세 코로나 확진자수의 ARIMA 모형은 ARIMA(2, 1, 1)으로 선정됨
auto.arima(covid19.ts[,2])

sarima(covid19.ts[,2], 2, 1, 1)


# 전체 취업자수 데이터를 분기별 합계 데이터로 변환
employees %>% mutate(year = lubridate::year(time),
                     qtr = lubridate::quarter(time)) %>%
  group_by(year, qtr) %>%
  summarise(sum = sum(total)) %>%
  ts(frequency = 4, start = c(2013,1)) -> qtr.employees.ts

# auto.arima로 일단 ARIMA 모형을 검토 - ARIMA(0,1,0)(0,1,0)[4]로 제안됨
auto.arima(qtr.employees.ts[,3]) %>% summary()

# ggtsdisplay()로 ACF 플롯을 볼 때 4주기마다 계절성이 있는 듯 보임
qtr.employees.ts[,3] %>% tsdisplay()

# KPSS 검정 결과 단위근이 존재하는 비정상성 데이터
qtr.employees.ts[,3] %>% urca::ur.kpss() %>% urca::summary()

# ndiffs()에 의하면 1차 차분 필요
qtr.employees.ts[,3] %>% ndiffs()

# 1차 차분 결과 plot()
qtr.employees.ts[,3] %>% diff() %>% tsdisplay(lag.max = 36)

# 잔차의 분포, ACF, Q-Q 플롯, Ljung-box test 모두 백색잡음으로 나타남
sarima(qtr.employees.ts[,3], p = 0, d = 1, q = 0, P = 1, D = 1, Q = 0, S = 4)

auto.arima(qtr.employees.ts[,3])

sarima(qtr.employees.ts[,3], p = 0, d = 1, q = 0, P = 0, D = 1, Q = 0, S = 4)

# 신규취업자수에 대한 수작업 모델과 auto.arima모델을 생성
arima010110 <- Arima(qtr.employees.ts[,3], order = c(0,1,0), seasonal = c(1,1,0))

arima010010 <- Arima(qtr.employees.ts[,3], order = c(0,1,0), seasonal = c(0,1,0))

# 각 모델의 회귀 계수 및 다양한 정보
summary(arima010110)

summary(arima010010)

# 각 모델의 예측값 산출
forecast010110 <- arima010110 %>% forecast()

forecast010010 <- arima010010 %>% forecast()

# 각 모델의 예측 결과 플롯
autoplot(qtr.employees.ts[,3]) +
  autolayer(forecast010110, PI = F, series = '010110') +
  autolayer(forecast010010, PI = F, series = '010010') +
  labs(title = '분기별 취업자수에 대한 ARIMA(0,1,0)(0,1,0)[4]와 ARIMA(0,1,0)(1,1,0)[4] 예측 결과',
       x = '연도', y = '취업자수', color = '모델')

# auto.arima()는 ARIMA(0,1,0)(0,1,1)[12] 모델 제안
employees.ts[,2] %>% auto.arima()

# ggtsdisplay()로 ACF 플롯을 볼 때 12주기마다 계절성이 있는 듯 보임
employees.ts[,2] %>% tsdisplay()

# KPSS 검정 결과 단위근이 존재하는 비정상성 데이터
employees.ts[,2] %>% urca::ur.kpss() %>% urca::summary()

# ndiffs()에 의하면 1차 차분 필요
employees.ts[,2] %>% ndiffs()

# 1차 차분 결과 plot()
# ACF, PACF를 확인해서 비계절성 모델은 (0,1,1), 계절성 모델은 (1,1,0)으로 결정
employees.ts[,2] %>% diff() %>% tsdisplay(lag.max = 36)

# ARIMA(0,1,1)(1,1,0)[12]모델 생성
arima011110 <- employees.ts[,2] %>% Arima(order = c(0,1,1), seasonal = c(1,1,0))

# auto.arima()가 제안한 ARIMA(0,1,0)(0,1,1)[12]모델 생성
arima010011 <- employees.ts[,2] %>% Arima(order = c(0,1,0), seasonal = c(0,1,1))

# 각 모델의 정보 확인
summary(arima011110)

summary(arima010011)

# 두 모델의 예측값 산출
forecast011110 <- arima011110 %>% forecast()

forecast010011 <- arima010011 %>% forecast()

# 두 모델의 플롯
autoplot(employees.ts[,2]) +
  autolayer(forecast011110, PI = F, series = '011110') +
  autolayer(forecast010011, PI = F, series = '010011') +
  labs(title = '월별 취업자수에 대한 ARIMA(0,1,1)(1,1,0)[12]와 ARIMA(0,1,0)(0,1,1)[12]', x =
         '연도', y = '취업자수', color = '모델')

autoplot(employees.ts[, 2]) +
  autolayer(forecast010011, PI = F, series = '010011') +
  autolayer(forecast(Arima(employees.ts[, 2], order = c(1,1,0), seasonal = c(0,1,1))),
            PI = F, series = '110011') +
  autolayer(forecast(Arima(employees.ts[, 2], order = c(0,1,1), seasonal = c(0,1,1))),
            PI = F, series = '011011') +
  autolayer(forecast(Arima(employees.ts[, 2], order = c(1,1,1), seasonal = c(0,1,1))),
            PI = F, series = '111011') +
  autolayer(forecast(Arima(employees.ts[, 2], order = c(1,1,1), seasonal = c(1,1,1))),
            PI = F, series = '111111') +
  labs(title = '월별 신규 취업자에 대한 ARIMA 모델 비교', x = '연도', y = '취업자수')


###  6.8 TBATS 모델

# 코로나 확진자 데이터에 대한 tBats 모델
covid19.ts[,2] %>% tbats() %>% forecast() %>% autoplot() + labs(title = '코로나 확진자(0-9세)에
대한 TBATS 모델 예측', x = '시간', y = '확진자수')


###  6.9 prophet 모델

if(!require(prophet)) {
  install.packages('prophet')
  library(prophet)
}
library(prophet)
students.prophet <- data.frame(ds = students$연도, y = students$학생수계)

model.prophet.students <- prophet::prophet(students.prophet)

future.students <- make_future_dataframe(model.prophet.students, periods = 10, freq = 'year')

forecast.students <- predict(model.prophet.students, future.students)

plot(model.prophet.students, forecast.students) +
  ggrepel::geom_text_repel(aes(label = scales::number(y, big.mark = ',', accuracy = 1)),
                           vjust = 1, size = 3) +
  labs(title = '전체 학생수에 대한 prophet 모델 예측 결과', x = '연도', y = '학생수') +
  scale_y_continuous(labels = scales::number_format(big.mark = ','))

prophet_plot_components(model.prophet.students, forecast.students)

employees.prophet <- data.frame(ds = employees[,1], y = employees[,2])

model.prophet.employees <- prophet::prophet(employees.prophet)

future.employees <- make_future_dataframe(model.prophet.employees, periods = 10, freq = 'month')

forecast.employees <- predict(model.prophet.employees, future.employees)

plot(model.prophet.employees, forecast.employees) +
  labs(title = '월별 전체 취업자수에 대한 prophet 모델 예측 결과', x = '연월', y = '취업자수') +
  scale_y_continuous(labels = scales::number_format(big.mark = ','))

prophet_plot_components(model.prophet.employees, forecast.employees)

covid.prophet <- data.frame(ds = covid19$date, y = covid19$`0-9세`)

model.prophet.covid <- prophet::prophet(covid.prophet, yearly.seasonality=TRUE, daily.seasonality=TRUE, weekly.seasonality=TRUE)

future.covid <- make_future_dataframe(model.prophet.covid, periods = 100, freq = 'day')

forecast.covid <- predict(model.prophet.covid, future.covid)

plot(model.prophet.covid, forecast.covid) +
  labs(title = '일별 코로나 확진자수 대한 prophet 모델 예측 결과(0-9세)', x = '연월', y = '확진자수') +
  scale_y_continuous(labels = scales::number_format(big.mark = ','))

prophet_plot_components(model.prophet.covid, forecast.covid)


###  6.10 신경망 모델

# 학생수에 대한 NNAR 모델은 NNAR(1, 1)모델
students.ts[,2] %>% nnetar() %>% forecast(PI = TRUE) %>% autoplot() + labs(title = '전체 학생수에 대한 신경망 모델 예측 결과', x = '연도', y = '학생수')

# 전체 취업자수에 대한 모델은 NNAR(1, 1, 2)[12] 모델
employees.ts[, 2] %>% nnetar() %>% forecast(PI = TRUE) %>% autoplot() + labs(title = '신규 취업자수에 대한 신경망 모델 예측 결과', x = '연도', y = '취업자수')

# 코로나 확진자수에 대한 모델은 NNAR(22, 12) 모델(예측값을 위해 22개의 과거 데이터를 활용했고 히든 레이어에 12개의 신경세포를 생성)
covid19.ts[,2] %>% nnetar() %>% forecast(h = 100, PI = TRUE) %>% autoplot() + labs(title = '코로나 확진자(0-9세)에 대한 신경망 모델 예측 결과', x = '연도', y = '확진자수')

######################################
###  7장 시계열 forecasting Part III - 시계열 분석 프레임워크

###  7.2 fable 프레임워크
###  7.2.1 미래 학생수 예측

split <- floor(nrow(students.tsibble) * 0.8)

students.tsibble.tr <- students.tsibble[1:split, ]

students.tsibble.test <- students.tsibble[(split+1):nrow(students.tsibble), ]

library(fable)

library(fable.prophet)

model.fable.students <- model(students.tsibble.tr,
                              ets = ETS(학생수계),
                              arima = ARIMA(학생수계),
                              naive = NAIVE(학생수계),
                              tslm = TSLM(학생수계 ~ trend()),
                              rw = RW(학생수계),
                              mean = MEAN(학생수계),
                              nnetar = NNETAR(학생수계),
                              prophet = fable.prophet::prophet(학생수계)
)

forecast.fable.students <- forecast(model.fable.students, h = 10)

autoplot(forecast.fable.students, students.tsibble, level = NULL)

accuracy(forecast.fable.students, students.tsibble.test) %>%
  arrange(RMSE)

best.model.fable.students <- model.fable.students %>%
  select(ets, prophet)

best.model.fable.students %>% forecast(h = 10) %>%
  autoplot(students.tsibble, alpha = 0.6, level = NULL) +
  autolayer(fitted(best.model.fable.students))

### 7.2.2 미래 취업자수 예측

employees$yearmonth <- yearmonth(employees$time)

employees.tsibble <- as_tsibble(employees, index = yearmonth)

split <- floor(nrow(employees.tsibble) * 0.9)

n <- nrow(employees.tsibble)

employees.tsibble.tr <- employees.tsibble[1:split, ]

employees.tsibble.test <- employees.tsibble[(split+1):n, ]

model.fable.employees <- employees.tsibble.tr %>%
  model(ets = ETS(total),
        arima = ARIMA(total),
        naive = NAIVE(total),
        tslm = TSLM(total~trend() + season(12)),
        rw = RW(total),
        mean = MEAN(total),
        nnetar = NNETAR(total),
        prophet = fable.prophet::prophet(total)
  )

forecast.fable.employees <- model.fable.employees %>% forecast(h = 24)

forecast.fable.employees %>%
  autoplot(employees.tsibble, level = NULL) +
  labs(title = 'fable로 생성한 8가지 모델 예측 플롯', x = '연월', y = '취업자수')

forecast.fable.employees %>% forecast::accuracy(employees.tsibble.test) %>% arrange(RMSE)

best.model.fable.employees <- model.fable.employees %>%
  select(naive, rw)

best.model.fable.employees %>%
  forecast(h = 12) %>%
  autoplot(employees.tsibble, level = NULL, lwd = 1) +
  autolayer(fitted(best.model.fable.employees), lwd = 1) +
  geom_point(aes(x = yearmonth, y = total)) +
  labs(title = '전체 취업자수 예측 모델', x = '연월', y = '취업자수')

### 7.2.3 미래 코로나 확진자수 예측

fill.covid19.tsibble <- fill_gaps(covid19.tsibble, `0-9세` = 0)

split <- floor(nrow(fill.covid19.tsibble) * 0.9)

n <- nrow(fill.covid19.tsibble)

fill.covid19.tsibble.tr <- fill.covid19.tsibble[1:split, ]

fill.covid19.tsibble.test <- fill.covid19.tsibble[(split+1):n, ]

model.covid19.tsibble <- fill.covid19.tsibble.tr %>%
  model(ets = ETS(`0-9세`),
        arima = ARIMA(`0-9세`),
        naive = NAIVE(`0-9세`),
        tslm = TSLM(`0-9세`),
        rw = RW(`0-9세`),
        mean = MEAN(`0-9세`),
        nnetar = NNETAR(`0-9세`),
        prophet = prophet(`0-9세`)
  )

forecast.covid19.tsibble <- model.covid19.tsibble %>%
  forecast(h = 120)

forecast.covid19.tsibble %>% autoplot(fill.covid19.tsibble, level = NULL) + labs(title = '코로나 확진자(0-9세)에 대한 8가지 모델 예측 결과', x = '날짜', y = '확진자수')

forecast.covid19.tsibble %>% forecast::accuracy(fill.covid19.tsibble.test) %>% arrange(RMSE)

best.model.covid19.tsibble <- model.covid19.tsibble %>% select(prophet)

best.model.covid19.tsibble %>%
  forecast(h = 120) %>%
  autoplot(fill.covid19.tsibble, lwd = 1, alpha = 0.6) +
  autolayer(fitted(best.model.covid19.tsibble), lwd = 1) +
  geom_point(aes(x = date, y = `0-9세`)) +
  labs(title = '코로나 확진자수(0-9세) 예측', x = '연월일', y = '확진자수')


###  7.3 modeltime 프레임워크
###  7.3.1 미래 학생수 예측

library(modeltime)

library(tidymodels)

splits.students <- initial_time_split(students, prop = 0.8)

model_fit_arima <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(학생수계 ~ 연도, data = training(splits.students))

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(학생수계 ~ 연도, data = training(splits.students))

model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(학생수계 ~ 연도, data = training(splits.students))

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(학생수계 ~ 연도,
      data = training(splits.students))

model_fit_nnetar <- nnetar_reg() %>%
  set_engine("nnetar") %>%
  fit(학생수계 ~ 연도, data = training(splits.students))

model_fit_tbats <- seasonal_reg() %>%
  set_engine("tbats") %>%
  fit(학생수계 ~ 연도, data = training(splits.students))

(models_tbl <- modeltime_table(
  model_fit_arima,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  model_fit_nnetar,
  model_fit_tbats))

(calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits.students)))

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits.students),
    actual_data = students
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show = FALSE
  ) +
  labs(title = 'modeltime을 사용한 전체 학생수 6개 모델 예측 결과', x = '연도', y = '학생수')

calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse)

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(학생수계 ~ 연도, data = students)

model_fit_tbats <- seasonal_reg() %>%
  set_engine("tbats") %>%
  fit(학생수계 ~ 연도, data = students)

(models_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_tbats))

models_tbl %>%
  modeltime_forecast(
    h = '10 years',
    actual_data = students
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE
  )

###  7.3.2 미래 취업자수 예측

splits.employees <- initial_time_split(employees, prop = 0.9)

model_fit_arima <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(total ~ time, data = training(splits.employees))

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(total ~ time, data = training(splits.employees))

model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(total ~ time, data = training(splits.employees))

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(total ~ time + factor(lubridate::month(time, label = TRUE), ordered = FALSE),
      data = training(splits.employees))

model_fit_nnetar <- nnetar_reg() %>%
  set_engine("nnetar") %>%
  fit(total ~ time, data = training(splits.employees))

model_fit_tbats <- seasonal_reg() %>%
  set_engine("tbats") %>%
  fit(total ~ time, data = training(splits.employees))

(models_tbl <- modeltime_table(
  model_fit_arima,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  model_fit_nnetar,
  model_fit_tbats))

(calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits.employees)))

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits.employees),
    actual_data = employees
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show = FALSE
  ) + labs(title = 'modeltime을 사용한 신규 취업자수 6개 모델 예측 결과', x = '연도', y = '취업자수')

calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse)

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(total ~ time + factor(lubridate::month(time, label = TRUE), ordered = FALSE),
      data = employees)

model_fit_nnetar <- nnetar_reg() %>%
  set_engine("nnetar") %>%
  fit(total ~ time, data = employees)

(models_tbl <- modeltime_table(
  model_fit_lm,
  model_fit_nnetar))

models_tbl %>%
  modeltime_forecast(
    h = '3 years',
    actual_data = employees
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE
  ) + labs(title = '신규 취업자수 모델 예측 결과', x = '연도', y = '취업자수')

###  7.3.3 미래 코로나 확진자수 예측

splits.covid19 <- initial_time_split(covid19, prop = 0.9)

model_fit_arima <- arima_reg(seasonal_period = 365) %>%
  set_engine(engine = "auto_arima") %>%
  fit(`0-9세` ~ date, data = training(splits.covid19))

model_fit_ets <- exp_smoothing(seasonal_period = 365) %>%
  set_engine(engine = "ets") %>%
  fit(`0-9세` ~ date, data = training(splits.covid19))

model_fit_prophet <- prophet_reg(seasonality_daily = TRUE, seasonality_weekly = TRUE, ) %>%
  set_engine(engine = "prophet") %>%
  fit(`0-9세` ~ date, data = training(splits.covid19))

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(`0-9세` ~ date,
      data = training(splits.covid19))

model_fit_nnetar <- nnetar_reg() %>%
  set_engine("nnetar") %>%
  fit(`0-9세` ~ date, data = training(splits.covid19))

model_fit_tbats <- seasonal_reg() %>%
  set_engine("tbats") %>%
  fit(`0-9세` ~ date, data = training(splits.covid19))


(models_tbl <- modeltime_table(
  model_fit_arima,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  model_fit_nnetar,
  model_fit_tbats))

(calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits.covid19)))

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits.covid19),
    actual_data = covid19
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show = FALSE
  ) + labs(title = 'modeltime을 사용한 코로나 확진자수(0-9세)에 대한 6개 모델 예측 결과', x = '연도',
           y = '확진자수')

calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse)

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(`0-9세` ~ date,
      data = covid19)

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(`0-9세` ~ date, data = covid19)

(models_tbl <- modeltime_table(
  model_fit_lm,
  model_fit_ets))

models_tbl %>%
  modeltime_forecast(
    h = '3 months',
    actual_data = covid19
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE
  ) + labs(title = '코로나 확진자수(0-9세) 모델 예측 결과', x = '날짜', y = '확진자수')


library(bsts)


students.ss <- AddLocalLinearTrend(list(), students.xts[, 1])
##students.ss <- AddSeasonal(students.ss, students.xts[, 1], nseasons = 1)
students.bayesian.model <- bsts(students.xts[, 1]~students.xts[, 2]+students.xts[, 3], data = students.xts, 
                                state.specification = students.ss,
                                niter = 1000)

plot(students.bayesian.model)
plot(students.bayesian.model, "components")  # plot(model1, "comp") works too!

students.bayesian.pred <- predict(students.bayesian.model, horizon = 12, newdata = students.xts)
plot(students.bayesian.pred)
plot(students.bayesian.model, burn = 100)
plot(students.bayesian.model, "residuals", burn = 100)
plot(students.bayesian.model, "components", burn = 100)
plot(students.bayesian.model, "forecast.distribution", burn = 100)



employees.ss <- AddLocalLinearTrend(list(), employees.xts[, 1])
employees.ss <- AddSeasonal(employees.ss, employees.xts[, 1], nseasons = 12)
employees.ss <- AddSeasonal(employees.ss, employees.xts[, 1], nseasons = 4)

employees.bayesian.model <- bsts(employees.xts[, 1],
                                 state.specification = employees.ss,
                                 niter = 1000)

plot(employees.bayesian.model)
plot(employees.bayesian.model, "components")  # plot(model1, "comp") works too!

employees.bayesian.pred <- predict(employees.bayesian.model, horizon = 12)
plot(employees.bayesian.pred)
plot(employees.bayesian.model, burn = 100)
plot(employees.bayesian.model, "residuals", burn = 100)
plot(employees.bayesian.model, "components", burn = 100)
plot(employees.bayesian.model, "forecast.distribution", burn = 100)

covid19.ss <- AddLocalLinearTrend(list(), covid19.xts[, 1])
covid19.ss <- AddSeasonal(covid19.ss, covid19.xts[, 1], nseasons = 52, season.duration = 7)
covid19.bayesian.model <- bsts(covid19.xts[, 1],
                               state.specification = covid19.ss,
                               niter = 100)
plot(covid19.xts[, 1])

plot(covid19.bayesian.model)
plot(covid19.bayesian.model, "components")  # plot(model1, "comp") works too!

covid19.bayesian.pred <- predict(covid19.bayesian.model, horizon = 30)
plot(pred1)
