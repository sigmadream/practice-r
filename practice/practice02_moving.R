# https://data.seoul.go.kr/dataList/OA-15377/S/1/datasetView.do

install.packages('janitor')
install.packages('tidyverse')

library('tidyverse')

# 데이터 로드
moving_data <- read_csv("data/seoul_moving_202107_09_hr.csv")
reference_data <- readxl::read_excel("data/reference.xlsx")

moving_data %>% 
  names()

reference_data %>% 
  names()

# Q1. 시도 단위는 몇 개인가?

reference_data %>% 
  distinct(시도) %>% 
  count()

reference_data %>% 
  distinct(시도) %>% 
  dim()

reference_data %>% 
  distinct(시도, .keep_all = T)

# Q2. 나이대는 어떻게 되나요?

moving_data %>% 
  distinct(나이)

moving_data %>% 
  distinct(나이) %>% 
  count()

# Q3. 일요일에 움직이는 남성/여성은 몇명인가?

moving_data %>% 
  filter(요일 == "일" & 성별 == "F") %>% 
  count()

moving_data %>% 
  filter(요일 == "일" & 성별 == "M") %>% 
  count()

# 04. 화요일 이동한 데이터 중 이동시간이 30분 이상인 데이터?

moving_data %>% 
  filter(요일 == "화" & `평균 이동 시간(분)` >= 30)

# 05. 이동시간이 30분 이상인 데이터 중에서 중랑구(11010)와 성북구(11020)에서 출발한 데이터는?

moving_data %>% 
  filter(`평균 이동 시간(분)` >= 30, `출발 시군구 코드` == 11010, `출발 시군구 코드` == 11020) %>% 
  count()

moving_data %>% 
  filter(`평균 이동 시간(분)` >= 30, `출발 시군구 코드` == 11010 | `출발 시군구 코드` == 11020) %>% 
  count()

# 06. 10대, 20대는 몇명인가?

moving_data %>% 
  filter(between(나이, "10", "20")) %>% 
  count()

# 07. 나이의 상위 5개는?

moving_data %>% 
  mutate(나이 = as.numeric(나이)) %>% 
  distinct(나이) %>% 
  slice_max(나이, n = 5)

# 08. 도착시간과 평균 이동 시간을 출력하라, 단 평균 이동시간은 내림차순으로 

moving_data %>% 
  select(도착시간, `평균 이동 시간(분)`) %>% 
  arrange(도착시간, desc(`평균 이동 시간(분)`))

# 09. 새로운 년도 열 생성하기
library(magrittr)

moving_data %<>% 
  mutate(year = substr(대상연월, 1, 4) %>% 
           as.integer()) %>% 
  select(year, everything())

# 10. 시도 정도만 뽑아내기

reference_data %>% 
  mutate(시도이름 = str_split_fixed(`full name`, pattern = " ", 2)[ ,1]) %>% 
  select(시도이름) %>% 
  distinct()

# 11. `평균 이동 시간(분)`을 평균시간으로 변경

moving_data %>% 
  rename(평균시간 = `평균 이동 시간(분)`)
