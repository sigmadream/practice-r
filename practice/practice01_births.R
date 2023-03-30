library(tidyverse)
library(readxl)

birth_df <- read_excel("data/2023_시군구_성_월별_출생.xlsx")

# 1. 데이터 확인
birth_df %>% 
  dim()

# 2. 전처리
## 2-1. 결측치(NA) 확인
colSums(is.na(birth_df))

birth_df$시점 %>% 
  head()

is.na(birth_df$시점) %>% 
  head()

!is.na(birth_df$시점) %>% 
  head()

## 2-2. 결측치(NA) 처리
birth_df %>%
  filter(!is.na(시점)) %>%
  head()

birth_df %>%
  filter(!is.na(시점)) %>%
  select(시점, 전국) %>%
  head()

## 2-3. 열 정리
birth_df <- birth_df %>%
  filter(!is.na(시점)) %>%
  select(시점, 전국) %>%
  separate(시점, into = c("년도", "월"))

birth_df %>% 
  head()

## 3. 데이터 정리
birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국))

birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국)) %>%
  arrange(평균출생수)
  
birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국)) %>%
  arrange(desc(평균출생수))

## 4. 시각화를 통한 데이터 확인
birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국)) %>%
  qplot(x = 월, y = 평균출생수, geom = "col", data = .) +
  labs(title = "월별 신생아 출생 평균", subtitle = "1997년 1월 ~ 2021년 12월") +
  theme_bw(base_size = 15)

# 추가문제) Q2.부산지역 출생아수는?
birth_df <- read_excel("data/2023_시군구_성_월별_출생.xlsx")
birth_df %>%
  filter(!is.na(시점)) %>%
  select(시점, 전국) %>%
  head()

busan_birth <- birth_df %>%
  fill(시점, .direction = 'downup') %>%
  select(시점, 항목, 부산광역시) %>%
  separate(시점, into = c("년도", "월")) %>%
  filter(항목 %in% "남자 (명)")

busan_birth %>%
  group_by(년도) %>%
  qplot(x = 년도, y = 부산광역시, geom = "col", data = .)
  