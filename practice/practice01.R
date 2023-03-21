getwd()

# 데이터 불러오기
library(tidyverse)
library(readxl)

birth_df <- read_excel("practice/01_시군구_성_월별_출생.xlsx")

# 데이터 탐색

birth_df %>% dim()
View(birth_df)

colSums(is.na(birth_df))

birth_df$시점 %>% head()

is.na(birth_df$시점) %>% head()
!is.na(birth_df$시점) %>% head()

# NA 처리
birth_df %>%
  filter(!is.na(시점)) %>%
  head()

birth_df %>%
  filter(!is.na(시점)) %>%
  select(시점, 전국) %>%
  head()

# 컬럼 정리
birth_df <- birth_df %>%
  filter(!is.na(시점)) %>%
  select(시점, 전국) %>%
  separate(시점, into = c("년도", "월"))

birth_df %>% head()

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

birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국)) %>%
  qplot(x = 월, y = 평균출생수, geom = "col", data = .) +
  labs(title = "월별 신생아 출생 평균", subtitle = "1997년 1월 ~ 2021년 12월") +
  theme_bw(base_size = 15)

x <- c(1,2)
x[1:10]

