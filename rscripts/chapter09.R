# Chapter 09(수정)

# 실습: SQLite3 데이터베이스 작성
# install.packages("RSQLite")
# install.packages("DBI")

# 단계 1: 데이터베이스 연결을 위한 패키지 로딩
library(tidyverse)
library(tidytext)
library(DBI)
library(RSQLite)

# 단계 2: DB에 입력할 데이터 
data("mtcars")
mtcars
rownames(mtcars)
mtcars$car_names <- rownames(mtcars)
rownames(mtcars) <- c()
head(mtcars)

# 단계 3: 데이터 베이스 연결
conn <- dbConnect(RSQLite::SQLite(), "output/testdb.db")

# 단계 4: 테이블 추가 및 확인
dbWriteTable(conn, "cars_data", mtcars)
dbListTables(conn)

# 실습: SQLite3 데이터 추가 및 확인
car <- c('Camaro', 'California', 'Mustang', 'Explorer')
make <- c('Chevrolet','Ferrari','Ford','Ford')
df1 <- data.frame(car,make)
df1

car <- c('Corolla', 'Lancer', 'Sportage', 'XE')
make <- c('Toyota','Mitsubishi','Kia','Jaguar')
df2 <- data.frame(car,make)
df2

dfList <- list(df1,df2)
dfList

for(k in 1:length(dfList)){
  dbWriteTable(conn,"Cars_and_Makes", dfList[[k]], append = TRUE)
}

dbListTables(conn)

# 실습: 데이터베이스로부터 레코드 검색
dbGetQuery(conn, "SELECT * FROM Cars_and_Makes")
dbGetQuery(conn, "SELECT * FROM cars_data LIMIT 10")
dbGetQuery(conn,"SELECT car_names, hp, cyl FROM cars_data
                 WHERE cyl = 8")
dbGetQuery(conn,"SELECT car_names, hp, cyl FROM cars_data
                 WHERE car_names LIKE 'M%' AND cyl IN (6,8)")
dbGetQuery(conn,"SELECT cyl, AVG(hp) AS 'average_hp', AVG(mpg) AS 'average_mpg' FROM cars_data
                 GROUP BY cyl
                 ORDER BY average_hp")
avg_HpCyl <- dbGetQuery(conn,"SELECT cyl, AVG(hp) AS 'average_hp' FROM cars_data
                 GROUP BY cyl
                 ORDER BY average_hp")
avg_HpCyl
class(avg_HpCyl)

# 실습: 매개변수를 사용한 검색
mpg <-  18
cyl <- 6
Result <- dbGetQuery(conn, "SELECT car_names, mpg, cyl 
                     FROM cars_data WHERE mpg >= ? AND cyl >= ?", params = c(mpg,cyl))
Result

assembleQuery <- function(conn, base, search_parameters){
  parameter_names <- names(search_parameters)
  partial_queries <- ""
  for(k in 1:length(parameter_names)){
    filter_k <- paste(parameter_names[k], " >= ? ")
    if(k > 1){
      filter_k <- paste("AND ", parameter_names[k], " >= ?")
    }
    partial_queries <- paste(partial_queries, filter_k)
  }
  final_paste <- paste(base, " WHERE", partial_queries)
  print(final_paste)
  values <- unlist(search_parameters, use.names = FALSE)
  result <- dbGetQuery(conn, final_paste, params = values)
  return(result)
}

base <- "SELECT car_names, mpg, hp, wt FROM cars_data"
search_parameters <- list("mpg" = 16, "hp" = 150, "wt" = 2.1)
result <- assembleQuery(conn, base, search_parameters)
result

dbGetQuery(conn, "SELECT * FROM cars_data LIMIT 10")
dbExecute(conn, "DELETE FROM cars_data WHERE car_names = 'Mazda RX4'")
dbGetQuery(conn, "SELECT * FROM cars_data LIMIT 10")
dbExecute(conn, "INSERT INTO cars_data VALUES (21.0,6,160.0,110,3.90,2.620,16.46,0,1,4,4,'Mazda RX4')")
dbGetQuery(conn, "SELECT * FROM cars_data")
dbDisconnect(conn)


# 실습: 형태소 분석을 위한 한국어 NLP 패키지 설치
#install.packages("remotes")
#remotes::install_github("bit2r/bitTA")
#bitTA::install_mecab_ko()
#install.packages("RcppMeCab")
library(bitTA)
library(tidyverse)
library(tidytext)

# 실습: 한국어 띄어쓰기
get_spacing("최근음성인식정확도가높아짐에따라많은음성데이터가텍스트로변환되고분석되기시작했는데,이를위해잘동작하는띄어쓰기엔진은거의필수적인게되어버렸다")
str <- "글쓰기에서맞춤법과띄어쓰기를올바르게하는것은좋은글이될수있는요건중하나이다.하지만요즘학생들은부족한어문규정지식으로인해맞춤법과띄어쓰기에서많은오류를범하기도한다.본연구는그중띄어쓰기가글을인식하는데중요한역할을하는것으로판단하여,대학생들이띄어쓰기에대해서어느정도정확하게인식하고있는지,실제오류실태는어떠한지에대해살펴서그오류를개선할수있는교육방안을마련할필요가있다고판단하였다."
get_spacing(str)

# 실습: 형태소 분석
## 형태소 관련은 http://kkma.snu.ac.kr/을 참고
## https://r2bit.com/bitTA/articles/morphology.html
docs <- c("님은 갔습니다. 아아, 사랑하는 나의 님은 갔습니다.",
          "푸른 산빛을 깨치고 단풍나무 숲을 향하여 난 작은 길을 걸어서, 차마 떨치고 갔습니다.")
morpho_mecab(docs,  type = "morpheme")
morpho_mecab(docs)
morpho_mecab(docs, indiv = FALSE)

# 실습: 워드 클라우드
#install.packages("wordcloud2")
president_speech$doc[1:100] %>% 
  morpho_mecab(indiv = FALSE) %>% 
  table() %>% 
  wordcloud2::wordcloud2()


# 실습: 감성분석
url_v <- "https://github.com/park1200656/KnuSentiLex/archive/refs/heads/master.zip"
dest_v <- "data/knusenti.zip"
download.file(url = url_v, 
              destfile = dest_v,
              mode = "wb")
unzip("data/knusenti.zip", exdir = "data")
list.files("data/KnuSentiLex-master/")

senti_file_list <- list.files("data/KnuSentiLex-master/",
                              full.names = TRUE)
read_lines(senti_file_list[9]) %>% 
  head(10)

read_lines(senti_file_list[9]) %>% 
  head(10) %>% 
  str_extract("\t|\n| ")

read_tsv(senti_file_list[9]) %>% 
  head(10)

read_tsv(senti_file_list[9], col_names = FALSE) %>% 
  head(10)

senti_dic_df <- read_tsv(senti_file_list[9], col_names = FALSE)

glimpse(senti_dic_df)

senti_dic_df %>% 
  slice(1:5)

senti_dic_df <- senti_dic_df %>% 
  rename(word = X1, sScore = X2)

glimpse(senti_dic_df)

senti_dic_df %>% filter(sScore == 2) %>% arrange(word)
senti_dic_df %>% filter(sScore == -2) %>% arrange(word)

knu_dic_df <- senti_dic_df %>% 
  mutate(word   = ifelse( is.na(sScore), "갈등", word),
         sScore = ifelse( is.na(sScore), -1, sScore) )

knu_dic_df %>%   
  count(sScore)

knu_dic_df %>% 
  mutate(emotion = case_when( sScore >= 1 ~ "positive",
                              sScore <= -1 ~ "negative", 
                              TRUE         ~ "neutral")) %>% 
  count(emotion)

# 실습: 소설을 활용한 감정 분석
# install.packages("epubr")
# 단계1: 데이터 불러오기
library(epubr)
epub("data/jikji.epub") %>% glimpse()

jikji_epub <- epub("data/jikji.epub")
jikji_epub %>% 
  select(data) %>% 
  unnest(data) %>% 
  arrange( desc(nchar) )
novel_raw <- jikji_epub %>% 
  select(data) %>% 
  unnest(data) %>% 
  select(text) 
novel_raw

# 단계2: 전처리
novel_tbl <- novel_raw %>% 
  transmute(제목 = str_extract(text, ".*\\b"),
            지은이 = str_extract(text, "지은이.*\\b") %>% str_remove("지은이: "),
            출전 = str_extract(text, "출전.*\\b") %>% 
              str_remove(":") %>% str_remove("\\)") %>% str_remove("출전 "),
            본문 = str_squish(text) %>% 
              str_extract("본문.*") %>%
              str_remove("본문|:"))

novel_tbl

sosul_df <- novel_tbl %>% 
  rename(title = 제목, 
         author = 지은이,
         source = 출전,
         main = 본문) %>%
  slice(1:89)

sosul_df %>% glimpse()

sosul2_df <- sosul_df %>% 
  slice(c(9:10))

sosul2_df

# 단계3: 데이터 확인
sosul2_senti_df <- sosul2_df %>% 
  unnest_tokens(word, main) %>% 
  inner_join(knu_dic_df) %>% 
  group_by(author) %>% 
  count(word, sScore, sort = TRUE) %>% 
  filter(str_length(word) > 1) %>% 
  mutate(word = reorder(word, n)) %>% 
  slice_head(n = 20) %>% 
  mutate(sScore = as.factor(sScore))

sosul2_senti_df

sosul2_senti_df %>% 
  ggplot(aes(n, reorder_within(word, n, author), fill = sScore)) + 
  geom_col() +
  scale_y_reordered() +
  scale_fill_brewer(type="div", palette = "RdBu") +
  facet_wrap( ~ author, scales = "free")

sosul2_senti_tk <- sosul2_df %>% 
  unnest_tokens(word, main, token = RcppMeCab::pos) %>% 
  separate(col = word, 
           into = c("word", "morph"), 
           sep = "/" ) %>% 
  inner_join(knu_dic_df) %>% 
  group_by(author) %>% 
  count(word, sScore, sort = T) %>% 
  filter(str_length(word) > 1) %>% 
  mutate(word = reorder(word, n)) %>% 
  slice_head(n = 20) %>% 
  mutate(sScore = as.factor(sScore))  

sosul2_senti_tk %>% 
  ggplot(aes(n, reorder_within(word, n, author), fill = sScore)) + 
  geom_col() +
  scale_y_reordered() +
  scale_fill_brewer(type="div", palette = "RdBu") +
  facet_wrap( ~ author, scales = "free")

# 단계3: 감정분석
sosul2_df %>% 
  unnest_tokens(word, main) %>% 
  left_join(knu_dic_df) %>% 
  mutate(sScore = ifelse(is.na(sScore), 0, sScore)) %>% 
  arrange(sScore)

sosul2_df %>% 
  unnest_tokens(word, main) %>% 
  left_join(knu_dic_df) %>% 
  mutate(sScore = case_when(sScore >= 1  ~ "긍정",
                            sScore <= -1 ~ "부정", 
                            is.na(sScore) ~ NA_character_,
                            TRUE ~ "중립")) %>% 
  count(sScore)

sosul2_df %>% 
  unnest_tokens(word, main) %>% 
  left_join(knu_dic_df) %>% 
  mutate(sScore = ifelse(is.na(sScore), 0, sScore)) %>% 
  group_by(title) %>% 
  summarise(emotion = sum(sScore))

# 단계4: 시각화
#install.packages("wordcloud")
library(wordcloud)
sosul2_df %>% 
  unnest_tokens(word, main) %>% 
  count(word) %>% 
  filter(str_length(word) > 1) %>% 
  with(wordcloud(word, n, max.words = 50))

sosul2_df %>% 
  unnest_tokens(word, main) %>% 
  inner_join(knu_dic_df) %>% 
  mutate(emotion = case_when(sScore > 0 ~ "긍정",
                             sScore < 0 ~ "부정",
                             TRUE       ~ "중립")) %>% 
  filter(emotion != "중립") %>% 
  count(word, emotion, sort = TRUE) %>% 
  filter(str_length(word) > 1) %>% 
  pivot_wider(names_from = emotion, values_from = n, values_fill = 0)  %>% 
  column_to_rownames(var = "word") %>% 
  as.matrix() %>% 
  comparison.cloud(colors = c("red", "blue"), max.words = 50)


