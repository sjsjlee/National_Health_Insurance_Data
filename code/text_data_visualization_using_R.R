# 빅데이터 시각화 텍스트 데이터 시각화
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(showtext)
library(multilinguer)
library(KoNLP)


insurance <- read.csv("national_insurance.csv", stringsAsFactors = F, encoding = 'UTF-8')

insurance <- insurance %>% rename(value = 제목)
text <- insurance %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value = str_squish(value))
text <- text %>% unnest_tokens(input = value, output = word, token = extractNoun)
frequency <- text %>% count(word) %>% filter(str_count(word)>1)

library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic") #필수
showtext_auto() #필수

top_30 <- frequency %>% filter(!word %in%  c('국민', '건강', '보험', '국민건강보험', '건보', '공단', '신민아', '지사', '작년')) %>% slice_max(n, n=30)
ggplot(top_30, aes(reorder(word,n),n, fill = word)) + geom_bar(stat='identity') + coord_flip() + labs(title = '뉴스 크롤링 데이터 상위 30개 단어 분석', x = '단어', y = '빈도') + theme_light() + theme(legend.position = 'none') + theme(title = element_text(size = 8), axis.text.y = element_text(size = 7), text = element_text(family = 'nanumgothic'))


# 워드클라우드(2020년 데이터 대상)
library(wordcloud2)
top_200 <- frequency %>% filter(!word %in%  c('국민', '건강', '보험', '국민건강보험', '건보', '공단', '신민아', '지사', '작년')) %>% slice_max(n, n = 200)
wordcloud2(data = top_200, size = 0.5, fontFamily = 'nanumgothic')
