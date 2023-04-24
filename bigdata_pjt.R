library(readxl)
library(dplyr)
library(ggplot2)
theme_set(theme_grey(base_family='AppleGothic'))
par(family = "AppleGothic")

library(MASS)


raw_df <- read_excel('빅데이터_분석_data.xlsx')
raw_df$날짜 <- as.Date(raw_df$날짜)
View(raw_df)
raw_df[,1]

## 2012.1 부터 아파트_평균매매가격의 데이터가 존재하기에 2011년 데이터 밑으로 제거
df <- raw_df["2012-01-01" <= raw_df$날짜 & raw_df$날짜 <= '2022-09-01',]
df <- read_excel('빅데이터_분석_data1.xlsx')
length(df)
df[,1]
View(df)

df %>%
  select(c('날짜', 'X2년물_금리', '아파트_매매가격지수', '아파트_평균매매가격')) %>%
  print


df %>%
  #select(c('날짜', 'X2년물_금리', '아파트_매매가격지수')) %>% 
  ggplot(aes(x = 날짜)) + 
  geom_line(aes(y = X2년물_금리), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = 아파트_매매가격지수), color = 'red', linetype = 2) + 
  geom_line(aes(y = 아파트_평균매매가격), color = 'blue', linetype = 2)



z_df <- df
colnames(z_df)

z_df$X2년물_금리 <- scale(df$X2년물_금리)
z_df$X2년물_금리_거래량 <- scale(z_df$X2년물_금리_거래량)
z_df$아파트_매매가격지수 <- scale(df$아파트_매매가격지수)
z_df$아파트_평균매매가격 <- scale(df$아파트_평균매매가격)
z_df$Fed_금리 <- scale(df$Fed_금리)
z_df$미국_CPI_지수 <- scale(df$미국_CPI_지수)
z_df$미국_주택착공건수 <- scale(df$미국_주택착공건수)
z_df$미국_신규_주택판매 <- scale(df$미국_신규_주택판매)
z_df$달러_원 <- scale(df$달러_원)
z_df$달러_원_변동 <- scale(df$달러_원_변동)
z_df$서울_주택_건설_인허가_실적 <- scale(df$서울_주택_건설_인허가_실적)
z_df$아파트거래_월별 <- scale(df$아파트거래_월별)
z_df$한국_CPI_지수 <- scale(df$한국_CPI_지수)


z_model <- lm(아파트_평균매매가격 ~ ., data = z_df[,-1])
summary(z_model)

new_z_model <- stepAIC(z_model)

# X2년물_금리 + Fed_금리 + 
# 미국_CPI_지수 + 미국_신규_주택판매 + `달러/원_변동` + 
# 아파트_매매가격지수 + 아파트거래_월별 + 한국_CPI_지수

z_df %>%
  ggplot(aes(x = 날짜)) + 
  geom_line(aes(y = X2년물_금리), color = 'dark grey', linetype = 1) +
  #geom_line(aes(y = X2년물_금리_변동), color = 'dark grey', linetype = 1)
  #geom_line(aes(y = 아파트_매매가격지수), color = 'blue', linetype = 3) +
  geom_line(aes(y = 아파트_평균매매가격), color = 'blue', linetype = 3) +
  geom_line(aes(y = Fed_금리), color = 'dark grey', linetype = 1) +
  geom_line(aes(y = 미국_CPI_지수), color = 'red', linetype = 2) +
  #geom_line(aes(y = 미국_주택착공건수), color = 'red', linetype = 2) +
  geom_line(aes(y = 미국_신규_주택판매), color = 'red', linetype = 1) +
  #geom_line(aes(y = 달러_원), color = 'green', linetype = 2) +
  geom_line(aes(y = 달러_원_변동), color = 'red', linetype = 2) +
  #geom_line(aes(y = 서울_주택_건설_인허가_실적), color = 'blue', linetype = 1) +
  geom_line(aes(y = 아파트거래_월별), color = 'green', linetype = 1) +
  geom_line(aes(y = 한국_CPI_지수), color = 'blue', linetype = 1)



#model <- lm(아파트_매매가격지수 ~ X2년물_금리+ 미국_CPI_지수, data = df_except_날짜)
model <- lm(아파트_매매가격지수 ~ ., data = df[,-1])
summary(model)

newmodel <- stepAIC(model)
newmodel

## 독립변수인 아파트 가격 매매와 상관관계가 높은 변수
# 아파트_매매가격지수 ~ X2년물_금리 + X2년물_금리_변동 + Fed_금리 + 미국_신규_주택판매 + 
# 달러_원 + 아파트_평균매매가격 + 아파트거래_월별 + 
# 한국_CPI_지수, -> 표준화를 할때와 동일 따라서 위의 변수를 활용하여 매매가격기수 예측






