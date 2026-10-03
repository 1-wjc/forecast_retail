library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

data1_21_1 <- read_excel('data/(1 데이터) 유통데이터 활용 경진대회 배포용.xlsx', sheet = 1)
data1_21_2 <- read_excel('data/(1 데이터) 유통데이터 활용 경진대회 배포용.xlsx', sheet = 2)
data1_22_1 <- read_excel('data/(1 데이터) 유통데이터 활용 경진대회 배포용.xlsx', sheet = 3)
data1_22_2 <- read_excel('data/(1 데이터) 유통데이터 활용 경진대회 배포용.xlsx', sheet = 4)
data1_23_1 <- read_excel('data/(1 데이터) 유통데이터 활용 경진대회 배포용.xlsx', sheet = 5)
data1_23_2 <- read_excel('data/(1 데이터) 유통데이터 활용 경진대회 배포용.xlsx', sheet = 6)

data2_21 <- read_excel('data/(2 데이터) 유통데이터 활용 경진대회 배포용.xlsx', sheet = 1)
data2_22 <- read_excel('data/(2 데이터) 유통데이터 활용 경진대회 배포용.xlsx', sheet = 2)
data2_23 <- read_excel('data/(2 데이터) 유통데이터 활용 경진대회 배포용.xlsx', sheet = 3)

data1 <- rbind(data1_21_1, data1_21_2, data1_22_1, data1_22_2, data1_23_1, data1_23_2)
data2 <- rbind(data2_21, data2_22, data2_23)

write.csv(data1, file = 'data/(1) 원데이터 합본.csv', fileEncoding = 'euc-kr', row.names = FALSE)
write.csv(data2, file = 'data/(2) 원데이터 합본.csv', fileEncoding = 'euc-kr', row.names = FALSE)

################################################################################
data1 <- data1[data1$판매수량 > 0, ]

data1$판매일 <- as.Date(data1$판매일, format = "%Y-%m-%d")

data1$년 <- format(data1$판매일, "%Y")
data1$월 <- format(data1$판매일, "%m")
data1$일 <- format(data1$판매일, "%d")

write.csv(data1, file = 'data/(1) (-)값 제거.csv', fileEncoding = 'euc-kr', row.names = FALSE)

# 매출과 반품으로 구분
data_sales1 <- data1 %>% filter(구분 == "매출")
data_returns1 <- data1 %>% filter(구분 == "반품")

# 년과 월을 이용하여 판매수량 총합 계산
sales_summary1 <- data_sales1 %>%
  group_by(년, 월, 중분류) %>%
  summarise(판매수량 = sum(판매수량))

returns_summary1 <- data_returns1 %>%
  group_by(년, 월, 중분류) %>%
  summarise(반품수량 = sum(판매수량))

# 매출 - 반품 값을 계산하여 순판매량 열 생성
final_data1 <- sales_summary1 %>%
  left_join(returns_summary1, by = c("년", "월", "중분류")) %>%
  mutate(반품수량 = ifelse(is.na(반품수량), 0, 반품수량),
         순판매량 = 판매수량 - 반품수량)

# 최종 데이터프레임: 날짜, 중분류, 판매수량, 반품수량, 순판매량
final_data1 <- final_data1 %>%
  mutate(날짜 = paste(년, 월, sep = "-")) %>%
  select(날짜, 중분류, 판매수량, 반품수량, 순판매량)

# CSV로 저장
write.csv(final_data1, 'data/(1) 월별 순판매량.csv', fileEncoding = 'euc-kr', row.names = FALSE)

# 년, 월, 일별로 판매수량 총합 계산
sales_summary_daily <- data_sales1 %>%
  group_by(년, 월, 일, 중분류) %>%
  summarise(판매수량 = sum(판매수량))

returns_summary_daily <- data_returns1 %>%
  group_by(년, 월, 일, 중분류) %>%
  summarise(반품수량 = sum(판매수량))

# 매출 - 반품 값을 계산하여 순판매량 열 생성
final_daily <- sales_summary_daily %>%
  left_join(returns_summary_daily, by = c("년", "월", "일", "중분류")) %>%
  mutate(반품수량 = ifelse(is.na(반품수량), 0, 반품수량),
         순판매량 = 판매수량 - 반품수량)

# 최종 데이터프레임: 날짜, 대분류, 판매수량, 반품수량, 순판매량
final_daily <- final_daily %>%
  mutate(날짜 = paste(년, 월, 일, sep = "-")) %>%
  select(날짜, 중분류, 판매수량, 반품수량, 순판매량)

# CSV로 저장
write.csv(final_daily, 'data/(1) 일일 순판매량.csv', fileEncoding = 'euc-kr', row.names = FALSE)

################################################################################
data2 <- data2[data2$판매수량 > 0, ]

data2$판매일 <- as.Date(data2$판매일, format = "%Y-%m-%d")

data2$년 <- format(data2$판매일, "%Y")
data2$월 <- format(data2$판매일, "%m")
data2$일 <- format(data2$판매일, "%d")

write.csv(data2, file = 'data/2데이터 -값 제거.csv', fileEncoding = 'euc-kr', row.names = FALSE)

# 매출과 반품으로 구분
data_sales2 <- data2 %>% filter(구분 == "매출")
data_returns2 <- data2 %>% filter(구분 == "반품")

# 년과 월을 이용하여 판매수량 총합 계산
sales_summary2 <- data_sales2 %>%
  group_by(년, 월, 대분류) %>%
  summarise(판매수량 = sum(판매수량))

returns_summary2 <- data_returns2 %>%
  group_by(년, 월, 대분류) %>%
  summarise(반품수량 = sum(판매수량))

# 매출 - 반품 값을 계산하여 순판매량 열 생성
final_data2 <- sales_summary2 %>%
  left_join(returns_summary2, by = c("년", "월", "대분류")) %>%
  mutate(반품수량 = ifelse(is.na(반품수량), 0, 반품수량),
         순판매량 = 판매수량 - 반품수량)

# 최종 데이터프레임: 년월, 대분류, 판매수량, 반품수량, 순판매량
final_data2 <- final_data2 %>%
  mutate(날짜 = paste(년, 월, sep = "-")) %>%
  select(날짜, 대분류, 판매수량, 반품수량, 순판매량)

# CSV로 저장
write.csv(final_data2, 'data/(2) 월별 순판매량.csv', fileEncoding = 'euc-kr', row.names = FALSE)

r_1 <- final_data1 %>% 
  filter(중분류 == '라면,통조림,상온즉석')

r_2 <- final_data2 %>% 
  filter(대분류 == '면류.라면류')

write.csv(r_1, 'data/(1) 라면 순판매량.csv', fileEncoding = 'euc-kr', row.names = FALSE)
write.csv(r_2, 'data/(2) 라면 순판매량.csv', fileEncoding = 'euc-kr', row.names = FALSE)


r_1_daily <- final_daily %>% 
  filter(중분류 == '라면,통조림,상온즉석')

r_1_daily$날짜 <- as.Date(r_1_daily$날짜)

r_1_daily$요일 <- weekdays(r_1_daily$날짜)

r_1_weekday <- r_1_daily %>%
  group_by(년, 요일, 중분류) %>%
  summarise(판매량 = sum(순판매량))

write.csv(r_1_weekday, 'data/1데이터 요일별.csv', fileEncoding = 'euc-kr', row.names = FALSE)
