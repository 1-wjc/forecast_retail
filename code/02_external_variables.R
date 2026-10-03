# 데이터 로드
data_L <- read.csv("data/생활물가지수_2020100__20241002205203.csv")
data_I <- read.csv("data/수입물가지수_품목별__20241002231917.csv")

# 필요한 라이브러리 로드
library(ggplot2)
library(tidyr)
library(dplyr)

# '총지수', '생활물가지수', '라면' 데이터 필터링
filtered_data <- data_L[data_L$품목별 %in% c('소비자물가지수', '생활물가지수', '라면'), ]
filtered_data2 <- data_I[data_I$계정코드별 %in% c('밀 (2020=100)'), ]


# 데이터 전처리: wide format을 long format으로 변환
long_data <- pivot_longer(filtered_data, cols = starts_with('X'),
                          names_to = "Month", values_to = "Index")

# 그래프 그리기
ggplot(long_data, aes(x = Month, y = as.numeric(Index), color = 품목별, group = 품목별)) +
  geom_line() +
  geom_point() +
  labs(title = "",
       x = "Month", y = "지수") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))


# CSV 파일 읽기 (경로에 맞게 수정)
file_path <- "data/기온.csv"
temperature_data <- read.csv(file_path, fileEncoding = "euc-kr")

# 날짜 컬럼을 Date 타입으로 변환
temperature_data$날짜 <- as.Date(temperature_data$날짜, format = "%Y-%m-%d")

# 연도와 월 컬럼 추가
temperature_data$연도 <- format(temperature_data$날짜, "%Y")
temperature_data$월 <- format(temperature_data$날짜, "%m")

# 월별 평균기온 계산
monthly_avg_temp <- temperature_data %>%
  group_by(연도, 월) %>%
  summarise(평균기온 = mean(평균기온, na.rm = TRUE))

# 월을 숫자로 변환 (그래프 정렬을 위해)
monthly_avg_temp$월 <- as.numeric(monthly_avg_temp$월)

# 꺾은선 그래프 그리기
ggplot(monthly_avg_temp, aes(x = 월, y = 평균기온, group = 연도, color = 연도)) +
  geom_line() +
  labs(title = "연도별 월별 평균기온",
       x = "월", 
       y = "평균기온 (°C)") +
  theme_minimal()





total <- data_L
total <- total %>% 
  select(-시도별)

total$구분 <- '생활'
total <- total[, c(ncol(total), 1:(ncol(total)-1))]

total2 <- data_I %>% 
  select(-통화계약구분코드별)

colnames(total2)[colnames(total2) == "계정코드별"] <- "품목별"

total2$구분 <- '수입'
total2 <- total2[, c(ncol(total2), 1:(ncol(total2)-1))]

total2 <- total2 %>% 
  select(-c(X2024.07, X2024.08))

total1 <- total

total <- rbind(total1, total2)


temp <- read.csv(file = 'data/기온.csv', fileEncoding = "euc-kr")


library(lubridate)

# 날짜 열을 Date 형식으로 변환하고 공백 제거
temp$날짜 <- trimws(temp$날짜)  # 공백 제거
temp$날짜 <- as.Date(temp$날짜, format="%Y-%m-%d")

# 연도와 월 열 추가
temp$연도 <- year(temp$날짜)
temp$월 <- month(temp$날짜)

# 연도별, 월별로 평균기온을 계산
monthly_avg_temp <- temp %>%
  group_by(연도, 월) %>%
  summarise(평균기온 = mean(평균기온, na.rm = TRUE)) %>%
  arrange(연도, 월)  # 연도와 월 기준으로 정렬

# 결과 확인
print(monthly_avg_temp)

write.csv(total, file = 'data/물가지수.csv', fileEncoding = 'euc-kr', row.names = FALSE)
write.csv(monthly_avg_temp, file = 'data/평균기온.csv', fileEncoding = 'euc-kr', row.names = FALSE)



# 데이터 불러오기 (파일 경로에 맞게 수정하세요)
monthly_avg_temp <- read.csv("data/평균기온.csv", fileEncoding = "euc-kr")

# 월과 연도를 factor로 변환
monthly_avg_temp$연도 <- as.factor(monthly_avg_temp$연도)
monthly_avg_temp$월 <- as.factor(monthly_avg_temp$월)

# ggplot으로 꺾은선 그래프 그리기
ggplot(data = monthly_avg_temp, aes(x = 월, y = 평균기온, color = 연도, group = 연도)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "연도별 월 평균기온", x = "월", y = "평균기온 (℃)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




# '총지수', '생활물가지수', '라면' 데이터 필터링
graph_data <- total[total$품목별 %in% c('총지수 (2020=100)', '밀 (2020=100)'), ]

# 열을 모두 문자형으로 변환한 후 pivot_longer 적용
graph_data <- graph_data %>% 
  mutate(across(starts_with("X"), as.character))

long_data <- pivot_longer(graph_data, cols = starts_with('X'),
                          names_to = "Month", values_to = "Index")

# 데이터 전처리: wide format을 long format으로 변환
long_data <- pivot_longer(graph_data, cols = starts_with('X'),
                          names_to = "Month", values_to = "Index")

# 그래프 그리기
ggplot(long_data, aes(x = Month, y = as.numeric(Index), color = 품목별, group = 품목별)) +
  geom_line() +
  geom_point() +
  labs(title = "",
       x = "Month", y = "지수") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        legend.text = element_text(size = 15)) # 범례 텍스트 크기 조정
