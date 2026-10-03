library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(readxl)
library(ggthemr)
ggthemr("flat")

# 01_preprocess_net_sales.R가 만든 라면 상품군 월별 순판매량
r_1 <- read.csv("data/(1) 라면 순판매량.csv", fileEncoding = "euc-kr")
r_2 <- read.csv("data/(2) 라면 순판매량.csv", fileEncoding = "euc-kr")

################################################################################
# 연도별 순판매량 합계 계산
r_1_yearly <- aggregate(순판매량 ~ 년, data = r_1, sum)

# 막대그래프 생성
ggplot(data = r_1_yearly, aes(x = as.factor(년), y = 순판매량)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  ggtitle("1데이터 라면 중분류 연간 순판매량") +
  xlab("연도") +
  ylab("순판매량") +
  scale_y_continuous(breaks = seq(0, 200000, by = 50000), 
                     limits = c(0, 200000), 
                     expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(size = 11),  # x축 라벨 각도 및 크기
    axis.text.y = element_text(size = 11)  # y축 라벨 크기
  )

ggplot(data = r_1_yearly, aes(x = as.factor(년), y = 순판매량)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  ggtitle("1데이터 중분류 [라면,통조림,상온즉석] 연도별 순판매량") +
  xlab("연도") +
  ylab("순판매량") +
  scale_y_continuous(breaks = seq(0, 200000, by = 50000), 
                     labels = comma,  # 1000단위 쉼표 추가
                     limits = c(0, 200000), 
                     expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(size = 16),  # x축 라벨 크기
    axis.text.y = element_text(size = 16),  # y축 라벨 크기
    axis.title.x = element_text(size = 23), # x축 제목 크기
    axis.title.y = element_text(size = 23), # y축 제목 크기
    plot.title = element_text(size = 24, hjust = 0),  # 제목 크기 및 중앙 정렬
  )



################################################################################
# 연도별 순판매량 합계 계산
r_2_yearly <- aggregate(순판매량 ~ 년, data = r_2, sum)

# 막대그래프 생성
ggplot(data = r_2_yearly, aes(x = as.factor(년), y = 순판매량)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  ggtitle("2데이터 라면 대분류 연간 순판매량") +
  xlab("연도") +
  ylab("순판매량") +
  scale_y_continuous(breaks = seq(0, 100000, by = 25000), 
                     limits = c(0, 100000), 
                     expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(size = 11),  # x축 라벨 각도 및 크기
    axis.text.y = element_text(size = 11)  # y축 라벨 크기
  )

ggplot(data = r_2_yearly, aes(x = as.factor(년), y = 순판매량)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  ggtitle("2데이터 대분류 [면류.라면류] 연도별 순판매량") +
  xlab("연도") +
  ylab("순판매량") +
  scale_y_continuous(breaks = seq(0, 100000, by = 25000), 
                     labels = comma,  # 1000단위 쉼표 추가
                     limits = c(0, 100000), 
                     expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(size = 16),  # x축 라벨 크기
    axis.text.y = element_text(size = 16),  # y축 라벨 크기
    axis.title.x = element_text(size = 23), # x축 제목 크기
    axis.title.y = element_text(size = 23), # y축 제목 크기
    plot.title = element_text(size = 24, hjust = 0),  # 제목 크기 및 중앙 정렬
  )

################################################################################
################################################################################
# ggplot으로 꺾은선 그래프 생성
ggplot(r_1, aes(x = 월, y = 순판매량, color = as.factor(년), group = 년)) +
  geom_line(size = 2) + 
  geom_point(size = 2) +
  ggtitle("1데이터 중분류 [라면,통조림,상온즉석] 월별 순판매량") +
  xlab("월") +
  ylab("순판매량") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 24, face = "bold"),  # x축 라벨 크기
        axis.text.y = element_text(size = 22, face = "bold"),  # y축 라벨 크기
        axis.title.x = element_text(size = 20), # x축 제목 크기
        axis.title.y = element_text(size = 20), # y축 제목 크기
        plot.title = element_text(size = 27, face = "bold"),
        legend.text = element_text(size = 20, face = "bold"),
        panel.grid.major = element_line(color = "darkgray"),
        panel.grid.minor = element_line(color = "darkgray")
    )



ggplot(r_2, aes(x = 월, y = 순판매량, color = as.factor(년), group = 년)) +
  geom_line(size = 1.2) + 
  geom_point(size = 2.5) +
  ggtitle("2데이터 월별 라면 순판매량 비교") +
  xlab("월") +
  ylab("순판매량") +
  theme(legend.title = element_blank())


ggplot(r_2, aes(x = 월, y = 순판매량, color = as.factor(년), group = 년)) +
  geom_line(size = 2) + 
  geom_point(size = 2) +
  ggtitle("2데이터 대분류 [면류.라면류] 월별 순판매량 비교") +
  xlab("월") +
  ylab("순판매량") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 24, face = "bold"),  # x축 라벨 크기
        axis.text.y = element_text(size = 22, face = "bold"),  # y축 라벨 크기
        axis.title.x = element_text(size = 20), # x축 제목 크기
        axis.title.y = element_text(size = 20), # y축 제목 크기
        plot.title = element_text(size = 27, face = "bold"),  # 제목 크기 및 중앙 정렬
        legend.text = element_text(size = 20, face = "bold"),    # 범례 텍스트 크기
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_line(color = "darkgray")
  )

################################################################################
# 데이터 불러오기
data <- read.csv("data/최종 변수.csv", stringsAsFactors = FALSE)

# 날짜 형식을 Date 타입으로 변환
data$날짜 <- as.Date(data$날짜, format="%Y-%m-%d")

# ggplot으로 시계열 그래프 그리기
ggplot(data, aes(x = 날짜)) +
  geom_line(aes(y = 소비자물가지수, color = "소비자물가지수"), size = 3) +
  geom_line(aes(y = 라면물가지수, color = "라면물가지수"), size = 3) +
  labs(title = "소비자물가지수 및 라면물가지수 시계열 그래프",
       x = "날짜",
       y = "물가지수") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 16),  # x축 라벨 크기
        axis.text.y = element_text(size = 16),  # y축 라벨 크기
        axis.title.x = element_text(size = 23), # x축 제목 크기
        axis.title.y = element_text(size = 23), # y축 제목 크기
        plot.title = element_text(size = 24, hjust = 0),  # 제목 크기 및 중앙 정렬
        legend.text = element_text(size = 20)    # 범례 텍스트 크기
  )
