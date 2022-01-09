pacman::p_load(tidyverse, lubridate, reshape, XLConnect, googlesheets4)

wb <- "https://docs.google.com/spreadsheets/d/1bdncrGKj7Efbh-RJwtHFZSrjaXzYuMaisUOzJQUlcDU/edit?usp=sharing"
gs4_auth(email = "jason.gang@kakaoenterprise.com")

############# 1. 각 raw data를 merge로 합치기  ###############
M_2022_ALL <- range_read(wb, range="2022_ALL!A1:L")
M_2021_ALL <- range_read(wb, range="2021_ALL!A1:L")
M_2020_ALL <- range_read(wb, range="2020_ALL!A1:L")

range_delete(wb, range="merge!A1:M", shift = "up")
df <- range_write(wb, data = M_2022_ALL, range="merge!A1")
df %>% sheet_append(M_2021_ALL) %>% sheet_append(M_2020_ALL)



akmd_infra <- range_read(wb, range="merge!A1:L")
akmd_infra$Date_A <- as.Date(akmd_infra$Date_A)
akmd_infra$Date_B <- as.Date(akmd_infra$Date_B)


############# 2. 측정값 열 추가 ###############

#필요한 컬럼만 선택 (갑청구일, 대분류, 중분류, 소분류, 월비용, 간접관리비율)
akmd_infra <- akmd_infra %>% 
  select(Date_A, Item1, Item2, Item3, Listprice,Mgmtratio) %>% 
  mutate(
    YEAR=year(Date_A),
    MONTH=month(Date_A),
    .before=Item1
  )

#필요한 컬럼 생성 
akmd_infra <- akmd_infra %>% 
  mutate(YM=format(as.Date(Date_A), "%Y-%m"),
         .before=YEAR)

#계산된 열 생성
akmd_infra <- akmd_infra %>% 
  mutate(Mgmtcost=Listprice*Mgmtratio) %>% 
  mutate(Sum=Listprice+Mgmtcost)

range_write(wb, data = akmd_infra, range="data!A1")
############# 3. 전체 비용 연도별 / 월별 요약 ###############

#연도별 총계 요약 만들기
akmd_infra_sum_y <- akmd_infra %>% 
  group_by(YEAR) %>% 
  summarise(Sum=sum(Sum,na.rm=T), .groups='drop')

akmd_infra_sum_y$Sum <- round(akmd_infra_sum_y$Sum,1)


#월별 총계 요약 만들기
akmd_infra_sum_m <- akmd_infra %>% 
  group_by(Date_A) %>% 
  summarise(Sum=sum(Sum,na.rm=T), .groups='drop')

akmd_infra_sum_m$Sum <- round(akmd_infra_sum_m$Sum,1)
