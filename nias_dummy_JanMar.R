library(readr)
library(dplyr)
library(moonBook)
library(truncnorm)

#1 일별 도축두수에 따른 일자 생성 

#1 성별
##암소
f1_date <- data.frame(date = rep(seq.Date(as.Date("2019-01-01"),as.Date("2019-01-31"),by = "day")),
                      sex = rep("1",31),
                      count= round(runif(31,min = 1700,max = 2700),0))
f2_date <- data.frame(date = rep(seq.Date(as.Date("2019-02-01"),as.Date("2019-02-28"),by = "day")),
                      sex = rep("1",28),
                      count= round(runif(28,min = 600,max = 1100),0))
f3_date <- data.frame(date = rep(seq.Date(as.Date("2019-03-01"),as.Date("2019-03-31"),by = "day")),
                      sex = rep("1",31),
                      count= round(runif(31,min = 800 ,max = 1500),0))
f_date_all <- rbind(f1_date,f2_date,f3_date)

##수소
m1_date <- data.frame(date = rep(seq.Date(as.Date("2019-01-01"),as.Date("2019-01-31"),by = "day")),
                      sex = rep("2",31),
                      count= round(runif(31,min = 70,max = 170),0))
m2_date <- data.frame(date = rep(seq.Date(as.Date("2019-02-01"),as.Date("2019-02-28"),by = "day")),
                      sex = rep("2",28),
                      count= round(runif(28,min = 20 ,max = 80),0))
m3_date <- data.frame(date = rep(seq.Date(as.Date("2019-03-01"),as.Date("2019-03-31"),by = "day")),
                      sex = rep("2",31),
                      count= round(runif(31,min = 30 ,max = 70 ),0))
m_date_all <- rbind(m1_date,m2_date,m3_date)

##거세우 
n1_date <- data.frame(date = rep(seq.Date(as.Date("2019-01-01"),as.Date("2019-01-31"),by = "day")),
                      sex = rep("3",31),
                      count= round(runif(31,min = 2200,max = 3300),0))
n2_date <- data.frame(date = rep(seq.Date(as.Date("2019-02-01"),as.Date("2019-02-28"),by = "day")),
                      sex = rep("3",28),
                      count= round(runif(28,min = 800,max = 1500),0))
n3_date <- data.frame(date = rep(seq.Date(as.Date("2019-03-01"),as.Date("2019-03-31"),by = "day")),
                      sex = rep("3",31),
                      count= round(runif(31,min = 1000 ,max = 1800 ),0))
n_date_all <- rbind(n1_date,n2_date,n3_date)

## 성별 날짜 더미데이터 생성 
fdate <- as.vector(f_date_all$date)
fcnt <- as.vector(f_date_all$count)
f_all <-  rep(fdate,fcnt)

mdate <- as.vector(m_date_all$date)
mcnt <- as.vector(m_date_all$count)
m_all <-  rep(mdate,mcnt)

ndate <- as.vector(n_date_all$date)
ncnt <- as.vector(n_date_all$count)
n_all <-  rep(ndate,ncnt)

# colnames ; date, sex, BFAT, EMA, CARCASS_WT, AGE, AGE2 
df_female3 <- data.frame(DATE = as.Date(f_all,origin="1970-01-01"),
                         SEX = rep("1",times=126764),
                         BFAT = rtruncnorm(n=126764, a=2, b=35, mean = 16, sd=7),
                         EMA = rtruncnorm(n=126764, a=62, b=116, mean = 88, sd=12),
                         CARCASS_WT = rtruncnorm(n=126764, a=233, b=547, mean=381, sd=54),
                         AGE=rtruncnorm(n=126764, a=25, b=136, mean = 62, sd= 24)
                         )
df_female3 <- df_female3 %>% mutate(AGE2=(AGE)^2)

df_male3 <- data.frame(DATE = as.Date(m_all,origin="1970-01-01"),
                         SEX = rep("2",times=6515),
                         BFAT = rtruncnorm(n=6515, a=1, b=16, mean = 6.2, sd=3.2),
                         EMA = rtruncnorm(n=6515, a=71, b=124, mean =92.8 , sd=11.3),
                         CARCASS_WT = rtruncnorm(n=6515, a=298, b=631, mean=428, sd=74),
                         AGE= rtruncnorm(n=6515, a=17, b=83, mean = 31, sd= 13)
)
df_male3 <- df_male3 %>% mutate(AGE2=(AGE)^2)

df_steer3 <- data.frame(DATE = as.Date(n_all,origin="1970-01-01"),
                       SEX = rep("3",times= 162769),
                       BFAT = rtruncnorm(n=162769, a=2, b=35, mean = 16, sd=6.5),
                       EMA = rtruncnorm(n=162769, a=62, b=116, mean = 95, sd=11),
                       CARCASS_WT = rtruncnorm(n=162769, a=, b=631, mean=472, sd=48),
                       AGE= rtruncnorm(n=162769, a=17, b=83, mean = 32, sd= 1.8)
)
df_steer3 <- df_steer3 %>% mutate(AGE2=(AGE)^2)

df_JanMar_dummy <- rbind(df_female3, df_male3, df_steer3)

write.csv(df_JanMar_dummy, file = "NIAS_Sample_JanMar.csv",row.names = F, na="")