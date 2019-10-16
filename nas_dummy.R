#1.데이터셋 불러오기 
#2.날짜는 데이터형식으로, uniqueNo는 str으로 
#3.성별로 데이터셋 만들고 전체 평균, 표준편차, densityPlot 그리고 데이터 특성 파악
#4.더미데이터셋 생성 

library(readr)
library(dplyr)
library(moonBook)

#1-1
guess_encoding("nasdummy.csv")
nasdummy <- read_csv("nasdummy.csv", 
                     locale=locale('ko',encoding='euc-kr'),
                      col_types = cols(uniqueNo = col_character(),
                                       sex = col_factor(c("암","수","거세")),
                                       deadDate = col_date(format = "%Y%m%d")
                                       )
                     )
str(nasdummy)
head(nasdummy)
table(nasdummy$sex)
summary(nasdummy)

#1-2 remove missing data
nasdummy <- na.omit(nasdummy)

#1-3 dataset by sex 
dummy_female <- nasdummy %>% filter(sex == "암")
dummy_male <- nasdummy %>% filter(sex == "수")
dummy_steer <- nasdummy %>% filter(sex == "거세")

# 2-1 연속분포 판단 *성별 그룹이 3가지* anova test 해야함 
out1 = aov(btf~sex, data = nasdummy)
shapiro.test(resid(out1))

out2 = aov(ema~sex, data = nasdummy)
shapiro.test(resid(out2))

# 2-2 공분산 판단 
bartlett.test(btf~sex, data = nasdummy)  # pvalue < 0.05 등분산이 아니다. 
bartlett.test(ema~sex, data = nasdummy)  # pvalue > 0.05 등분산이다. 

# 2-3 ANOVA
summary(out1)  ## pvalue<0.05 성별 btf는 유의미한 차이가 있다. 
summary(out2)  ## 성별 ema는 유의미한 차이가 있다. 

# 2-4 kruskal test
oneway.test(btf~sex, data = nasdummy, var.equal = FALSE)
kruskal.test(ema~factor(sex), data = nasdummy)

# 2-5 Tukey
library(nparcomp)
result = mctp(btf~sex, data = nasdummy)
summary(result)

library(userfriendlyscience)
posthocTGH(nasdummy$sex, y = nasdummy$ema, method = 'games-howell')

# 3-1 dummy data 생성

with(nasdummy,tapply(btf,sex,sd))
with(nasdummy,tapply(btf,sex,mean))
with(nasdummy,tapply(ema,sex,sd))
with(nasdummy,tapply(ema,sex,mean))
with(nasdummy,tapply(weight,sex,sd))
with(nasdummy,tapply(weight,sex,mean))
with(nasdummy,tapply(age,sex,sd))
with(nasdummy,tapply(age,sex,mean))

# 3-2 create random data
library(truncnorm)
date1 <- as.Date("2019-08-01")
date2 <- as.Date("2019-09-30")


df_female <- data.frame(uniqueNo = seq(1,24000),
                        date = rep(seq.Date(as.Date("2019-08-01"), length.out = 60, by = "day"),each=400),
                        sex = rep("암", times=24000),
                        age = rtruncnorm(n=24000, a=25, b=136, mean = 62, sd= 24),
                        carcassWeight = rtruncnorm(n=24000, a=233, b=547, mean=381, sd=54),
                        btf = rtruncnorm(n=24000, a=2, b=35, mean = 16, sd=7),
                        ema = rtruncnorm(n=24000, a=62, b=116, mean = 88, sd=12 ))

df_male <- data.frame(uniqueNo = seq(24001,48000),
                      date = rep(seq.Date(as.Date("2019-08-01"), length.out = 60, by = "day"),each=400),
                      sex = rep("수", times=24000),
                      age = rtruncnorm(n=24000, a=17, b=83, mean = 31, sd= 13),
                      carcassWeight = rtruncnorm(n=24000, a=298, b=631, mean=428, sd=74),
                      btf = rtruncnorm(n=24000, a=1, b=16, mean = 6.2, sd=3.2),
                      ema = rtruncnorm(n=24000, a=71, b=124, mean =92.8 , sd=11.3))
                      
                      
df_steer <- data.frame(uniqueNo = seq(48001,72000),
                        date = rep(seq.Date(as.Date("2019-08-01"), length.out = 60, by = "day"),each=400),
                        sex = rep("거세", times=24000),
                        age = rtruncnorm(n=24000, a=17, b=83, mean = 32, sd= 1.8),
                        carcassWeight = rtruncnorm(n=24000, a=, b=631, mean=472, sd=48),
                        btf = rtruncnorm(n=24000, a=2, b=35, mean = 16, sd=6.5),
                        ema = rtruncnorm(n=24000, a=62, b=116, mean = 95, sd=11))    
df_all <- rbind(df_female,df_male,df_steer)

write.csv(df_all, file ="NAS_dfall.csv",row.names = F, na="")
