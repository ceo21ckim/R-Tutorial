# getwd()로 현재 directory 위치를 확인하고, setwd()를 통해서 파일이 있는 위치로 가야된다.
# 파일은 gapminder 데이터를 활용.

n <- readLines('wine.name.txt')
wine <- read.csv('wine.data.txt', header=FALSE, sep=',')

wine

names(wine)[2:14] <- substr(n, 4 , nchar(n))

library(dplyr)

# train_test_split ----
train_set = sample_frac(wine, 0.6)
str(train_set)

test_set = setdiff(wine, train_set)


elec_gen <- read.csv('electricity_generation_per_person.csv', header = TRUE)
elec_use <- read.csv('electricity_use_per_person.csv', header = TRUE)

View(elec_use)


names(elec_gen)[2:33] <- substr(names(elec_gen)[2:33], 2, nchar(names(elec_gen)[2:33]))
names(elec_gen)

names(elec_use)[2:56] <- substr(names(elec_use)[2:56], 2, nchar(names(elec_use)[2:56]))
names(elec_use)

# gather ----
if (!require('tidyr')) install.packages('tidyr')
library(tidyr)
# 속성을 재구성해준다.
str(elec_gen)

# country 를 뺀 나머지 값들을 year라는 값으로 묶어준다. 
# country 값 뺴고 모두 transpose 해주는거.
elec_gen_df <- elec_gen %>% 
  gather(-country, key = 'year', value = 'ElectricityGeneration')

head(elec_gen_df)

elec_use_df <- elec_use %>% 
  gather(-country, key = 'year', value = 'ElectricityUse')



# merge ----
# 둘다 겹치는 값만 출력해준다. 
elec_gen_use <- merge(elec_gen_df, elec_use_df)
elec_gen_use




