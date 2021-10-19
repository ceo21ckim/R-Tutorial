# summarization ----

rm(list = ls())

# sample <- read.csv('sample.csv, header=TURE)
# sample <- readr::read_csv('sample.csv', col_names = TRUE)

car <- mtcars
# length ----
length(car)

length(car$mpg)


# View ----
View(car)


# head ----
head(car)

head(car, n = 10)

# 25r개의 관측치를 뺀 나머지를 보여준다.
head(car, n = -25)


# tail ----
tail(car, n = 6)


# str ----
str(car)


typeof(car$vs)

vs <- as.character(car$vs)

typeof(vs)


# summary ----
summary(car)

# NA 값을 확인해볼 수 있다.
car[!complete.cases(car),]

# describe ----
if (!require('Hmisc')) install.packages('Hmisc')
library(Hmisc)

describe(vs)

describe(car$mpg)

library(dplyr)
car
car %>% 
  group_by(gear) %>% 
  summarise(count = n(),
            mpg.mean = mean(mpg, na.rm = TRUE),
            mpg.median = median(mpg, na.rm = TRUE),
            mpg.sd = sd(mpg, na.rm =TRUE)) -> s1


knitr::kable(s1, 
             digits = 2, # 소숫점 단위
             align = 'c' # c : center, r : right, l : left
             )


