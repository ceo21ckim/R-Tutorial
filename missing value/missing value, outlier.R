str(airquality)

rm(list = ls())

sum(is.na(airquality))

table(is.na(airquality$Temp))


mean(airquality$Temp)

# remove na ----
mean(airquality$Ozone, na.rm = TRUE)

air_narm <- airquality[!is.na(airquality$Ozone),]
air_narm
mean(air_narm$Ozone)

# 전체 결측치 모두 삭제된다.
air_narm1 <- na.omit(airquality)

mean(air_narm1$Ozone)


# outlier ----
patients <- data.frame(name = c('1', '2', '3', '4', '5'),
                       age = c(22, 20, 25,30, 27),
                       gender = factor(c('M', 'F', 'M', 'K', 'F')),
                       blood.type = factor(c('A', 'B', 'O', 'AB', 'C')))

patients

patients_outrm <- patients[patients$gender =='M' | patients$gender == 'F',]
patients_outrm1 <- patients[patients$gender != 'K',]
patients_outrm1

patients <- data.frame(name = c('1', '2', '3', '4', '5'),
                       age = c(22, 20, 25,30, 27),
                       gender = c('1', '2', '1', '3', '2'),
                       blood.type = c('1', '2', '3', '4', '5'))
patients$gender <- ifelse(patients$gender <1 | patients$gender >2, NA, patients$gender)
patients$blood.type <- ifelse(patients$blood.type <1 | patients$blood.type >4, NA, patients$blood.type)

patients




boxplot(airquality[,c(1:4)])
boxplot(airquality[,1])$stats


air <- airquality

table(is.na(air$Ozone))

air$Ozone <- ifelse(air$Ozone<1 | air$Ozone >122, NA, air$Ozone)

table(is.na(air$Ozone))

air_narm <- air[!is.na(air$Ozone),]


