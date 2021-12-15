# two-way analysis  of variance ----

# 10마리의 기니피그를 대상으로 비타민C가 이빨의 성장에 미치는 실험

# len : 이빨의 길이이며 종속변수로서의 역할
# supp :  비타민C 보충제
# dose : 0.5, 1.0, 2.0 밀리그램의 보충제 투여량

str(ToothGrowth)

ToothGrowth$dose <- factor(ToothGrowth$dose, levels = c(0.5, 1.0, 2.0),labels = c('Low', 'med', 'high'))

str(ToothGrowth)


ToothGrowth[seq(1,60,5), ]


with(ToothGrowth, tapply(len,list(supp, dose), length))

with(ToothGrowth, tapply(len,list(supp, dose), mean))

with(ToothGrowth, tapply(len,list(supp, dose), sd))


# two-way ANOVA를 할 때에는 각 독립변수에 *를 해준다.
ToothGrowth.aov <- aov(len ~ supp * dose, data = ToothGrowth)

# len ~ supp*dose == len ~ supp + dose + supp:dose ( 상호작용 )

summary(ToothGrowth.aov)


model.tables(ToothGrowth.aov, type = 'means')


boxplot(len ~ supp * dose, data= ToothGrowth, 
        col = c('deeppink', 'yellowgreen'), las = 1,
        xlab = 'Vitamin C Type', ylab = 'Tooth Growth',
        main = 'Effects of Vitamin C on Tooth Growth of Guinea Pigs')


# x.factor : x축에 위치할 집단 변수 
# trace.facotr : 그래프 상에 선으로 그려질 집단 변수
# response : 숫자 벡터인 반응변수(y축)
# type(b) : 선과 점 둘다 표현하기 위해서 'b'
# pch : 점의 모양
# trace.label : legend 이름.
interaction.plot(x.factor = ToothGrowth$dose, trace.factor = ToothGrowth$supp,
                 response = ToothGrowth$len, las = 1, type = 'b',
                 pch = c(1, 19), col = c('blue', 'red'), trace.label = 'Supplement',
                 xlab = 'Dose Level', ylab = 'Tooth Length',
                 main = 'Intercation Plot for Tooth Growth of Guinea Pigs')


interaction.plot(x.factor = ToothGrowth$supp, trace.factor = ToothGrowth$dose,
                 response = ToothGrowth$len, las = 1, type = 'b',
                 pch = c(1, 19), col = c('blue', 'red'), trace.label = 'Supplement',
                 xlab = 'Dose Level', ylab = 'Tooth Length',
                 main = 'Intercation Plot for Tooth Growth of Guinea Pigs')



library(gplots)

# connect 인수에는 선으로 연결할 집단평균의 인덱스를 지정할 수 있다. 
# fefault는 모든 집단 평균이 선으로 연결된다.
plotmeans(len ~ interaction(supp, dose, sep = ''), data = ToothGrowth,
          connect = list(c(1, 3, 5), c(2, 4, 6)),
          col = c('red', 'green3'),
          xlab = 'Supplement and Dost Combination', ylab = 'Tooth Length',
          main = 'Means Plot for Tooth Growth of Guinea Pigs \n with 95% CI of Mean')


plotmeans(len ~ interaction(supp, dose, sep = ''), data = ToothGrowth,
          xlab = 'Supplement and Dost Combination', ylab = 'Tooth Length',
          main = 'Means Plot for Tooth Growth of Guinea Pigs \n with 95% CI of Mean')

# 점들을 통과하는 직선을 추가하기 위해서는  pannel = panel.smmoth를 한다.
# lwd : 선의 두께
# col.smooth : 선의 색상
coplot(len ~ dose | supp, data = ToothGrowth, 
       col = 'steelblue', pch = 19,
       panel = panel.smooth, lwd=2, col.smooth = 'darkorange',
       xlab = 'Dose Level', ylab = 'Tooth Lenght')



if (!require('HH')) install.packages('HH')
library(HH)

interaction2wt(len ~ supp * dose, data = ToothGrowth)


TukeyHSD(ToothGrowth.aov)

#  which : 비교하고자하는 집단만을 지정한다.
TukeyHSD(ToothGrowth.aov, which = c('dose'), conf.level = 0.99)


TukeyHSD(ToothGrowth.aov, which = c('supp'), conf.level = 0.99)






# repeated two-way ANOVA ----

head(CO2, 3); tail(CO2, 3)

# Type :  집단 간 요인
# conc : (이산화탄소 농도) : 집단 내 요인
CO2sub <- subset(CO2, Treatment = 'chilled')
CO2sub$conc <- factor(CO2sub$conc)

# Error : 상호작용을 알기 위함.
CO2sub.aov <- aov(uptake ~ Type * conc + Error(Plant/conc), data = CO2sub)
summary(CO2sub.aov)


boxplot(uptake ~ Type * conc, data = CO2sub,
        col = c('deepskyblue', 'violet'), las = 2, cex.axis = 0.75, 
        ylab = 'Carbon dioxide uptake rate',
        main = 'Effects of Plant Type and CO2 on Carbon Dioxide Uptake')
legend('topleft', inset = 0.02, legend = c('Quebec', 'Mississippi'), fill = c('deepskyblue','violet'))

# 주효과와 상호작용효과를 두 종류의 도표를 이용하여 좀 더 명확하게 보여준다.
interaction2wt(uptake ~ conc*Type, data = CO2sub)


