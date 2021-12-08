# lavaan -> PoliticalDemocracy 
# 구조방정식을 지원하는 패키지 
# sem, strum, lava, lavaan, OpenMx etc..

rm(list = ls())

library(lavaan)
str(PoliticalDemocracy)

cfa <- "ind60 =~ x1 + x2 + x3  
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8"
 
cfa(model = cfa, data = PoliticalDemocracy)

lavOptions('std.lv')

fit <- cfa(model = cfa, data = PoliticalDemocracy)
summary(fit, fitmeasures = TRUE, standardized = TRUE)

library(knitr)
options(knift.kable.NA='')
library(dplyr)

parameterEstimates(fit, standardized = TRUE) %>% 
  filter(op == '=~') %>% 
  mutate(stars = ifelse(pvalue < 0.001, '***', 
                        ifelse(pvalue < 0.01, "**", 
                               ifelse(pvalue < 0.05), '*', ""))) %>% 
  select("Latent Factor" = lhs, Indicator = rhs, B = est, SE = se, 
         Z = z, "p-value" = pvalue, Sig. = stars, Beta = std.all) %>% 
  kable(digits = 3, format = "pandoc", caption = "Factor Loading")


library(knitr)
options(knitr.kable.NA = "")
parameterEstimates(fit, standardized = TRUE) %>% 
  filter(op == '=~') %>% 
  mutate(stars = ifelse(pvalue < 0.001, "***", 
                        ifelse(pvalue < 0.01, "**", 
                               ifelse(pvalue < 0.05, '*', "")))) %>% 
  select("Latent Factor" = lhs, Indicator = rhs, B = est, SE = se, 
         Z = z, "p-value" = pvalue, Sig. = stars, Beta = std.all) %>% 
  kable(digits = 3, format = "pandoc", caption = 'Factor Loadings')

residuals(fit, type = "cor")$cov

resid.cor <- residuals(fit, type = "cor")$cov 
resid.cor[upper.tri(resid.cor, diag = TRUE)] <- NA

library(knitr)
options(knitr.kable.NA = "")
kable(resid.cor, digits = 2, format = "pandoc", caption = "Residual Correlations")

fitMeasures(fit)

fitMeasures(fit, c('chisq', 'df', 'pvalue', 'gfi', 'rmsea', 'cfi'))

# modify metric ----
summary(fit, modindices = TRUE)
modindices(fit)


library(knitr)
modindices(fit) %>% 
  filter(op == '=~') %>% 
  kable(digits = 3, format = 'pandoc', 
        caption = 'Modification Indices for Factor Loadings')

modindices(fit, sort.= TRUE, minimum.value = 3)

cfa2 <- "ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8"

fit2 <- cfa(model = cfa2, data = PoliticalDemocracy)
fit2

anova(fit, fit2)

fitMeasures(fit2, c('chisq', 'df', 'pvalue'))

library(dplyr) ; library(tibble) ; library(magrittr) ; library(stargazer)
compareFit <- function(...) {
  m <- list(...)
  sapply(m, fitMeasures) %>% 
    set_colnames(paste0("Model", 1:length(m))) %>% 
    as.data.frame() %>% 
    rownames_to_column('Fit_Measures') %>% 
    slice(match(c('chisq', 'df', 'pvalue', 
                  'gfi', 'rmsea', 'cfi'), Fit_Measures)) %>% 
    mutate(Fit_Measures = c('Chi-square', 'df', 'p-value', 'GFI', 'RMSEA', 'CFI'))
}

compareFit(fit, fit2) %>% 
  stargazer(type = 'text', title = 'Model Comparison', summary = FALSE, 
            digits = 3, digits.extra = 0, rownames = FALSE)

# 요인적재값의 통계적 유의성
standardizedsolution(fit2)

standardizedsolution(fit2) %>% 
  filter(op == '=~') %>% 
  mutate(stars = ifelse(pvalue < 0.001, "***", 
                        ifelse(pvalue < 0.01, "**", 
                               ifelse(pvalue < 0.05, "*", "")))) %>% 
  select(Construct=lhs, Item=rhs, "Factor Loading" = est.std, 
         Z=z, "p-value" = pvalue, Sig.=stars) %>% 
  stargazer(type = 'text', title = 'Comvergent Validity: Factor Loadings', summary = FALSE, 
            digits = 3, digits.extra = 0, rownames = FALSE)


# 측정모델 경로도

library(semPlot)

semPaths(fit2, what = 'std', layout = 'tree2', edge.label.cex = 1, edge.color = 'darkgreen', 
         color = list(lat = 'orange', man = 'palegoldenrod'), fade = FALSE, 
         style = 'lisrel', rotation = 4, curvature = 2)


# 신뢰도 
if (!require('semTools')) install.packages('semTools')

library(semTools)

# 크론바흐, 복합신뢰도, 평균분산추출
reliability(fit2)


reliability(fit2) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column("Construct") %>% 
  slice(-n()) %>% 
  select(Construct, "Composite Reliability" = omega, 
         "Average Variance Extracted" = avevar, "Cronbach's alpha" = alpha) %>% 
  stargazer(type = 'text', title = 'Convergent Validity: Reliability', 
            summary = FALSE, digits = 3, digits.extra = 0, rownames = FALSE)


lavInspect(fit2, what = 'cor.lv')

lavInspect(fit2, what = 'cor.lv') %>% 
  as.data.frame() %>% 
  rownames_to_column('Construct') %>% 
  cbind(Square_Root_of_AVE = 
          sqrt(reliability(fit2)['avevar', -ncol(reliability(fit2))])) %>% 
  stargazer(type = 'text', title = 'Discriminant Validity: Correlation and AVE', 
            summary = FALSE, digits = 3, digits.extra = 0, rownames = FALSE)


sem <- "# measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions 
dem60 ~ ind60
dem65 ~ ind60 + dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8"

fit <- sem(model = sem, data = PoliticalDemocracy)

fitMeasures(fit, c('chisq', 'df', 'pvalue', 'gfi', 'rmsea', 'cfi'))

standardizedsolution(fit) %>% 
  filter(op == '~') %>% 
  mutate(stars = ifelse(pvalue < 0.001, "***", 
                        ifelse(pvalue < 0.01, "**", 
                               ifelse(pvalue < 0.05, "*", "")))) %>% 
  select(Dependent=lhs, Independent=rhs, Coefficient=est.std, 
         Z=z, "p-value"=pvalue, Sig.=stars) %>% 
  stargazer(type = 'text', title="Regression Coefficients", summary=FALSE, 
            digits=3, digits.extra=0, rownames=FALSE)

lavInspect(fit, what = 'rsquare')

semPaths(fit, what='std', layout = 'tree2', edge.label.cex=1, dege.color='royalblue', 
         color=list(lat='lightcoral', man='lavenderblush'), fade=FALSE, 
         style = 'lisrel', curvature=2)