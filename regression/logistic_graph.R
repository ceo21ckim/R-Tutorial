# logistic ----
if (!require('dplyr')) install.packages('dplyr')
if (!require('tidyverse')) install.packages('tidyverse')
library(dplyr) ; library(tidyverse)

te <- data.frame(seq(-4, 4, by = 0.1))
names(te) <- c('x')

te <- te %>% 
  mutate( y1 = 1/(1+exp(-x)))

te

te %>% 
  ggplot(aes(x, y1)) + 
  geom_line() +
  geom_vline(xintercept = 0.0, 
             linetype = 'dashed') + 
  geom_hline(yintercept = c(0, 1.0),
             color = 'blue',
             alpha = 0.5,
             linetype = 'dashed') + 
  ggtitle('Logistic & CDF') +
  xlab('x') +
  ylab('y') +
  scale_color_brewer(palette = 'Set1')