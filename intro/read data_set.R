# load data ----
# txt 파일은 read.table()로 읽읅 수 있다.

df.txt <- read.table('sample.csv',
                     header = TRUE,
                     sep = ',')

df.csv <- read.csv('sample.csv', 
                   header = TRUE,
                   sep = ',')

# 속도면에서 read.csv보다 빠르다.
if (!require('readr')) install.packages('readr')
library(readr)

df.csv2 <- read_csv('sample.csv')

class(df.csv2)


if (!require('readxl')) install.packages('readxl')
library(readxl)

df.xlsx <- read_excel('file')

# 직접 파일을 클릭해서 가져올 수 있다.
df.xlsx.2 <- read_excel(file.choose())

if (!require('foreign')) install.packages('foreign')
if (!require('haven')) install.packages('haven')
library(foreign) ; library(haven)

# stata file을 읽어들일때 사용한다.
df.sata <- haven::read_dta('file.dta')

