if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

if (!requireNamespace("lme4", quietly = TRUE)) {
  install.packages("lme4")
}
library(lme4)

data <- read_excel("data.xlsx")
attach(data)
data<-na.omit(data)
class(data$percent_vaccinated)
data$percent_vaccinated<-as.numeric(data$percent_vaccinated)
class(data$percent_vaccinated)
N.effect<-lmer(percent_vaccinated~ population_density+(1|county_code), data = data)
summary(N.effect)