#Libraries
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(tidyverse)

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!requireNamespace("purrr", quietly = TRUE)) {
  install.packages("purrr")
}
library(purrr)

if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr")
}
library(knitr)
###

data_munic<-read.csv("~/Documents/Econometric challange/data_municipalities.csv")
summary(data_munic)
table(data_munic$percent_vaccinated<50)

data_munic$y <- ifelse(data_munic$percent_vaccinated<50 , 0, 1)
table(data_full_f$y)
#jei daugiau pasiskiepijo --> 1

data_munic$urbrur<-data_munic$municipality_code %% 10
data_munic<-data_munic[data_munic$urbrur != "3", ]
#1=urban, 2=rural

balsavimai<-read_xlsx("~/Documents/Econometric challange/votes.xlsx")
balsavimai<-balsavimai[,c(1,2,3,7)]
names(balsavimai)[names(balsavimai) == 'TERYT Powiatu'] <- 'county_code'
data_munic$county_code <- substr(data_munic$county_code,1,5)
data_munic$county_code<-as.numeric(data_munic$county_code)
balsavimai$county_code <- substr(balsavimai$county_code,1,5)
balsavimai$county_code<-as.numeric(balsavimai$county_code)
data<- balsavimai %>% right_join(data_munic, by=c("county_code"))
names(data)[names(data) == 'Województwo'] <- 'Vaivadija'


geografic<-read_xlsx("~/Documents/Econometric challange/partitions.xlsx")
names(geografic)[names(geografic) == 'Województwo'] <- 'Vaivadija'

data1<- geografic %>% right_join(data, by=c("Vaivadija"))
data11<-subset(data1, !duplicated(subset(data1, select=c(municipality_code))))

data_counties<-read.csv("~/Documents/Econometric challange/data_counties.csv")
data_counties$county_code <- substr(data_counties$county_code,1,5)
data_counties$county_code<-as.numeric(data_counties$county_code)

data11_f<- data_counties %>% right_join(data11, by=c("county_code"))
data11_f<-subset(data11_f, !duplicated(subset(data11_f, select=c(municipality_code))))

names(data11_f)[names(data11_f) == 'Frekwencja'] <- 'Voting'

data_full=subset(data11_f,select = c(y,
                                   urbrur,
                                   population_total,
                                   population_total_m,
                                   population_total_f,
                                   county_name,
                                   Vaivadija,
                                   municipality_name,
                                   `Historical partitions`,
                                   `Eastern/Western`,
                                   education_share_primary,
                                   education_share_vocational,
                                   education_share_secondary,
                                   education_share_higher,
                                   population_5_9_total,
                                   population_10_14_total,
                                   population_15_19_total,
                                   population_20_24_total,
                                   population_25_29_total,
                                   population_30_34_total,
                                   population_35_39_total,
                                   population_40_44_total,
                                   population_45_49_total,
                                   population_50_54_total,
                                   population_55_59_total,
                                   population_60_64_total,
                                   population_65_69_total,
                                   population_70_74_total,
                                   population_75_79_total,
                                   population_80_84_total,
                                   population_density,
                                   healthcare_advices,
                                   bicycle_paths_per_10k_persons,
                                   library_books_per_1000_persons,
                                   unemployed_long_term,
                                   unemployment_rate,
                                   revenues_per_capita,
                                   birthrate_per_1000_persons,
                                   Voting

))

library(fastDummies)
data_full_f<-dummy_cols(data_full,select_columns = c("Historical partitions","Eastern/Western","urbrur"))
data_full_f$unemployed_long_term<-as.numeric(data_full_f$unemployed_long_term)
data_full_f$Voting<-as.numeric(gsub(",", ".", gsub("\\.", "", data_full_f$Voting)))

options(scipen = 9999)

boxplot(data_full_f$education_share_primary)
summary(data_full_f$education_share_primary)
out <- boxplot.stats(data_full_f$education_share_primary)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
data_full_f<-data_full_f[data_full_f$education_share_primary>8.64,]

boxplot(data_full_f$education_share_secondary)
summary(data_full_f$education_share_secondary)
out <- boxplot.stats(data_full_f$education_share_secondary)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
data_full_f<-data_full_f[data_full_f$education_share_secondary<40.54,]   

boxplot(data_full_f$education_share_vocational)
summary(data_full_f$education_share_vocational)
out <- boxplot.stats(data_full_f$education_share_vocational)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
data_full_f<-data_full_f[data_full_f$education_share_vocational>15.91,] 
data_full_f<-data_full_f[data_full_f$education_share_vocational<33.12,] 

boxplot(data_full_f$education_share_higher)
summary(data_full_f$education_share_higher)
out <- boxplot.stats(data_full_f$education_share_higher)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
data_full_f<-data_full_f[data_full_f$education_share_higher<23.7,] 

data_full_f <- transform(data_full_f, age_5_9 = round(population_5_9_total / population_total,2))
data_full_f <- transform(data_full_f, age_10_14 = round(population_10_14_total / population_total,2))
data_full_f <- transform(data_full_f, age_15_19 = round(population_15_19_total / population_total,2))
data_full_f <- transform(data_full_f, age_20_24 = round(population_20_24_total / population_total,2))
data_full_f <- transform(data_full_f, age_25_29 = round(population_25_29_total / population_total,2))
data_full_f <- transform(data_full_f, age_30_34 = round(population_30_34_total / population_total,2))
data_full_f <- transform(data_full_f, age_35_39 = round(population_35_39_total / population_total,2))
data_full_f <- transform(data_full_f, age_40_44 = round(population_40_44_total / population_total,2))
data_full_f <- transform(data_full_f, age_45_49 = round(population_45_49_total / population_total,2))
data_full_f <- transform(data_full_f, age_50_54 = round(population_50_54_total / population_total,2))
data_full_f <- transform(data_full_f, age_55_59 = round(population_55_59_total / population_total,2))
data_full_f <- transform(data_full_f, age_60_64 = round(population_60_64_total / population_total,2))
data_full_f <- transform(data_full_f, age_65_69 = round(population_65_69_total / population_total,2))
data_full_f <- transform(data_full_f, age_70_74 = round(population_70_74_total / population_total,2))
data_full_f <- transform(data_full_f, age_75_79 = round(population_75_79_total / population_total,2))
data_full_f <- transform(data_full_f, age_80_84 = round(population_80_84_total / population_total,2))

data_full_f <- transform(data_full_f, age_5_19 = round((age_5_9+age_10_14+age_15_19)*100,2))
data_full_f <- transform(data_full_f, age_20_39 = round((age_20_24+age_25_29+age_30_34+age_35_39)*100,2))
data_full_f <- transform(data_full_f, age_40_64 = round((age_40_44+age_45_49+age_50_54+age_55_59+age_60_64)*100,2))
data_full_f <- transform(data_full_f, age_65_84 = round((age_65_69+age_70_74+age_75_79+age_80_84)*100,2))

boxplot(data_full_f$age_5_19)
summary(data_full_f$age_5_19)
out <- boxplot.stats(data_full_f$age_5_19)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
data_full_f<-data_full_f[data_full_f$age_5_19>10,]
data_full_f<-data_full_f[data_full_f$age_5_19<23,] 

boxplot(data_full_f$age_20_39)
summary(data_full_f$age_20_39)
out <- boxplot.stats(data_full_f$age_20_39)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
data_full_f<-data_full_f[data_full_f$age_20_39>22,]
data_full_f<-data_full_f[data_full_f$age_20_39<33,] 

boxplot(data_full_f$age_40_64)
summary(data_full_f$age_40_64)
out <- boxplot.stats(data_full_f$age_40_64)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
data_full_f<-data_full_f[data_full_f$age_40_64>28,] 
data_full_f<-data_full_f[data_full_f$age_40_64<41,] 

boxplot(data_full_f$age_65_84)
summary(data_full_f$age_65_84)
out <- boxplot.stats(data_full_f$age_65_84)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
data_full_f<-data_full_f[data_full_f$age_65_84<24,] 

boxplot(data_full_f$unemployment_rate)
summary(data_full_f$unemployment_rate)
out <- boxplot.stats(data_full_f$unemployment_rate)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
out
data_full_f<-data_full_f[data_full_f$unemployment_rate<12,]
 
boxplot(data_full_f$Voting)

data_full_f <- transform(data_full_f, hc_advices_per_capita = round(healthcare_advices / population_total,2))
boxplot(data_full_f$hc_advices_per_capita)
summary(data_full_f$hc_advices_per_capita)
out <- boxplot.stats(data_full_f$hc_advices_per_capita)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
min(out)
data_full_f<-data_full_f[data_full_f$hc_advices_per_capita<6.14,]
data_full_f<-data_full_f[data_full_f$hc_advices_per_capita>0.12,]

boxplot(data_full_full$population_density)
summary(data_full_f$population_density)
out <- boxplot.stats(data_full_full$population_density)$out
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
min(out)
data_full_full<-data_full_f[data_full_f$population_density<103.9,]

data_full_f<-data_full_f[!is.na(data_full_f$y),]
summary(data_full_f)

data_full_f <- transform(data_full_f, Female = round((population_total_f / population_total)*100,2))
data_full_f <- transform(data_full_f, Male = round((population_total_m / population_total)*100,2))

stat<-data_full_f %>%
  group_by(y,Female)%>%
  summarise(v=n())

data_full_f <- transform(data_full_f, vaivadija_1 = Vaivadija_śląskie+Vaivadija_małopolskie)
data_full_f <- transform(data_full_f, vaivadija_2 = Vaivadija_mazowieckie+Vaivadija_dolnośląskie+Vaivadija_łódzkie+Vaivadija_pomorskie)
data_full_f <- transform(data_full_f, vaivadija_3 = Vaivadija_podkarpackie+Vaivadija_wielkopolskie+Vaivadija_kujawsko.pomorskie+Vaivadija_świętokrzyskie+Vaivadija_opolskie)
data_full_f <- transform(data_full_f, vaivadija_4 = Vaivadija_lubelskie+Vaivadija_zachodniopomorskie+Vaivadija_lubuskie+Vaivadija_warmińsko.mazurskie+Vaivadija_podlaskie)


#Best model
data_full_f<-dummy_cols(data_full_f,select_columns = c("Vaivadija"))
model_probit<-glm(y~urbrur_1+
                    vaivadija_1+
                    vaivadija_2+
                    vaivadija_3+
                    `Historical.partitions_austrian`+
                    `Historical.partitions_russian`+
                    education_share_primary+
                    education_share_vocational+
                    education_share_secondary+
                    education_share_higher+
                    age_20_39+
                    age_40_64+
                    age_65_84+
                    hc_advices_per_capita+
                    unemployment_rate+
                    birthrate_per_1000_persons+
                    Voting, data = data_full_f,family = binomial(link = 'probit')
)
summary(model_probit)
#AME
model_probit_pred<-mean(dnorm(predict(model_probit, type = "link")))
Probit_prob<-as.data.frame(model_probit_pred*coef(model_probit))*100
Probit_variables<-summary(model_probit)$coefficients %>% as.data.frame %>% rownames_to_column() %>% select(rowname) 
Probit<-cbind(Probit_variables,Probit_prob)
names(Probit)[2] <- "Probit_prob"

var<-list(model_probit)
var <- var %>% mutate(across(c('Probit_prob'), round, 2))
kable(Probit, "html") %>%
  cat(., file = "var.html")

write_csv(data_full_f,"data_full.csv")
