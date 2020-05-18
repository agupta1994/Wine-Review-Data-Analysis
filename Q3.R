library(tidyverse)
Data<- read.csv("../input/wine-reviews/winemag-data-130k-v2.csv")
Data<-filter(Data, country == "US")

#Cleaning the data
sum(is.na(Data))
wine_data_us <- na.omit(Data)
wine_data_us <- wine_data_us%>%
extract(title,'year',"(20\\d\\d)",convert = T,remove = F)%>%
mutate(year=ifelse(year<1900,NA,year))

#Random Samples generation
wine_data_us<-sample_n(wine_data_us, 10000)

# analysing features
wine_data_us%>%
mutate(region_1=fct_relevel(fct_lump(region_1,7),"US"))%>%
mutate(region_1=fct_reorder(region_1,points))%>%
ggplot(aes(region_1,points))+geom_boxplot()+coord_flip()
wine_data_us%>%
mutate(reviwer=fct_reorder(fct_lump(taster_name,10),points))%>%
ggplot(aes(reviwer,points))+geom_boxplot()+coord_flip()

#factorizing categorical features
wine_data_us$province<-factor(wine_data_us$province)
wine_data_us$region_1<-factor(wine_data_us$region_1)
wine_data_us$region_2<-factor(wine_data_us$region_2)
wine_data_us$variety<-factor(wine_data_us$variety)
wine_data_us$winery<-factor(wine_data_us$winery)
wine_data_us$taster_name<-factor(wine_data_us$taster_name)

#getting R-Squared values for different features
lm1 <- lm(points ~ province, wine_data_us)
summary(lm1)
lm2 <- lm(points ~ region_1, wine_data_us)
summary(lm2)
lm3 <- lm(points ~ region_2, wine_data_us)
summary(lm3)
lm4 <- lm(points ~ variety, wine_data_us)
summary(lm4)
lm5 <- lm(points ~ winery, wine_data_us)
summary(lm5)
lm6 <- lm(points ~ taster_name, wine_data_us)
summary(lm6)
lm8 <- lm(points ~ price, wine_data_us)
summary(lm8)
lm9 <- lm(points ~ year, wine_data_us)

#model using best 4 features
lm_final <- lm(points ~ winery+price+region_1+taster_name, wine_data_us)
summary(lm_final)
plot(lm_final)

#model using all the features
lm_new <-lm(points~winery+price+region_1+taster_name+province+price+region_2+variety+year, wine_data_us)
summary(lm_new)
plot(lm_new)

#model using random features
lm_1 <- lm(points ~ winery+price+region_2+taster_name+year+province, wine_data_us)
summary(lm_1)
lm_2 <- lm(points ~ winery+price+region_1+region_2, wine_data_us)
summary(lm_2)
lm_3 <- lm(points ~ province+region_1+price, wine_data_us)
summary(lm_3)

#getting AIC for all models
step_AIC_backward <- step(lm_final)
step_AIC_backward <- step(lm_new)
step_AIC_backward <- step(lm_1)
step_AIC_backward <- step(lm_2)
step_AIC_backward <- step(lm_3)

#generating plots
library(broom)
plot_coeff <-
lm_final %>%
tidy(conf.int = TRUE)%>%
filter(term!="(Intercept)")%>%
mutate(term=str_replace(term,'taster_name',"Taster: "),term=fct_reorder(term,estimate))%>%
ggplot(aes(estimate,term))+geom_point()+
geom_errorbarh(aes(xmin =conf.low,xmax =conf.high))
plot_coeff