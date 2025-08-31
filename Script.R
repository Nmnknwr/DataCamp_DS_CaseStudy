library(readr)
library(tidyverse)
library(skimr)
library(data.table)
library(clustMixType)
library(ggplot2)
library(zoo)


coffee_shops <- fread("/Users/namankanwar/Case Studys/DataCamp_DS_CaseStudy/Data/coffee_shops.csv")


class(coffee_shops)


skim(coffee_shops)

table(coffee_shops$`Place type`)

coffee_shops <- coffee_shops[.(`Place type`=c("Cafe","Coffee roasters","Coffee shop","Coffee store","Espresso bar","Coffee stand","Store"),to="Coffee shop"),on="Place type",`Place type`:=i.to]

coffee_shops <- coffee_shops %>%
  filter(`Place type`=="Coffee shop") %>% 
  select(-c(`Place type`))


coffee_shops$`Dine in option` <- replace_na(coffee_shops$`Dine in option`,FALSE)

coffee_shops$`Takeout option` <- replace_na(coffee_shops$`Takeout option`,FALSE)
coffee_shops$`Delivery option` <- replace_na(coffee_shops$`Delivery option`,FALSE)

coffee_shops$Region <- as.factor(coffee_shops$Region)
coffee_shops$`Delivery option` <- as.factor(coffee_shops$`Delivery option`)
coffee_shops$`Dine in option` <- as.factor(coffee_shops$`Dine in option`)
coffee_shops$`Takeout option` <- as.factor(coffee_shops$`Takeout option`)

skim(coffee_shops)

table(coffee_shops$Price)

coffee_shops$Price[coffee_shops$Price==''] <- NA

table(coffee_shops$Price)

coffee_shops %>% 
  ggplot(aes(Price)) +
  geom_bar()

coffee_shops$Price <- replace_na(coffee_shops$Price,"$$")

table(coffee_shops$Price)

coffee_shops$Price <- ordered(coffee_shops$Price,levels=c("$","$$","$$$"))

cols <- c("Rating","Reviews")

coffee_shops[, (cols) := lapply(.SD, function(x) nafill(x, type = "const", fill = mean(x, na.rm = TRUE)))
             , by = Region
             , .SDcols = cols][] 

coffee_shops$Points <- as.numeric(coffee_shops$Rating*coffee_shops$Reviews)

skim(coffee_shops)

for_clustering <- coffee_shops %>% 
  select(-c("Rating","Reviews","Region","Place name","Price"))

for_clustering$Points <- scale(for_clustering$Points)

wss <- numeric(10) 
set.seed(26)
for(i in 1:10){
  kpres <-kproto(for_clustering, k = i, nstart = 25) 
  wss[i] <-kpres$tot.withinss 
} 

plot(1:10, wss, type = "b", ylab = "Objective Function", xlab = "# Clusters", main = "Scree Plot")

set.seed(26)
clusters <- kproto(coffee_shops,k=5,nstart=25)

coffee_shops$cluster <- clusters$cluster


(coffee_shops %>% 
    group_by(cluster) %>% 
    summarise(Count = n(),
              Avg_Rating = mean(Rating),
              Avg_Reviews = mean(Reviews),
              Delivery_pct = round(sum(`Delivery option`=="TRUE")/n()*100),
              Takeout_pct = round(sum(`Takeout option`=="TRUE")/n()*100),
              Dine_in_pct = round(sum(`Dine in option`=="TRUE")/n()*100)))
