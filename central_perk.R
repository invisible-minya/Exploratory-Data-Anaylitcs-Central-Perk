library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)
library(rmarkdown)
library(plyr)
library(tidyr)
library(gridExtra)
library(stringr)
library(lubridate)

# 1. data cleaning part
cp_2016 = fread('Central Perk Item Sales Summary 2016.csv', stringsAsFactors = FALSE)
cp_2017 = fread('Central Perk Item Sales Summary 2017.csv', stringsAsFactors = FALSE)
cp_2018 = fread('Central Perk Item Sales Summary 2018.csv', stringsAsFactors = FALSE)

cp = rbind(cp_2016, cp_2017, cp_2018)


clean_cp <- cp %>%
  filter(!(Event.Type == "Refund")) %>%
  filter(!is.na(Qty)) %>%
  filter(!(Date == 'Unknown Error')) %>%
  mutate(Item = as.character(Item)) %>%
  filter(Item != 'Custom Amount' & Category != 'None') %>%
  mutate(Item = str_trim(str_replace(Item, "SM$", ""))) %>%
  mutate(Item = str_trim(str_replace(Item, "LG$", ""))) %>%
  mutate(Item = str_replace(Item, "ðY\u008d<LemonadeðY\u008d<", "Lemonade")) %>%
  mutate(Timestamp = as.POSIXct(paste(Date, Time), format="%m/%d/%y %H:%M:%S"),
         Hour = factor(hour(Timestamp)),
         Month = factor(months(Timestamp,abbreviate=TRUE)),
         Weekday = factor(weekdays(Timestamp,abbreviate=TRUE)) %>%
  mutate(Price.Point.Name = str_replace(Price.Point.Name, "Regular Price", "Regular")))

clean_cp$Customer.ID <- ifelse(is.na(clean_cp$Customer.ID),'Non Membership', clean_cp$Customer.ID)
clean_cp[clean_cp$Item %like% "Lemon",]$Item <- "Lemonade"
clean_cp$Gross.Sales <- str_remove_all(clean_cp$Gross.Sales, "[$()]")
clean_cp$Discounts <- str_remove_all(clean_cp$Discounts, "[$]")
clean_cp$Net.Sales <- str_remove_all(clean_cp$Net.Sales, "[$()]")
clean_cp$Tax <- str_remove_all(clean_cp$Tax, "[$()]")

summary(as.factor(clean_cp$Item))

clean_cp$Timestamp = as.character(clean_cp$Timestamp)

write.csv(clean_cp,file = "clean_cp.csv",row.names=FALSE)

# 2. label loyal customers from rfm analysis
loyal_label <- fread("rfm_cat.csv",stringsAsFactors = FALSE)
colnames(clean_cp)[13] <- 'customer_id'
clean_cp_loyal<- merge(x=clean_cp, y =loyal_label, by="customer_id", all.x = TRUE)

# 3. filter out customers buying extras and filter the dataset with customers who have multiple purchases, preparing for
# association rules analysis
multi_buy <- clean_cp
multi_buy <- clean_cp[clean_cp$Category != "Extras",]
multi_buy <- multi_buy[multi_buy$Timestamp %in% multi_buy[duplicated(multi_buy$Timestamp),]$Timestamp,]
multi_buy$quarter <- 0
multi_buy[multi_buy$Month %in% c('October','November','December'),]$quarter <- 4
multi_buy[multi_buy$Month %in% c('January','February','March'),]$quarter <- 1
multi_buy[multi_buy$Month %in% c('April','May','June'),]$quarter <- 2
multi_buy[multi_buy$Month %in% c('July','August','September'),]$quarter <- 3

multi_buy$Combine <- paste(multi_buy$Item, multi_buy$Weekday)

multi_buy <- multi_buy%>%
            filter(loyalty == 'Semi-Loyal')

# 4. transfer rows into columns

library(data.table)

transform_data <- multi_buy %>%
            select(Timestamp, Item)

combine <- dcast(setDT(transform_data), Timestamp ~ rowid(Timestamp),value.var ="Item")

combine$Hour <- hour(combine$Timestamp)
combine$Weekday<- weekdays(combine$Timestamp)
combine$Month <- months(combine$Timestamp)

combine$quarter <- 0
combine[combine$Month %in% c('October','November','December'),]$quarter <- 4
combine[combine$Month %in% c('January','February','March'),]$quarter <- 1
combine[combine$Month %in% c('April','May','June'),]$quarter <- 2
combine[combine$Month %in% c('July','August','September'),]$quarter <- 3
combine$day <- 0
combine[combine$Weekday %in% c('Monday','Tuesday','Wednesday','Thursday','Friday'),]$day <- 0
combine[combine$Weekday %in% c('Saturday','Sunday'),]$day <- 1
  
# 5. arules

library("arules")

rule_data <- combine %>%
  select(Hour, c(1:13),-Timestamp)


write.csv(rule_data,file = "all_transactions_month.csv",row.names=FALSE)

rule_data <- read.csv("all_transactions_month.csv")
orders = read.transactions(
  file = 'all_transactions_month.csv', header=TRUE,
  format = "basket",
  sep = ",",
  cols = c("Timestamp","Combine"),
  rm.duplicates = T)

summary(orders)
inspect(orders)

rules <- apriori(orders, parameter = list(supp = 0.01, conf = 0.01,minlen=1), appearance = list(lhs =unique(rule_data$Month)))
inspect(rules)

sorted_rules<- head(sort(rules, decreasing = TRUE, by = c("lift")),10)
inspect(sorted_rules)
        
#library(arulesViz)
#plot(rules, method="grouped")

  
  
  
