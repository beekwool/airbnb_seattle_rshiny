library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)



#import data
arbnb.raw = read_csv('arbnb.csv')
unemp.raw = read_csv('BLSseattle2019_2020.csv')
weather.raw = read_csv('road-weather-information-stations.csv')

#aggregate to monthly data

#arbnb data - monthly
arbnb.raw$price = substring(arbnb.raw$price, 2)

#monthly total unique listings
arbnb_list = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         price = as.numeric(price)) %>% 
  filter(!is.na(price), year == 2020) %>% 
  select(year, month, listing_id) %>% 
  unique() %>% 
  group_by(year, 
           month) %>% 
  summarise(list_cnt = n_distinct(listing_id))
arbnb_list

#vacancy
arbnb_vacnt = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         price = as.numeric(price)) %>% 
  filter(!is.na(price), year == 2020) %>% 
  group_by(year, month) %>%
  summarise(vacancy_rate = sum(available==TRUE)/n())



ggplot(arbnb_vacnt, aes(month, vacancy_rate)) + 
  geom_bar(stat = 'identity') +
  geom_smooth(se = F)+
  labs(title='Listing Vacancy by Month in Seattle', x='Month', y='VacancyRate') +
  scale_x_continuous(breaks = 1:12) +
  theme_bw() 
       


#Listing vacancy by day
listing_by_day = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         day_of_year = yday(date),
         price = as.numeric(price)) %>% 
  filter(!is.na(price), year == 2020) %>% 
  group_by(day_of_year, 
           available) %>% 
  summarise(list_cnt = n()) %>% 
  filter(available == TRUE)


ggplot(listing_by_day, aes(day_of_year, list_cnt)) +
  geom_freqpoly(stat='identity') +
#  coord_cartesian(xlim=c(1,12)) +
  labs(title='Count of Vacant Listing by day in Seattle', x='Day', y='Listings') +
  #scale_x_continuous(breaks = 1:12) +
  theme_bw() 


#vacancy rate vs pricing
arbnb_aval_price = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         price = as.numeric(price)) %>% 
  filter(!is.na(price), year == 2020) %>% 
  select(year, month, listing_id) %>% 
  unique() %>% 
  group_by(year, 
           month) %>% 
  summarise(vacancy_rate = sum(available==T)/n(),
            ave_price = mean(price))

ggplot(arbnb_avail, aes(month, avg_price, col= Availability)) +
  geom_point(aes(color = factor(available))) +
  labs(title='Price by Listing Availability Each Month in Seattle', x='Month', y='Average Price') +
  scale_x_continuous(breaks = 1:12) +
  theme_bw() 
  


#average price by month
vacancy_vs_price = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         price = as.numeric(price)) %>% 
  filter(!is.na(price), year == 2020) %>% 
  group_by(month) %>% 
  summarise(vacancy_rate = sum(available == TRUE)/n(), 
            days_avail = sum(available == TRUE),
            listing_days =n(),
            avg_price = mean(price)) #%>%  
top_n(20, avg_price)


ggplot(vacancy_vs_price, aes(month, avg_price)) + 
  geom_bar(stat = 'identity') +
  labs(title='Price by  Month in Seattle', x='Month', y='Average Price') +
  scale_x_continuous(breaks = 1:12) +
  theme_bw() 

ggplot(arbnb_avail, aes(month, avg_price)) +
  geom_point() +
  labs(title='Price by Listing Availability Each Month in Seattle', x='Month', y='Average Price') +
  scale_x_continuous(breaks = 1:12) +
  theme_bw() 

#availability based on average price
arbnb_avail = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         price = as.numeric(price),
         no_max_stay = maximum_nights>30,
         no_min_stay = minimum_nights == 1,
         min_stay2 = minimum_nights == 2,
         min_stay3 = minimum_nights == 3) %>% 
  filter(!is.na(price), year == 2020) %>% 
  group_by(year, 
           month,
           available) %>% 
  summarise(avg_price = mean(price), 
            list_cnt = n()) %>% head(100)


ggplot(arbnb_avail, aes(month, avg_price, col= Availability)) +
  geom_point(aes(color = factor(available))) +
  labs(title='Price by Listing Availability Each Month in Seattle', x='Month', y='Average Price') +
  scale_x_continuous(breaks = 1:12) +
  theme_bw() 

#the lower the price the less available it is

  
#seeing if min stays have effect on pricing
arbnb_no_min_stay = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         price = as.numeric(price),
         no_max_stay = maximum_nights>30,
         no_min_stay = minimum_nights == 1,
         min_stay2 = minimum_nights == 2,
         min_stay3 = minimum_nights == 3) %>% 
  filter(!is.na(price), year == 2020) %>% 
  group_by(year, 
           month,
           no_min_stay) %>% 
  summarise(avg_price = mean(price), 
            sd_price = sd(price),
            avg_min_nights = mean(minimum_nights), 
            avg_max_nights = mean(maximum_nights),
            min_nights = min(minimum_nights),
            maximum_nights = max(maximum_nights),
            list_cnt = n())

ggplot(arbnb_no_min_stay, aes(month, avg_price, col= NoMinNights)) +
  geom_point(aes(color = factor(no_min_stay))) +
  labs(title='Price by Min Stay Requirement Each Month in Seattle', x='Month', y='Average Price') +
  scale_x_continuous(breaks = 1:12) +
  theme_bw() 
#the less restriction on min stay the higher the price is

#comparing min stay
arbnb_min_stay_compare = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         price = as.numeric(price)) %>% 
  filter(!is.na(price), year == 2020, minimum_nights <=7) %>% 
  group_by(year, 
           month,
           minimum_nights) %>% 
  summarise(avg_price = mean(price), 
            sd_price = sd(price),
            list_cnt = n())
ggplot(arbnb_min_stay_compare, aes(month, avg_price, col= minimum_nights)) +
  geom_smooth(se = F, aes(color = factor(minimum_nights))) +
  labs(title='Price by Min Stay Requirement Each Month in Seattle', x='Month', y='Average Price') +
  scale_x_continuous(breaks = 1:12) +
  theme_bw() 
#no min stay has the best price





#percent of vacancy by day
vacancy_by_day = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         day_of_year = yday(date),
         price = as.numeric(price)) %>% 
  filter(!is.na(price), year == 2020) %>% 
  group_by(month, 
           day_of_year) %>% 
  summarise(vacancy_rate = sum(available == TRUE)/n())

ggplot(vacancy_by_day, aes(day_of_year, vacancy_rate)) +
  geom_bar(stat = 'identity')+
  labs(title='Listing Vacancy in Seattle', x='Days', y='Listings') +
  geom_freqpoly(stat='identity') +
  theme_bw() 

#best performed listings:  
popular_listing = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         price = as.numeric(price)) %>% 
  filter(!is.na(price), year == 2020) %>% 
  group_by(listing_id) %>% 
  summarise(vacancy_rate = sum(available == TRUE)/n(), 
            days_avail = sum(available == TRUE),
            listing_days =n(),
            avg_price = mean(price),
            avg_min_stay = mean(minimum_nights)) %>% 
  #assuming 0 available days are inactive
  filter(vacancy_rate < 0.1, vacancy_rate > 0) %>% 
  #top_n(250, desc(vacancy_rate)) %>% 
  arrange(vacancy_rate)

ggplot(popular_listing, aes(vacancy_rate, avg_price)) + geom_point()
ggplot(popular_listing, aes(vacancy_rate, avg_min_stay)) + geom_point()
#can't find anything



#Profit based on occupancy and min nights
arbnb_profit = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         price = as.numeric(price),
         profit = as.numeric(price)*available) %>% 
  filter(!is.na(price), year == 2020, minimum_nights <=14, available == FALSE) %>% 
  group_by(listing_id, minimum_nights) %>% 
  summarise(profit = sum(price)) %>% 
  group_by(minimum_nights) %>% 
  summarise(avg_profit = mean(profit)) 

ggplot(arbnb_profit, aes(minimum_nights, avg_profit)) + 
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = 1:14) +
  labs(title='Average Total Profit by Minimum Nights', x='Min Nights', y='Average Profit') +
  theme_bw() #+
  #annotate("rect", xmin = 4, xmax = 5, ymin = 0, ymax = 0.13, alpha = .2, fill='darkred') 
#min nights = 4 or 5


#Most profitable by month
arbnb_profit_by_month = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         price = as.numeric(price),
         profit = as.numeric(price)*available) %>% 
  filter(!is.na(price), year == 2020, available == FALSE) %>% 
  group_by(month, listing_id) %>% 
  summarise(profit = sum(price))  %>% 
  group_by(month) %>% 
  summarise(avg_profit = mean(profit)) 


ggplot(arbnb_profit_by_month, aes(month, avg_profit)) + 
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = 1:14) +
  labs(title='Average Total Profit by Minimum Nights', x='Min Nights', y='Average Profit') +
  theme_bw()




#unemployment data
names(unemp.raw) = gsub(' ', '_', names(unemp.raw))
names(unemp.raw) = tolower(names(unemp.raw))

unemp.raw %>% 
  mutate(month = match(unemp.raw$period, month.abb),
         un_rt = unemployment/labor_force) %>% 
  filter(year == 2020)


head(arbnb.raw)
str(arbnb.raw) #3,293,395
summary(arbnb.raw)
summary(unemp.raw)
summary(weather.raw)
head(unemp.raw, 24)
head(weather.raw)




#time related data/weather maybe


summary = arbnb.raw %>% 
  mutate(year = year(date), 
         month = month(date),
         day_of_year = yday(date),
         price = as.numeric(price)) %>% 
  filter(!is.na(price), year == 2020) %>% 
  group_by(year, 
           month,
           day_of_year,
           available) %>% 
summarise(avg_price = mean(price), 
          avg_min_nights = mean(minimum_nights), 
          avg_max_nights = mean(maximum_nights),
          min_nights = min(minimum_nights),
          maximum_nights = max(maximum_nights),
          listings = n())

summary
ggplot(summary, aes(day_of_year, listings)) + 
  #geom_point(aes(color = available)) + 
  geom_smooth(aes(color = available), se = F)




#%>%  warnings()
# , 
# min_night = min(minimum_nights), 
# max_night = max(maximum_nights)) %>% warnings()