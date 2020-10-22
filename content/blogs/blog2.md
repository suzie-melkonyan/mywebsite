---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Airbnb
draft: false
image: grand-place-flower-carpet.jpg
keywords: ""
slug: blog2
title: Airbnb analytics for Brussels
---

Analyzing Airbnb dataset for Brussels

We are going to study on the factors that would affect the accommodation price on Airbnb in Brussels, and make a prediction on the total cost for 2 people staying 4 nights using proper model.


```{r load-libraries, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(tidyquant)
library(infer)
library(openintro)
library(tidyquant)
library(readr)
library(kableExtra)
library(moderndive)
library(car)
library(ggfortify)
library(huxtable)
library(leaflet)
```

First of all, we performed exploratory data analysis to get an overall understanding of the data and examine the data in more detail. Next, we did some mappings to show our key insights. Then we construct the models, analyse the results and  wrap it up in our conclusion.


# EDA 

Before we start, we need to import the data and conduct some basic exploratory data analysis.

## Raw Data

```{r}
#Import data
listings_csv <- vroom::vroom("http://data.insideairbnb.com/belgium/bru/brussels/2020-06-15/data/listings.csv.gz")

#save as another dataset
listings <- listings_csv

#take a glimpse of all the data
glimpse(listings)

#summary statistics
skimr::skim(listings)

```

Overall, there are 106 variables and 8986 observations in the initial data. Among all of those, 39 variables are stored as numbers, including the IDs, host listings counts, review scores ratings,number of reviews, number of rooms and beds, accommodates, number of guests, longitude, latitude etc. 

What's more, we could get the categorical factors from the results above, such as the experiences_offered, host_location, host_is_superhost, host_has_profile_pic, host_identity_verified, neighborhood_cleansed, city, state, market, smart_location, country, is_location exact, property_type, room_type, bed_type, has_availability, requires_license, instant_bookable, is_business_travel_ready, require_guest_profile_picture, require_guest_phone_verification etc. 
Basically, these are the factors related to the host, the location and other requirements.


## Data wrangling

We notice that all the value for price is stored as characters rather than numbers. Therefore, we change the those values into numbers.

```{r}
#Data wrangling

#change the price values from strings to numbers
listings <- listings %>% 
  mutate(price = parse_number(price),
         weekly_price=parse_number(weekly_price),
         monthly_price=parse_number(monthly_price),
         security_deposit=parse_number(security_deposit),
         cleaning_fee = parse_number(cleaning_fee),
         extra_people=parse_number(extra_people))

skimr::skim(listings)
```

## Select the variables and handling NAs

We select the variables we believe are the most relevant to our analysis from the perspective of hosts, location, property and reviews, and then filter out those could only accommodate 1 because we have 2 persons.

```{r select-variables}
listings <- listings%>%
  
  # we select the variables we believe are the most relevant to our analysis.
  select(#id
         listing_url,
          
         #hosts
         host_id,
         host_since,
         host_response_rate,
         host_response_time,
         host_acceptance_rate,
         host_is_superhost,
         host_has_profile_pic,
         host_identity_verified,
         
         #location
         neighbourhood_cleansed,
         latitude,
         longitude,
         is_location_exact,
         neighbourhood,
         
         #property
         property_type,
         room_type,
         accommodates,
         bathrooms,
         bedrooms,
         beds,
         price,
         security_deposit,
         cleaning_fee,
         guests_included,
         extra_people,
         minimum_nights,
         maximum_nights,
         
         #reviews
         number_of_reviews,
         review_scores_rating,
         review_scores_accuracy,
         review_scores_checkin,
         review_scores_cleanliness,
         review_scores_communication,
         review_scores_location,
         review_scores_value,
         cancellation_policy)%>%
  
  #filter for those accommodate more than 1
  filter(accommodates>1)

skim(listings)

```

As we can see from the results above, there are 2211 missing values in cleaning_fee and 3102 missing values in Security_deposit, which indicates that the cleaning fee and the security deposit are not applicable in those cases. Therefore, we could change the NAs into 0 as follows.

```{r handling_the_NAs}

#handling NAs in cleaning fees and security deposit
listings <- listings %>%
  mutate(cleaning_fee = case_when(
    is.na(cleaning_fee) ~ 0, 
    TRUE ~ cleaning_fee),
    security_deposit = case_when(
    is.na(security_deposit) ~ 0, 
    TRUE ~ cleaning_fee))

```

Also, we found that there are some observations with missing values in bedroom and bathroom. It could be something like a studio with no extra bedroom or bathroom in the property. Therefore, we change those NAs into 0 as well.

```{r NAs_in_the_rooms}

#handling NAs in bedrooms and bathrooms
listings <- listings %>%
  mutate(bedrooms = case_when(
    is.na(bedrooms) ~ 0, 
    TRUE ~ bedrooms),
    bathrooms = case_when(
    is.na(bathrooms) ~ 0, 
    TRUE ~ bathrooms))

```


As for those missing values in host_since and the reviews, we just filter those things out.

```{r}

#filter out those with NAs in host_since and the review data
listings <- listings %>%
  drop_na(host_since,
          review_scores_rating,
          review_scores_accuracy,
          review_scores_checkin,
          review_scores_cleanliness,
          review_scores_communication,
          review_scores_location,
          review_scores_value)

```

After that, we also simplified the property types into 5 categories. 4892 of the properties are apartments, which is 73.22% of the total listings in Brussels. There are 529 houses and 314 condominiums on Airbnb as well. In this case, we keep the top 4 types of properties---apartment,house,condominium,townhouse--and classify the other types by a new category called "Others" to construct the simplified property type as follows.

```{r property_types_distribution}

#find the top 4 property types
top_4_p<- listings%>%
  count(property_type)%>%
  arrange(desc(n))%>%
  mutate(proportion=n/sum(n))
top_4_p

#apply simplified property types into 5 categories
listings <- listings %>%
  mutate(prop_type_simplified = case_when(
    property_type %in% c("Apartment","House", "Condominium","Townhouse") ~ property_type, 
    TRUE ~ "Other"))

category_5_p<-listings%>%
  count(prop_type_simplified)%>%
  mutate(prop_type_simplified=factor(prop_type_simplified,order=TRUE,levels = c("Apartment","House", "Condominium","Townhouse","Other")),
         proportion=n/sum(n))
  
ggplot(category_5_p,aes(x=prop_type_simplified,y=n))+
  geom_col(fill="Pink")+
  labs(title="Simplified property type distribution",
       subtitle="Simplified property types on Airbnb in Brussels",
       x="Simplified Property Type",
       y="Numbers")+
  theme_bw()+
  NULL

```


As for the minimum nights, Airbnb is most commonly used for travel purposes, and we only want to include listings in our regression analysis that are intended for travel purposes. Therefore, we took a look at the most common values for minimum nights in Brussels.

```{r minimum_nights_distribution}

#find the most common values for minimum_night
min_n <- listings%>%
  count(minimum_nights)%>%
  arrange(desc(n))%>%
  mutate(proportion=n/sum(n))

top10_min_n<-min_n%>%
  head(10)

#The most common values for minimum_night
ggplot(top10_min_n,aes(x=reorder(minimum_nights,-n),y=n))+
  geom_col(fill="pink")+
  labs(title="Most common minimum-nights requirements",
       subtitle="Top 10 minimum-nights required to book a property on Airbnb in Brussels",
       x="Value of minimum nights",
       y="Numbers")+
  theme_bw()+
  NULL

#overall density
ggplot(min_n,aes(x=minimum_nights))+
  geom_density(fill="pink",color="red")+
  labs(title="Minimum nights distribution",
       subtitle="minimum nights required to book a property on Airbnb in Brussels",
       x="Minimum nights",
       y="Density")+
  theme_bw()+
  NULL

```

The most common values for the variable minimum_nights are 1 and 2 days, the majority of them are within 7 days. Among the top 10 most common values, we could see that the minimum required nights of 30 days and 90 days are somehow more common than that of 6 days, which is much longer than what we expected for travel purposes. This probably suggests that Airbnb plays the same role as the housing agents that provide short-rent houses in Brussels. The overall distribution is right skewed as well, which shows the same conclusion.

Since we are going to estimate the cost of 2 people staying for 4 nights, we filter the Airbnb data so that it only includes observations with a minimum nights less than 4 and get rid of the outliers. Then we could get the data that we are going to use in the regression model.

```{r filter_minimum_night}
# filter the airbnb data 
listings <- listings%>%
  filter(minimum_nights<=4)

```


## Statisical summary of variables that we are interested in

```{r}

skim(listings)
  
```

And we are going to explore the data from the perspectives of host, location, property, reviews and cancellation policy separately.

### Host 

First of all, we would like to explore how long have the hosts been on Airbnb.

```{r years_of_host_experience}

#we could get the latest date of all the scrape in checking the listings_csv file and found it to be "2020-06-19"

#caculate the year of host experience
year_host_experience<- listings%>%
  mutate(year_host=as.numeric(ymd("2020-06-19")-host_since)/365)

#statistical summary of host experience
year_host<-favstats(~year_host, data=year_host_experience)

year_host%>%
  kbl() %>%
  kable_styling()

#plot the distribution of host experience
ggplot(year_host_experience,aes(x=year_host))+
  geom_density(fill="pink",color="red")+
  labs(title="The majority of the hosts had more than 3 years of experience on Airbnb",
       subtitle="in Brussels",
       x="Years of host experience",
       y="Density")+
  theme_bw()+
  NULL


```

Overall, the hosts had an average of 4.6 years of experience in hosting on Airbnb. It is slightly left-skewed with a median of 4.81. The majority of hosts had an experience over 3 years.

Then we look at the response time of the hosts. Although this variable is not applicable to 49.9% of the Airbnb hosts, 28.57% of them will response within one hour. And only less than 4% of the hosts have a response time longer than one day, which suggests that the quality of Airbnb hosts in Brussels are high.

```{r host_acc_response}

#distribution of host response time
listings<-listings%>%
   mutate(host_response_time=factor(host_response_time,order=TRUE,levels=c("within an hour", "within a few hours", "within a day","a few days or more")))

#summary of the response time
res_time <-listings%>%
  group_by(host_response_time)%>%
  summarise(num=n())%>%
  mutate(proportion=num/sum(num))

res_time%>%
  kbl() %>%
  kable_styling()

#plot the distribution of host response time
ggplot(res_time,aes(x=host_response_time,y=num))+
  geom_col(fill="pink")+
  labs(title="Distribution of host response time",
       x="Host response time",
       y="Numbers")+
  theme_bw()+
  NULL

```

After that, we checked the relationship between host years and host response time to see if those who response quick would have longer host experience on Airbnb. Yet the result didn't reflect this relationship.

```{r corr_host_year_response_time}

#reorder the host response time in year host experience
year_host_experience<-year_host_experience%>%
    mutate(host_response_time=factor(host_response_time,order=TRUE,levels=c("within an hour", "within a few hours", "within a day","a few days or more")))

#check the relationship between response time and host years
ggplot(year_host_experience,aes(x=host_response_time, y=year_host)) +
  geom_boxplot(fill="pink",color="red") +
  labs(title="Quick response doesn't mean longer years of host experience",
       x="Host response time",
       y="Years of host")+
  theme_bw()+
  NULL

```

Then we take a look at another factor that could reflected the quality of hosts--whether the host is a super host--and check whether there's any correlation between super hosts and others. We found that 20.8% of the hosts are super hosts on Airbnb in Brussels. Still we cannot say that super hosts would response faster than other hosts.

```{r super_hosts_and_response_time}
#summary of super hosts
super_hosts<- listings%>%
  group_by(host_is_superhost,host_response_time)%>%
  summarise(num=n())%>%
  mutate(proportion_within_the_group=num/sum(num))

super_hosts%>%
  kbl() %>%
  kable_styling()

#plot the distribution of host response time
ggplot(super_hosts,aes(x=reorder(host_is_superhost,num),y=num))+
  geom_col(fill="pink")+
  labs(title="There are 1182 super hosts on Airbnb in Brussels",
       x="Host is a super host",
       y="Numbers")+
  theme_bw()+
  NULL
  

#Plot to see the relationship between super hosts and other hosts
ggplot(listings,aes(x=host_response_time, y=count(host_response_time))) +
  geom_col(fill="pink") +
  facet_wrap(~host_is_superhost,ncol=1)+
  labs(title="Super hosts didn't seem to respond more quickly than other hosts",
       x="Host response time",
       y="Numbers")+
  theme_bw()+
  NULL

```


### Location

Next we take a look at the locations of property. According to the data, there are 19 neighborhoods covered by the listings on Airbnb. And 1891 listings are in Bruxelles, which is 33% of the total listings in Brussels. The top 3 neighborhoods are Bruxelles, Ixelles and Saint-Gilles.

```{r summary_of_locations}
#summary of the neighborhood covered by Airbnb listings in Brussels
su_locations<-listings%>%
  group_by(neighbourhood_cleansed)%>%
  summarise(num=n())%>%
  mutate(proportion=num/sum(num))%>%
  arrange(desc(num))

su_locations%>%
  kbl() %>%
  kable_styling()

#plot the number of listings in different neighborhoods
ggplot(su_locations,aes(x=num,y=reorder(neighbourhood_cleansed,num)))+
  geom_col(fill="pink")+
  labs(title="Airbnb listings in different neighbourhoods in Brussels",
       x="Number of listings",
       y=NULL)+
  theme_bw()+
  NULL

```

And then we take a look at the accuracy of the locations shown on Airbnb. Almost 85% of the listings show the exact same location as it is in Brussels, which suggests that the level of accuracy is quite high.

```{r location_accu}
#summary of the location accuracy
location_accu<-listings%>%
  group_by(is_location_exact)%>%
  summarise(num=n())%>%
  mutate(proportion=num/sum(num))%>%
  arrange(desc(num))

location_accu%>%
  kbl() %>%
  kable_styling()

#plot the accuracy of locations
ggplot(location_accu,aes(x=reorder(is_location_exact,-num),y=num))+
  geom_col(fill="pink")+
  labs(title="The accuracy of the location shown on Airbnb listings in Brussels",
       x="Is the location exact the same as shown?",
       y="Number of listings")+
  theme_bw()+
  NULL

```


### Property

In this part, we look at the property type, room type and other features.

```{r property_types1}
# summary of simplified property types after filters
category_5_p2<-listings%>%
  count(prop_type_simplified)%>%
  mutate(prop_type_simplified=factor(prop_type_simplified,order=TRUE,levels = c("Apartment","House", "Condominium","Townhouse","Other")),
         proportion=n/sum(n))

#plot the number of listings in different property types
ggplot(category_5_p2,aes(x=prop_type_simplified,y=n))+
  geom_col(fill="Pink")+
  labs(title="The majority of Airbnb listings in Brussels are apartments",
       x="Simplified proper types",
       y="Number of listings")+
  theme_bw()+
  NULL

```

Then we take a look at the room type. 69% of the listings are entire home/apt and 29% of the listings are private rooms. There are only a few hotel rooms and shared rooms, which shows a high level of overall privacy.
Besides 

```{r summ_room_types}
#summary of room types
r_type1<-listings%>%
  group_by(room_type)%>%
  summarise(num=n())%>%
  mutate(proportion=num/sum(num))%>%
  arrange(desc(num))

r_type1%>%
  kbl() %>%
  kable_styling()

#plot the distribution of rooms types
ggplot(r_type1,aes(x=num, y=reorder(room_type,num))) +
  geom_col(fill="pink") +
  labs(title="Airbnb listings in Brussels has a high level of privacy",
       subtitle = "distribution of different room types",
       x="Number of listings",
       y=NULL)+
  theme_bw()+
  NULL

```

Also we could see the distribution of bedrooms and bathrooms among the Airbnb listings in Brussels. The most common value for both bedrooms and bathrooms is 1.

```{r summ_bed_bath_rooms}
#bedrooms
#statistical summary of bedrooms
bed_r<-favstats(~bedrooms, data=listings)

bed_r%>%
  kbl() %>%
  kable_styling()

#plot the distribution of bedrooms
ggplot(listings,aes(x=bedrooms))+
  geom_density(fill="pink",color="red")+
  labs(title="The majority of the listings have one bedroom",
       x="Number of bedrooms",
       y="Density")+
  scale_x_continuous(breaks = seq(0,10,1), limits = c(0,10))+
  theme_bw()+
  NULL

#bathrooms
#statistical summary of bathrooms
bath_r<-favstats(~bathrooms, data=listings)

bath_r%>%
  kbl() %>%
  kable_styling()

#plot the distribution of bathrooms
ggplot(listings,aes(x=bathrooms))+
  geom_density(fill="pink",color="red")+
  labs(title="The majority of the listings have one bathroom",
       x="Number of bathrooms",
       y="Density")+
  scale_x_continuous(breaks = seq(0,10,1), limits = c(0,10))+
  theme_bw()+
  NULL

```

The average accommodates is 3.25 persons, and over 50% of listings accommodates for exact 2 people. Besides, those could accommodate even number of people are more than those accommodates odd number of people. 

```{r summary_accommodates}
#accommodates
#statistical summary of accommodates
acco_n<-favstats(~accommodates, data=listings)

acco_n%>%
  kbl() %>%
  kable_styling()

#plot the distribution of accommodates
ggplot(listings,aes(x=accommodates))+
  geom_density(fill="pink",color="red")+
  labs(title="Over 50% of listings accommodates 2 people",
       x="Accommodates",
       y="Density")+
  scale_x_continuous(breaks = seq(0,16,2), limits = c(0,16))+
  theme_bw()+
  NULL
```

Overall, the average price is 76.9 dollars and the median price is 60 dollars. But the maximum price reached 3500 dollars, which led to a positive skewed price distribution. 

```{r summary_price}
#price
#statistical summary of price
summary_price<-favstats(~price, data=listings)

summary_price%>%
  kbl() %>%
  kable_styling()

#plot the distribution of price
ggplot(listings,aes(x=price))+
  geom_density(fill="pink",color="red")+
  labs(title="Distribution of price is skewed right",
       x="Price",
       y="Density")+
  theme_bw()+
    scale_x_continuous(breaks = seq(0,1000,50), limits = c(0,1000))+
  NULL

```


### Reviews

The average number of reviews is 42, while the median is only 14, which suggests a right-skewed distribution. Meanwhile, the maximum number of reviews is 766, which is far beyond the average.

```{r stats_number_of_reviews}
#statistical summary of number_of_reviews
su_review_numbers<-favstats(~number_of_reviews, data=listings)

su_review_numbers%>%
  kbl() %>%
  kable_styling()

#plot the distribution of number_of_reviews
ggplot(listings,aes(x=number_of_reviews))+
  geom_density(fill="pink",color="red")+
  labs(title="Distribution of number of reviews is skewed right",
       x="numbers of reviews",
       y="Density")+
  theme_bw()+
    scale_x_continuous(breaks = seq(0,800,100), limits = c(0,800))+
  NULL
```


The average rating score is 92.7 and the median rating score is 95, which suggests high satisfaction from customers. The distribution of rating scores is skewed left.

```{r stats_review_scores_rating}

#statistical summary of reviews_scores_rating
su_review_rating<-favstats(~review_scores_rating, data=listings)

su_review_rating%>%
  kbl() %>%
  kable_styling()

#plot the distribution of reviews_scores_rating
ggplot(listings,aes(x=review_scores_rating))+
  geom_density(fill="pink",color="red")+
  labs(title="Distribution of rating scores is skewed left",
       x="Rating scores",
       y="Density")+
  theme_bw()+
    scale_x_continuous(breaks = seq(0,100,25), limits = c(0,100))+
  NULL

```

To break down the rating scores, we also take a look at the scores on different aspects.

```{r rating_from_different_perspectives}

#plot the distribution of different rating scores
# plot the distribution of scores on accuracy
p_accuracy <- listings%>%
  ggplot(aes(x=review_scores_accuracy)) +
  geom_density(fill="pink",color="red") +
  labs(x = "review score") +
  ggtitle("Accuracy") +
  theme_bw()+
  NULL


# plot the distribution of scores on checkin process
p_checkin <- listings%>%
  ggplot(aes(x=review_scores_checkin)) +
  geom_density(fill="pink",color="red") +
  labs(x = "review score") +
  ggtitle(" Checkin") +
  theme_bw()+
  NULL


# plot the distribution of scores on cleanliness
p_cleanliness <- listings%>%
  ggplot(aes(x=review_scores_cleanliness)) +
  geom_density(fill="pink",color="red") +
  labs(x = "review score") +
  ggtitle("Cleanliness") +
  theme_bw()+
  NULL

# plot the distribution of scores on communication
p_communication <- listings%>%
  ggplot(aes(x=review_scores_communication)) +
  geom_density(fill="pink",color="red") +
  labs(x = "review score") +
  ggtitle("Communication") +
  theme_bw()+
  NULL

# plot the distribution of scores on location
p_location <- listings%>%
  ggplot(aes(x=review_scores_location)) +
  geom_density(fill="pink",color="red") +
  labs(x = "review score") +
  ggtitle("Location") +
  theme_bw()+
  NULL

# plot the distribution of scores on value
p_value <- listings%>%
  ggplot(aes(x=review_scores_value)) +
  geom_density(fill="pink",color="red") +
  labs(x = "review score") +
  ggtitle("Value") +
  theme_bw()+
  NULL

#put the plots together
library(patchwork)
combine1<- p_accuracy+p_cleanliness+p_checkin+p_communication+p_location+p_value
combine1
```

### Cancellation_policy

Basically, the cancellation policy in Airbnb listings in Brussels is proportioned evenly. About 37% of the listings have a flexible cancellation policy, and around 31% of the listings have a moderate cancellation policy and the rest have a strict policy. 

```{r sum_cancellation_policy}
#summary of the cancellation policies
su_cancel<-listings%>%
  group_by(cancellation_policy)%>%
  summarise(num=n())%>%
  mutate(proportion=num/sum(num),
         cancellation_policy=factor(cancellation_policy,ordered = TRUE,levels = c("flexible","moderate","strict_14_with_grace_period","super_strict_30")))

su_cancel%>%
  kbl() %>%
  kable_styling()

#plot the number of listings in different cancellation policies
ggplot(su_cancel,aes(x=cancellation_policy,y=num))+
  geom_col(fill="pink")+
  labs(title="Airbnb listings with different cancellation policies in Brussels",
       x="Cancellation policy",
       y="Number of listings")+
  theme_bw()+
  NULL

```

### Compute the dependent variable: Price_4_nights

we create the price_4_nights variable using the case_when() function. 
Our conditional statement says that if the number of guests included in the price is greater than or equal to 2, the price is just the cleaning fee plus 4* the nightly rate, since the 2 people will stay for 4 nights. 
However, if the number of guests included in the price is less than 2, meaning that a party of 2 would have to pay for exactly 1 extra guest, then the price will be 4* the nightly price, which is the base price plus the price for that extra guest, plus the cleaning fee.

The average cost of 2 people staying for 4 nights is 339 dollars and the overall distribution is right-skewed. 

```{r compute_price_4_nights}
#compute the price_4_nights variable 
year_host_experience <- year_host_experience %>% 
  mutate(price_4_nights = case_when(
    guests_included >= 2 ~ cleaning_fee + 4*price,
    TRUE ~ 4 * (price + extra_people) +cleaning_fee))

#see the summary of price_4_nights
kbl(skim(year_host_experience$price_4_nights))%>%
  kable_styling()

ggplot(year_host_experience,aes(x=price_4_nights))+
  geom_density(fill="pink",color="red")+
  labs(title="Distribution of price for 2 persons staying 4 nights",
       x="Price for 4 nights",
       y="Density")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0,1000,500), limits = c(0,1000))+
  NULL

```


## Correlation between variables 

To take a look at how the numerical variables are correlated, we use the "ggpair()" function and get the following graphs.

```{r correlation_review_scores}
correlation1<- year_host_experience%>%
  #reviews
  select(review_scores_rating, 
         review_scores_accuracy, 
         review_scores_cleanliness, 
         review_scores_checkin, 
         review_scores_communication, 
         review_scores_location, 
         review_scores_value
         ) %>% 
  ggpairs(alpha=0.5) +
  theme_bw()

correlation1

```

The review scores are highly correlated to each other with correlation coefficient over 0.4, so we should avoid using them all, and maybe just use one variable as the representative of the reviews.

```{r correlation_prop_features}
correlation2<- year_host_experience%>%
  #property features and price
  select(accommodates,
         bathrooms,
         bedrooms,
         security_deposit,
         cleaning_fee,
         guests_included,
         extra_people,
         minimum_nights,
         maximum_nights,
         price_4_nights) %>% 
  ggpairs(alpha=0.5) +
  theme_bw()

correlation2

```

Based on the above correlation matrix, the number of bedrooms,bathrooms and accommodates are highly correlated with correlation factors over 0.4. Therefore, we would avoid using all three of them in our regression model. 
Also, we found that the cleaning fee and security deposit are highly correlated to each other, and the correlation is almost linear according to the scatter plot above. Other variables didn't show obvious linear correlations, therefore we'd like to group some of the variables and see if there's any correlations between the factors. We started from the potential independent variables.

First, we would like to start from the reviews and explore the relationship between number of reviews and score ratings. We found that the median rating score goes down as the quartile group goes up. However, the average score in bucket 4 is higher than that in bucket 1. After testing the difference of average rating score in bucket 1 and bucket 4, we found that the difference in mean is statistically significant at 95% level. 
Besides, because the number is highly right-skewed, the sample size is much bigger in quartile group 4 and the result has smaller standard deviation as well. But there's no linear correlation between these 2 variables.

```{r review_num_scores_corr}
#Relationship between number of reviews and the score ratings

##select the data
reviewData <- year_host_experience %>% 
  select(review_scores_rating,number_of_reviews)
glimpse(reviewData)

kbl(skim(reviewData))%>%
  kable_styling()

##We group the number of reviews by its quartile
reviewData <- reviewData %>%
  mutate(buckets = ntile(number_of_reviews,4))

#check the quartile of number of reviews
reviewData2 <- reviewData %>%
  group_by(buckets)%>%
  summarize(minimum=min(number_of_reviews),maximum=max(number_of_reviews))
reviewData2%>%
  kbl()%>%
  kable_styling()

#change buckets into factors
reviewData$buckets<-as.factor(reviewData$buckets)

#plot rating scores in different groups 
ggplot(reviewData,aes(x=review_scores_rating,y=buckets)) + 
  geom_boxplot(fill="pink",color="red")+
  labs(title="Rating scores are more stable in those with higher number of reviews",
       x="Rating scores",
       y="Quartiles group of review numbers")+
  theme_bw()+
  NULL

# test the difference in bucket 1 and bucket 4 
redata<-reviewData%>%
  filter(buckets!="2",
         buckets!="3")
t.test(review_scores_rating ~ buckets, data=redata)

```

Then we would like to explore the relationship between hosts' experience and the review scores. Also we tried to form 4 quartile groups based on the host's experience. The average score of bucket 4 is slightly higher than that of bucket 1. However, there isn't any linear correlation between these factors as well.

```{r host_reviews}
#Relationship between score ratings and year of hosts' experience

##select the data
rating_host_y <- year_host_experience %>% 
  select(review_scores_rating,year_host)
glimpse(rating_host_y)

kbl(skim(rating_host_y))%>%
  kable_styling()

##We group the year of hosts by its quartile
rating_host_y <- rating_host_y %>%
  mutate(buckets = ntile(year_host,4))

#check the quartile of years of hosts
rating_host_y2 <- rating_host_y %>%
  group_by(buckets)%>%
  summarize(minimum=min(year_host),maximum=max(year_host))
rating_host_y2%>%
  kbl()%>%
  kable_styling()

#change buckets into factors
rating_host_y$buckets<-as.factor(rating_host_y$buckets)

#plot rating scores in different groups
ggplot(rating_host_y,aes(x=review_scores_rating,y=buckets)) + 
  geom_boxplot(fill="pink",color="red")+
  labs(title="Rating scores didn't vary a lot between quartile group of host years",
       x="Rating scores",
       y="Quartiles group of host years")+
  theme_bw()+
  NULL

# test the difference in bucket 1 and bucket 4 
rating_host<-rating_host_y%>%
  filter(buckets!="2",
         buckets!="3")
t.test(review_scores_rating ~ buckets, data=rating_host)

```

Then we check the relationship between neighborhood and the reviews. No obvious linear correlations are found, yet the standard deviation of the number of reviews and the rating score is slightly different between different neighborhood.

```{r neighbor_rating_scores_numbers}
#neighborhood vs. number of reviews
ggplot(listings,aes(x=number_of_reviews,y=reorder(neighbourhood_cleansed,number_of_reviews,median)))+
  geom_boxplot(fill="pink",color="red")+
  labs(title="Number of reviews on listings in different neighbourhoods in Brussels",
       x="Number of reviews",
       y=NULL)+
  theme_bw()+
  NULL

#neighborhood vs. rating scores
ggplot(listings,aes(x=review_scores_rating, y=reorder(neighbourhood_cleansed,review_scores_rating),median))+
  geom_boxplot(fill="pink",color="red")+
  labs(title="Rating scores on listings in different neighbourhoods in Brussels",
       x="Rating scores",
       y=NULL)+
  theme_bw()+
  NULL

```

Then we check the relationship between simplified property types and the reviews. 

```{r}
#simplified property type vs. number of reviews
ggplot(listings,aes(x=number_of_reviews, y=reorder(prop_type_simplified,number_of_reviews)))+
  geom_boxplot(fill="pink",color="red")+
  labs(title="Number of reviews didn't vary much across different property types",
       x="Number of reviews",
       y=NULL)+
  theme_bw()+
  NULL

#simplified property type vs. rating scores
ggplot(listings,aes(x=review_scores_rating, y=reorder(prop_type_simplified,review_scores_rating)))+
  geom_boxplot(fill="pink",color="red")+
  labs(title="Rating scores didn't vary much across different property types",
       x="Rating scores",
       y=NULL)+
  theme_bw()+
  NULL
```

What's more, we examine the relationship between simplified property types and the neighborhood. Apartments are the most common property type in almost every neighborhoods in Brussels.

```{r property_type_location}
prop_type1<-listings%>%
  group_by(neighbourhood_cleansed,prop_type_simplified)%>%
  summarise(num=n())%>%
  mutate(proportion=num/sum(num))%>%
  arrange(desc(num))

prop_type1%>%
  kbl() %>%
  kable_styling()

#plot the distribution of rooms types
ggplot(prop_type1,aes(x=num,y=reorder(neighbourhood_cleansed,num),fill=prop_type_simplified)) +
  geom_bar(position="dodge",stat="identity") +
  labs(title="Distribution of different property types",
       subtitle = "across different neighbourhoods in Brussels",
       x="Number of listings",
       y=NULL)+
  theme_bw()+
  theme(legend.title = element_blank())+
  NULL
```

### Correlation between price_4_nights and other variables in particular

Since we have to study how the other variable could impact the total cost of staying, we decided to explore the correlation between price_4_nights and the other potential variables.

First we start with the cost and the property types. Apart from the "Other" category, the average costs to stay in a house is 365 dollars, while that for an apartment is 322 dollars. However, we found that the average price of apartment is not statistically significant than that of houses after testing the difference.

```{r price_prop_type2}
#compute summary of price of different property types
sum_price_property<-year_host_experience%>%
  group_by(prop_type_simplified)%>%
  summarise(avg_price4=mean(price_4_nights),
            median_price4=median(price_4_nights),
            sd_p4=sd(price_4_nights),
            max_p4=max(price_4_nights),
            min_p4=max(price_4_nights))

#show the summary
sum_price_property%>%
  kbl()%>%
  kable_styling()

#plot the box plot based on simplified property type
ggplot(year_host_experience,aes(x=prop_type_simplified,y=price_4_nights))+
  geom_boxplot(fill="pink",color="red")+
  labs(title="Price for 4 nights in different property types",
       x=NULL,
       y="Price for 4 nights")+
  scale_y_continuous(breaks = c(0,1000,500),limits = c(0,1000))+
  theme_bw()+
  NULL
  
# test the difference in apartment and house
test_prop_price<-year_host_experience%>%
  filter(prop_type_simplified %in% c("Apartment","House"))

t.test(price_4_nights ~ prop_type_simplified, data=test_prop_price)

```

We also examined the relationship between price and room types. After testing the difference between average price in each group, we found that hotel rooms are more expensive than the entire home or apartment, and the difference is statistically significant at 95% level. Meanwhile, the average price of private room is higher than that of shared room, and the difference is statistically significant at 90% level.

```{r price_4_nights_room_type}
#compute summary of price of different room types
sum_price_room<-year_host_experience%>%
  group_by(room_type)%>%
  summarise(avg_price4=mean(price_4_nights),
            median_price4=median(price_4_nights),
            sd_p4=sd(price_4_nights),
            max_p4=max(price_4_nights),
            min_p4=max(price_4_nights))

#show the summary
sum_price_room%>%
  kbl()%>%
  kable_styling()

#plot the box plot based on simplified property type
ggplot(year_host_experience,aes(x=room_type,y=price_4_nights))+
  geom_boxplot(fill="pink",color="red")+
  labs(title="Price for 4 nights varies in different room types",
       x=NULL,
       y="Price for 4 nights")+
  scale_y_continuous(breaks = c(0,1000,500),limits = c(0,1000))+
  theme_bw()+
  NULL
  
# test the difference in hotel room and entire home/apt
test_room_price1<-year_host_experience%>%
  filter(room_type %in% c("Entire home/apt","Hotel room"))
t.test(price_4_nights ~ room_type, data=test_room_price1)

# test the difference in private room and shared home
test_room_price1<-year_host_experience%>%
  filter(room_type %in% c("Private room","Shared room"))
t.test(price_4_nights ~ room_type, data=test_room_price1)

```


Then we started with the cost and the reviews by creating scatter plots. We found that the price didn't goes up as the number of reviews goes up. Maybe people reviews more on the property with not that high price on Airbnb.

On the other hand, we noticed that the maximum price goes up when the ratings goes up, which somehow suggests that people might tend to pay higher price for those with higher rating. 

```{r price_4_nights_reviews}
#examine the relationship between price and number of reviews
ggplot(year_host_experience,aes(x=number_of_reviews,y=price_4_nights,color="red"))+
  geom_point()+
  labs(title="Price for 4 nights change as reviews numbers goes up",
       x=NULL,
       y="Price for 4 nights")+
  scale_y_continuous(breaks = c(0,5000,500),limits = c(0,5000))+
  theme_bw()+
  NULL

#examine the relationship between price and rating scores
ggplot(year_host_experience,aes(x=review_scores_rating,y=price_4_nights,color="red"))+
  geom_point(show.legend = FALSE)+
  labs(title="Maximum price for 4 nights goes up as rating scores goes up",
       x=NULL,
       y="Price for 4 nights")+
  scale_y_continuous(breaks = c(0,5000,500),limits = c(0,5000))+
  theme_bw()+
  NULL

```

However, the average price didn't change as the rating scores goes up. We also formed 4 groups based on the quartile of ratings scores and and found the difference between the average costs is not statistically significant.

```{r}

##We group the rating scores by its quartile
rating_price <- year_host_experience %>%
  mutate(buckets = ntile(review_scores_rating,4))

#check the quartile of rating scores
rating_price2 <- rating_price %>%
  group_by(buckets)%>%
  summarize(minimum=min(review_scores_rating),maximum=max(review_scores_rating))
rating_price2%>%
  kbl()%>%
  kable_styling()

#change buckets into factors
rating_price$buckets<-as.factor(rating_price$buckets)

#plot price for 4 nights in different groups
ggplot(rating_price,aes(x=price_4_nights,y=buckets)) + 
  geom_boxplot(fill="pink",color="red")+
  labs(title="Average price didn't vary a lot between different groups of rating scores",
       x="Price for 4 nights",
       y="Quartiles group of rating scores")+
  scale_x_continuous(breaks = c(0,1000,500),limits=c(0,1000))+
  theme_bw()+
  NULL

# test the difference in bucket 1 and bucket 4 
rating_price<-rating_price%>%
  filter(buckets!="2",
         buckets!="3")
t.test(price_4_nights ~ buckets, data=rating_price)
```

Next, we would like to see whether the hosting experience is correlated to the total price. According to the following graphs, we found that the price didn't vary a lot between experienced hosts and new hosts. Besides, listings with a super host may have a higher average price than others, and the difference is statistically significant at 90% level.

```{r price4_nights_host_years_superhost}
#examine the relationship between price and years of host
ggplot(year_host_experience,aes(x=year_host,y=price_4_nights,color="red"))+
  geom_point()+
  labs(title="Price for 4 nights didn't vary a lot between experienced hosts and new hosts",
       x="years of hosting experience",
       y="Price for 4 nights")+
  scale_y_continuous(breaks = c(0,1000,500),limits = c(0,1000))+
  theme_bw()+
  NULL

#examine the relationship between price and super hosts
ggplot(year_host_experience,aes(x=host_is_superhost,y=price_4_nights))+
  geom_boxplot(fill="pink",color="red")+
  labs(title="Price for 4 nights goes up as rating scores goes up",
       x=NULL,
       y="Price for 4 nights")+
  scale_y_continuous(breaks = c(0,1000,500),limits = c(0,1000))+
  theme_bw()+
  NULL

#test the significance of the difference
t.test(price_4_nights ~ host_is_superhost, data=year_host_experience)

```

After that, we examine the relationship between price and cancellation policy. 
From the graphs below, we could see that listings with stricter cancellation policies cost slightly more in average than those with more flexible policies. This could be related to how popular the listings are. Strict cancellation policy might suggest that the listing is more popular in the market, thus the price would be a little bit higher.

```{r price_4_nights_cancellation policy}
#examine the relationship between price and cancellation policies
ggplot(year_host_experience,aes(x=cancellation_policy,y=price_4_nights))+
  geom_boxplot(fill="pink",color="red")+
  labs(title="Price for 4 nights goes up as policy becomes stricter",
       x="Cancellation policy",
       y="Price for 4 nights")+
  scale_y_continuous(breaks = c(0,1000,500),limits = c(0,1000))+
  theme_bw()+
  NULL

```

Finally, we examine the correlation between price and location. The median of total costs slightly varied among different neiborhood. Bruxelles has the highest median costs and the difference between its median cost and that of Evere is statistically significant at 95% level.

```{r price_4_nights_locations}
#price_4_nights in different neighborhood
ggplot(year_host_experience,aes(x=price_4_nights,y=reorder(neighbourhood_cleansed,price_4_nights,median)))+
  geom_boxplot(fill="pink",color="red")+
  labs(title="Distribution of price for 4 nights in different neighbourhoods in Brussels",
       x="Price for 4 nights",
       y=NULL)+
  scale_x_continuous(breaks = c(0,2000,500),limits = c(0,2000))+
  theme_bw()+
  NULL

# test the difference of average price in Bruxelles and Evere
test_neighbor_price<-year_host_experience%>%
  filter(neighbourhood_cleansed %in% c("Bruxelles","Evere"))
t.test(price_4_nights ~ neighbourhood_cleansed, data=test_neighbor_price)
```









# Mappings

``` {r}
leaflet(data = filter(year_host_experience, minimum_nights <= 4)) %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircleMarkers(lng = ~longitude, 
                   lat = ~latitude, 
                   radius = 1, 
                   fillColor = "blue", 
                   fillOpacity = 0.4, 
                   popup = ~listing_url,
                   label = ~property_type)


```









# Regression Analysis

```{r}
#Data used for regression
listings2<- year_host_experience %>%
  select(price,
         cleaning_fee,
         extra_people,
         property_type,
         room_type,
         number_of_reviews,
         review_scores_rating,
         longitude,
         latitude,
         neighbourhood,
         guests_included,
         host_is_superhost,
         beds,
         cancellation_policy, 
         is_location_exact,
         bedrooms,
         bathrooms,
         accommodates,
         neighbourhood_cleansed, 
         prop_type_simplified,
         price_4_nights)

listings3 <- listings2 

#Grouping neighbourhoods
listings3 <- listings3 %>%    
  mutate(neighbourhood_simplified=case_when(neighbourhood_cleansed=="Bruxelles"|neighbourhood_cleansed=="Etterbeek"|neighbourhood_cleansed=="Ixelles"|neighbourhood_cleansed=="Saint-Gilles"|neighbourhood_cleansed=="Saint-Josse-ten-Noode"|neighbourhood_cleansed=="Schaerbeek"~"Central",neighbourhood_cleansed=="Berchem-Sainte-Agathe"|neighbourhood_cleansed=="Evere"|neighbourhood_cleansed=="Ganshoren"|neighbourhood_cleansed=="Koekelberg"|neighbourhood_cleansed=="Jette"~"North",neighbourhood_cleansed=="Uccle"|neighbourhood_cleansed=="Watermael-Boitsfort"~"South",neighbourhood_cleansed=="Anderlecht"|neighbourhood_cleansed=="Forest"|neighbourhood_cleansed=="Molenbeek-Saint-Jean"~"West",neighbourhood_cleansed=="Auderghem"|neighbourhood_cleansed=="Woluwe-Saint-Lambert"|neighbourhood_cleansed=="Woluwe-Saint-Pierre"~"East", TRUE~neighbourhood_cleansed))

#Adding log variable
listings3 <- listings3 %>%
  mutate(log_price_4_nights=log(price_4_nights))

```



```{r}
plot2 <- listings3 %>%
  ggplot(aes(x=price_4_nights))+
   geom_histogram(color="blue",fill="tomato2")
plot2

plot3 <- listings3 %>%
  ggplot(aes(x=log_price_4_nights))+
   geom_histogram(color="blue",fill="tomato2")
plot3

```

The first reason why we will use a log dependent variable (log_price_4_nights) is the interpretation. It is more meaningful to see how each explanatory variable affects the dependent variable in terms of percentages than in the absolute terms. Secondly, as can be seen from the plots above, when we transform the price_4_nights in the log form, it more closely follows a normal distribution while the normal form is skewed to the right. So, it might help us reduce heteroscedasticity of residuals and improve the precision of our estimates.

The goal is to determine how a variety of different variables affects price for 4 nights in an Airbnb accommodation in Bruxelles. We start by making a model where the dependent variable is log_price_4_nights, and the explanatory variables are prop_type_simplified, number_of_reviews, and review_scores_rating. When there is a dependent variable in a log form, coefficients are not interpreted in the standard way. Specifically, if we have a coefficient B1, then that means that a one unit increase in the variable X1 increases the dependent variable by (e^B1-1). But for small enough values of B1, e^B1 - 1 is roughly equal to B1. This is a good approximation when -0.1 < B < 0.1. For values outside of this range, we will use e^B1 - 1 to calculate the effect.

In each regression, we will use vif() to check whether there exists multicollinearity among the explanatory variables. Similarly, we will use autoplot() and see how the model satisfies 4 main OLS assumptions.


```{r MODEL 1}
#Regression model 1
  model1 <-  lm(log_price_4_nights ~ prop_type_simplified+number_of_reviews+review_scores_rating, data=listings3)
  table1 <- get_regression_table(model1)
  table1
  vif(model1)
  autoplot(model1)
  
```

With the prop type simplified, we have 4 dummy variables where the omitted group is "apartment", so coefficients need to be interpreted in relation with the apartment group. For instance, on the prop_type_simplifiedHouse the coefficient is -0.075. This means that on average holding other things constant, houses are roughly 7.5% cheaper than the apartments. Similarly, townhouses are roughly 11% cheaper than apartments, and the accommodations belonging to the "other group" are around 33% more expensive. Condominiums are correlated with 8.5% higher prices than in the apartments. In those 4 cases, the coefficients are significant as p-values are very low. Furthermore, the coefficient on review_scores_rating is 0.003, meaning that a unit increase in this variable, holding everything else constant, increases prices by roughly 0.3%. This statistic is significant. Number_of_reviews seems to have no effect on prices as the coefficient is ~0. 

There seems to be no collinearity among explanatory variables as VIF values are all significantly lower than 5. In this model, R-sqaured is very low and there is a heteroscedasticity of residuals, which is a violation of one of the main assumptions of linear regressions. Although heteroscedasticity does not lead to bias in the estimates, it might make them less precise and lead to incorrect standard errors. Hence, in the subsequent models we will add new variables. In addition, even though the above-mentioned coefficients seem to have effect on the dependent variable, this does not necessarily imply causation. When variables which are correlated with both some of the explanatory variables and with the dependent variable are not included in the model, coefficients on some variables cause 'omitted variables bias'. For instance, it might not necessarily be the case that the type of the accommodation being a house automatically leads to lower prices, rather it might be that the houses are generally located in cheaper regions than apartments, or that the people renting houses take only one room whereas people renting apartments usually take the whole place. That is also why in the next models, we control for variables which might cause this bias (confounding variables).



```{r}
  
#Regression model2, with room_type
  
model2 <- lm(log_price_4_nights ~ prop_type_simplified+number_of_reviews+review_scores_rating+room_type, data=listings3)
  table2 <- get_regression_table(model2)
  table2
  vif(model2)
  autoplot(model2)
 
```

In this regression, we add variable room_type, which can take 4 values: entire house, shared room, private room, and hotel room.  WE created 3 dummies for hotel, private and shared rooms. Hence, the omitted group is entire house and the coefficients on these dummies must be interpreted with respect to this omitted group. Firstly, the coefficient on the "hotel room" dummy, is 0.163, meaning that, ceteris paribus, hotel rooms are on average 17.7% more expensive than entire houses for 4 nights. On the private room dummy, we get a coefficient of -0.589, when transformed, tells us that private rooms are roughly 45% cheaper than entire houses, holding everything else constant. The largest gap is with shared rooms, which seem to be around 50% cheaper than the entire houses. All those statistics are significant even at 1% level.  It is interesting to note that for instance, the coefficient on prop_type_simplifiedHouse completely changed, and is now positive. It means that it was negatively biased in the first regression, and it was capturing some of the effect effect of the room_type. 



``` {r}
#Adding new explanatory variables - bathrooms, bedrooms, and beds
  
  model3 <- lm(log_price_4_nights ~ prop_type_simplified+number_of_reviews+review_scores_rating+room_type+bathrooms+bedrooms+beds, data=listings3)
  table3 <- get_regression_table(model3)
  table3
  vif(model3)
  autoplot(model3)
  
```


In this model, WE added variables 'bathrooms', 'bedrooms', and 'beds'. The coefficients on all three new variables are very significant (p-values are very low) and they have a significant effect on prices, as expected. For instance, the coefficient on bedrooms is 0.144, which means that a unit increase in bedroom leads to roughly a 15.5% increase in prices. Similarly, the coefficients for beds and bathrooms are 0.07 and 0.1, respectively. Like in the previous model, the coefficients on the house and townhouse dummies changed, and they are now insignificant.   


``` {r}
  
 
  
   model4 <- lm(log_price_4_nights ~ prop_type_simplified+number_of_reviews+review_scores_rating+room_type+bathrooms+bedrooms+beds+accommodates, data=listings3)
  table4 <- get_regression_table(model4)
  table4
  vif(model4)
  autoplot(model4)


```

In the model 4, we add variable 'accommodates' which signifies the size of the accommodation. The coefficient on the variable 'accommodates' is 0.112, meaning that a unit increase increases price by around 11.85%, holding other variables constant. At this point, there is still no high multicollinearity among the variables, as Variance Inflation Factor (VIf) is not higher than 5 for any variable. 



``` {r}

  
  
  model5 <- lm(log_price_4_nights ~ prop_type_simplified+number_of_reviews+review_scores_rating+room_type+bathrooms+bedrooms+beds+accommodates+host_is_superhost+ is_location_exact, data=listings3)
  table5 <- get_regression_table(model5)
  table5
  vif(model5)
  autoplot(model5)

```


We can also check whether the facts that a host is a superhost and whether the location is exact lead to higher prices when controlling for other variables.  The coefficient on the superhost dummy is 0.11 meaning that being a superhost is correlated with roughly 11% higher prices, ceteris paribus. This coefficient is significant at 1 % level. On the other hand, the coefficient on the exact location dummy is positive and equals 0.027, however it is not significant at the 5% level.  Furthermore, the VIF remains the highest on 'accommodates' but it is still lower than 5. 


``` {r}
  

  
  

  model6 <- lm(log_price_4_nights ~ prop_type_simplified+number_of_reviews+review_scores_rating+room_type+bathrooms+bedrooms+beds+accommodates+host_is_superhost+ is_location_exact+cancellation_policy, data=listings3)
  table6 <- get_regression_table(model6)
  table6
  vif(model6)
  autoplot(model6)

```

In this model we add cancellation policy in the regression and see whether it correlates with the higher prices. Three dummies are created corresponding to strict, super strict and moderate cancellation policy, and the omitted group is 'flexible'. Compared to flexible, other types of policies seem to correlate with higher prices, however the estimate on the super strict dummy is not significant due to a very low sample size. Of course, this cannot be interpreted as a causal impact, and actually there might be a reverse relationship, since higher quality and hence more expensive apartments might have stricter policies for instance. 



``` {r}

   model7 <- lm(log_price_4_nights ~ prop_type_simplified+number_of_reviews+review_scores_rating+room_type+bathrooms+bedrooms+beds+accommodates+host_is_superhost+ is_location_exact+cancellation_policy+neighbourhood_simplified, data=listings3)
  table7 <- get_regression_table(model7)
  table7
  vif(model7)
  autoplot(model7)

```


Finally, we grouped 19 different neighbourhoods of Bruxelles according to their location in 5 groups: Central, West, North, East, and South. In this model, we will also control for these variables. The omitted group is 'Central' and the corresponding coefficients on dummies tell us about the gap between the particular region and the central part. Compared to the central part, other parts of the city correlate with lower prices holding other variables constant and estimates are statistically significant. Omitted variables bias in the previous regressions might be caused by location, as location is correlated with both price and the variables such as number_of_reviews, exact location, rating etc. In addition, we check whether this model satisfies the main LINE assumptions of linear regressions by using autoplot. We can see that there exists a clear linear relationship between price_4_nights and explanatory variables, and that residuals are roughly normally distributed except at the borders (Normal Q-Q plot). In addition, it seems that there is no heteroscedasticity and that variance of the residuals is constant. In addition, there is no multicollinearity. Hence, we will use this model to make a prediction.


```{r}
  
  #Overall table
  huxreg(model1,model2,model3,model4,model5,model6,model7,statistics = c('#observations' = 'nobs', 
                                'R squared' = 'r.squared', 
                                'Adj. R Squared' = 'adj.r.squared', 
                                'Residual SE' = 'sigma'))

```


```{r}
  
#Prediction
  
  model_data <- data.frame(prop_type_simplified = "Apartment",number_of_reviews = 10,review_scores_rating = 95,room_type="Private room",bathrooms=1,bedrooms=2,beds=2,host_is_superhost=FALSE,is_location_exact = FALSE, cancellation_policy = "moderate",neighbourhood_simplified = "Central",accommodates=2)
  
predict(model7,newdata=model_data, interval = "prediction")
```


Using model7, we try to predict the price of accommodation for 2 persons for 4 nights. We included the data shown in the chunk above. From this, we get that the price prediction is $198 with the wide confidence interval ranging from roughly 91 dollars to 425 dollars. 

