---
categories:
- ""
- ""
date: "2017-10-31T22:42:51-05:00"
description: 
image: 1218.jpg
keywords: ""
slug: blog4
title: Financial analysis of Stock returns
---

# Returns of financial stocks


> You may find useful the material on [finance data sources](https://mfa2021.netlify.app/reference/finance_data/). 

We will use the `tidyquant` package to download historical data of stock prices, calculate returns, and examine the distribution of returns. 

We must first identify which stocks we want to download data for, and for this we must know their ticker symbol; Apple is known as AAPL, Microsoft as MSFT, McDonald's as MCD, etc. The file `nyse.csv` contains 508 stocks listed on the NYSE, their ticker `symbol`, `name`, the IPO  (Initial Public Offering) year, and the sector and industry the company is in.



```{r load_nyse_data, message=FALSE, warning=FALSE}

nyse <- read_csv(here::here("data","nyse.csv"))

skimr::skim(nyse)

```

Based on this dataset, create a table and a bar plot that shows the number of companies per sector, in descending order

```{r companies_per_sector}
library(dplyr)
  sectors <- nyse %>% 
  group_by(sector) %>% 
  dplyr::count(sort=TRUE) 

ggplot(sectors, aes(x=n, y = reorder(sector, n))) + 
  geom_col() +
  labs(title = "NYSE Ranking - Largest Sectors", x="Number of companies", y="Sector") + 
  NULL

```

Next, let's choose the [Dow Jones Industrial Aveareg (DJIA)](https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average) stocks and their ticker symbols and download some data. Besides the thirty stocks that make up the DJIA, we will also add `SPY` which is an SP500 ETF (Exchange Traded Fund).


```{r, tickers_from_wikipedia}

djia_url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"

#get tables that exist on URL
tables <- djia_url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called djia. 
# Use purr::map() to create a list of all tables in URL
djia <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               clean_names())


# constituents
table1 <- djia[[2]] %>% # the second table on the page contains the ticker symbols
  mutate(date_added = ymd(date_added),
         
         # if a stock is listed on NYSE, its symbol is, e.g., NYSE: MMM
         # We will get prices from yahoo finance which requires just the ticker
         
         # if symbol contains "NYSE*", the * being a wildcard
         # then we jsut drop the first 6 characters in that string
         ticker = ifelse(str_detect(symbol, "NYSE*"),
                          str_sub(symbol,7,11),
                          symbol)
         )

# we need a vector of strings with just the 30 tickers + SPY
tickers <- table1 %>% 
  select(ticker) %>% 
  pull() %>% # pull() gets them as a sting of characters
  c("SPY") # and lets us add SPY, the SP500 ETF

```




```{r get_price_data, message=FALSE, warning=FALSE, cache=TRUE}
# Notice the cache=TRUE argument in the chunk options. Because getting data is time consuming, # cache=TRUE means that once it downloads data, the chunk will not run again next time you knit your Rmd

#Selecting stocks to analyse
library(tidyquant)
myStocks <- c("BAC", "BLK","C","GS","JPM","MS","TROW","SPY" ) %>%
  tq_get(get  = "stock.prices",
         from = "2010-09-01",
         to   = "2020-08-31") %>%
  group_by(symbol) 

#Taking a glimpse at our resulting dataframe
glimpse(myStocks)

```

Financial performance analysis depend on returns; If I buy a stock today for 100 and I sell it tomorrow for 101.75, my one-day return, assuming no transaction costs, is 1.75%. So given the adjusted closing prices, our first step is to calculate daily and monthly returns.


```{r calculate_returns, message=FALSE, warning=FALSE, cache=TRUE}
#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

#calculate yearly returns
myStocks_returns_annual <- myStocks %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic",
               col_rename = "yearly_returns",
               cols = c(nested.col))
```

Create a dataframe and assign it to a new object, where you summarise monthly returns since 2017-01-01 for each of the stocks and `SPY`; min, max, median, mean, SD.

```{r summarise_monthly_returns}

#Summarizing and calculating variables
monthly_returns <- myStocks_returns_monthly %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::summarise(min = min(monthly_returns),
            max = max(monthly_returns), 
            median=median(monthly_returns), 
            mean=mean(monthly_returns), 
            sd = sd(monthly_returns)) %>% 
  ungroup()

#Return the resulting table
monthly_returns

```


Plot a density plot, using `geom_density()`, for each of the stocks
```{r density_monthly_returns}

ggplot(myStocks_returns_monthly, aes(x=monthly_returns)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ symbol) +
  labs(title = "Distribution of Monthly Returns per Stock", x="Monthly return", y="Density") +
  theme_bw()+
  NULL

```

What can you infer from this plot? Which stock is the riskiest? The least risky? 


This graph show that the least riskier investment is the SPY (sd = 0.0382) since it has the smallest standard deviation comparing to the other stocks. BAC (sd = 0.0948) and MS (sd = 0.0920) are the riskier stocks because the present the highest standard deviation.


Finally, produce a plot that shows the expected monthly return (mean) of a stock on the Y axis and the risk (standard deviation) in the X-axis. Please use `ggrepel::geom_text_repel()` to label each stock with its ticker symbol

```{r risk_return_plot}

ggplot(monthly_returns, aes(x=sd, y=mean, colour=symbol)) +
  geom_point() +
  geom_text_repel(aes(label = symbol)) +
  labs(title = "Average Monthly Risk x Return by Stock", 
       x="Average Monthly Standard Deviation", 
       y="Average Monthly Return") +
  theme_bw()+
  NULL

```

What can you infer from this plot? Are there any stocks which, while being riskier, do not have a higher expected return?


1. From a portfolio construction point of view the financial sector is riskier (higher standard deviation) than the SPY and this additional risk is not justified by higher returns, in most of the cases.
2. Comparing to the SPY just JPM (JP Morgan), BLR (Black Rock) and TROW (T. Row Price) performed better than the SPY. This stocks presents higher standard deviation and also higher return. This means that the additional risk brought higher returns.
3. When comparing C (Citi), GS (Goldman Sachs), TROW (T. Row Price), BAC (Bank of America) and MS (Morgan Stanley) to SPY, none of this investments where good decisions since they have higher risk but equal or less return.
4. Grouping the big banks: BAC (Bank of America), C (Citi) and JPM (JP Morgan), JPM was the best investment since it showed the higher return and smallest standard deviation.
5. Grouping the investment banks: GS (Goldman Sachs) and MS (Morgan Stanley), MS had the best risk x return ratio.
6. Grouping the Asset Management firms: BLR (BlackRock) and TROW (T.Row Price), BLR had the best risk x return.
