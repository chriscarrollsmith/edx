#DELETE BEFORE SUBMITTING
setwd("C:/Users/chris/OneDrive/Documents/R Studio/Education")



# Data Acquisition --------------------------------------------------------



#Install any missing required R libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

#Load required R libraries
library(tidyverse)
library(tidyquant)
library(caret)

#Install and load the rvest package for basic web scraping
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
library(rvest)

#scrape Wikipedia page for table of current S&P500 tickers
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
tickers <- as.data.frame(url %>%
                           read_html() %>% #pull all HTML from the webpage
                           html_nodes(xpath = '//*[@id="constituents"]') %>%
                           html_table()) #get table using XPath

#Keep only symbol and date added columns
tickers <- tickers %>% 
  select(symbol = Symbol, date_added = Date.first.added) %>%
  mutate(date_added = as.Date(date_added))

#replace dots with dashes in select ticker symbols
tickers$symbol = case_when(
  tickers$symbol == "BRK.B" ~ "BRK-B",
  tickers$symbol == "BF.B" ~ "BF-B",
  #TRUE is equivalent to "else" statement
  TRUE ~ as.character(tickers$symbol)
)

#scrape Wikipedia page for table of former S&P500 tickers
removed_tickers <- as.data.frame(url %>%
                                   read_html() %>% #pull all HTML from the webpage
                                   html_nodes(xpath = '//*[@id="changes"]') %>%
                                   html_table()) #get table using XPath

#Get dates of index removal, identify bankrupt or delisted companies, and remove
#blank entries from the table
removed_tickers <- removed_tickers[-1,] %>%
  mutate(delisted = str_detect(Reason,"bankruptcy|delisted")) %>%
  select(symbol=Removed,removed_date=Date,delisted) %>%
  mutate(removed_date = as.Date(removed_date,format="%B %d, %Y")) %>%
  filter(symbol != "")  %>%
  filter(removed_date > as.Date("2007-01-01"))

#Find which stocks were added to the index more than once
removed_twice <- removed_tickers %>% 
  group_by(symbol) %>% 
  filter(n()>1) %>%
  pull(symbol) %>%
  unique()
added_twice <- tickers %>%
  filter(symbol %in% removed_tickers$symbol) %>%
  pull(symbol)

#Remove Allergan from the list because it wasn't really removed from index; 
#its name changed
removed_twice <- removed_twice[!removed_twice %in% c("AGN")]
removed_tickers <- removed_tickers[-last(which(removed_tickers$symbol == "AGN")),]

#Find how many stocks were added to and removed from the index more than once
n_added_twice <- length(removed_twice) + length(added_twice)

#Combine the tables of current and former tickers
sp500tickers <- bind_rows(tickers,removed_tickers)

#Download historical price data from Yahoo! Finance API
df_1 <- tq_get(unique(sp500tickers$symbol))

#Keep only the ticker symbol, date, and adjusted closing price
df_1 <- df_1 %>%
  select(symbol,date,adjusted)

#Find out which symbols we failed to obtain price data for
sp500tickers_remaining <- sp500tickers$symbol[!sp500tickers$symbol %in% df_1$symbol]

#NOTE: before running the following code, you must manually download and extract
#archive.zip file from https://www.kaggle.com/ehallmar/daily-historical-stock-prices-1970-2018
#into your working directory; this will require a free Kaggle account.

#Read Kaggle historical price data from .csv file
#and change column names & formats to match our Yahoo! Finance data
df_2 <- read.csv(file = 'historical_stock_prices.csv') %>%
  select(symbol = ticker,date,adjusted = adj_close) %>%
  mutate(date = as.Date(date))

#Keep only the ticker symbols for which we failed to get data from Yahoo! Finance
df_2 <- df_2 %>%
  filter(symbol %in% sp500tickers_remaining)

#Combine the data from both sources into a single data frame and remove NA values
df <- bind_rows(df_1,df_2) %>%
  filter(!is.na(adjusted)) %>% 
  arrange(symbol,date)

#Find which and how many ticker symbols are still missing from our price data
sp500tickers_remaining_2 <- unique(sp500tickers$symbol)[!unique(sp500tickers$symbol) %in% df$symbol]
length(sp500tickers_remaining_2)
length(unique(sp500tickers$symbol))

#NOTE: before running the following code, you must manually download and extract
#archive.zip file from https://www.kaggle.com/borismarjanovic/price-volume-data-for-all-us-stocks-etfs
#into a folder titled "Stocks" in your working directory; this will require a 
#free Kaggle account.

#Read Kaggle historical price data from .csv file
#and change column names & formats to match our Yahoo! Finance data
df_3 <- map_dfr(.x=sp500tickers_remaining_2,.f=function(ticker){
  dat <- tryCatch({read.csv(paste("./Stocks/",tolower(ticker),".us.txt",sep="")) %>%
      mutate(symbol = ticker,date=as.Date(Date,format="%Y-%m-%d"),adjusted=Close) %>%
      select(symbol,date,adjusted)},error = function(e){data.frame(ticker=c(),date=c(),adjusted=c())})
})

#Combine data from all three sources into a single data frame
df <- bind_rows(df,df_3)

#Find which and how many ticker symbols are still missing from our price data
sp500tickers_remaining_3 <- unique(sp500tickers$symbol)[!unique(sp500tickers$symbol) %in% df$symbol]
length(sp500tickers_remaining_3)
length(unique(sp500tickers$symbol))



# Data Cleaning -----------------------------------------------------------



#Add numbers to the end of tickers that did more than one stint in the index
redundant_tickers <- map(.x=c(removed_twice,added_twice),.f=function(ticker){
  sp500tickers$symbol[first(which(sp500tickers$symbol == ticker))] <<- paste(ticker,"2",sep="")
  sp500tickers$symbol[last(which(sp500tickers$symbol == ticker))] <<- paste(ticker,"1",sep="")
  ticker
}) %>% unlist()

#Remove original, redundant, unnumbered tickers
sp500tickers <- sp500tickers[!sp500tickers$symbol %in% redundant_tickers,]

#Create duplicate time series with numbered tickers in df
#Arrange ticker symbols alphabetically and dates chronologically
tmp <- df[df$symbol %in% c(removed_twice,added_twice),]
tmp$symbol <- paste(  tmp$symbol,"2",sep="")
df$symbol[df$symbol %in% c(removed_twice,added_twice)] <- paste(
  df$symbol[df$symbol %in% c(removed_twice,added_twice)],"1",sep="")
df <- bind_rows(df,tmp) %>%
  arrange(symbol,date)

#Based on research I performed manually, set the dates on which select stocks were 
#added to the index
sp500tickers[str_detect(sp500tickers$symbol,"\\d"),]
sp500tickers$date_added[sp500tickers$symbol == "MXIM2"] <- as.Date("2018-12-03")
sp500tickers$date_added[sp500tickers$symbol == "AGN"] <- as.Date("1999-04-12")
sp500tickers$date_added[sp500tickers$symbol == "OI"] <- as.Date("2009-01-02")
sp500tickers$date_added[sp500tickers$symbol == "GAS2"] <- as.Date("2011-12-13")
sp500tickers$date_added[sp500tickers$symbol == "CBE2"] <- as.Date("2011-11-18")
sp500tickers$date_added[sp500tickers$symbol == "AMD2"] <- as.Date("2017-03-20")
sp500tickers$date_added[sp500tickers$symbol == "DD2"] <- as.Date("2019-04-02")
sp500tickers$date_added[sp500tickers$symbol == "TER2"] <- as.Date("2020-09-21")
sp500tickers[str_detect(sp500tickers$symbol,"\\d"),]

#Remove tickers from df that were added to index after the end of my data or 
#removed from the index prior to 2007 or prior to the start of my data
map(.x=c(unique(df$symbol)),.f=function(ticker){
  removed <- FALSE
  if(!is.na(sp500tickers$removed_date[sp500tickers$symbol == ticker])){
    if(sp500tickers$removed_date[sp500tickers$symbol == ticker] <= as.Date("2007-01-01") | 
       sp500tickers$removed_date[sp500tickers$symbol == ticker] <= first(df$date[df$symbol == ticker])){
      df <<- df[df$symbol != ticker,]
      removed <- TRUE
    }
  }
  if(!is.na(sp500tickers$date_added[sp500tickers$symbol == ticker])){
    if(sp500tickers$date_added[sp500tickers$symbol == ticker] >= last(df$date[df$symbol == ticker])){
      df <<- df[df$symbol != ticker,]
      removed <- TRUE
    }
  }
  removed
}) %>% unlist() %>% sum()

#filter out all dates in df more than one day after a stock's removal from the index
map(.x = sp500tickers$symbol[!is.na(sp500tickers$removed_date)],.f = function(ticker){
  removed_date <- sp500tickers$removed_date[sp500tickers$symbol == ticker]
  next_date <- first(df$date[df$symbol == ticker & df$date > removed_date])
  if(length(next_date == 1)){
    df <<- df[!(df$date > next_date & df$symbol == ticker),]
  }
  ticker
})

#Get the maximum date found in the 1st Kaggle dataset and see how many of our price
#series in the df data frame end on this date
max_date_1 <- max(df_2$date)
kaggle_cutoff_1 <- df  %>%
  group_by(symbol) %>%
  filter(date==last(date))
kaggle_cutoff_1 <- kaggle_cutoff_1[kaggle_cutoff_1$date == max_date_1,]

#Get the maximum date found in the 2nd Kaggle dataset and see how many of our price
#series in the df data frame end on this date
max_date_2 <- max(df_3$date)
kaggle_cutoff_2 <- df  %>%
  group_by(symbol) %>%
  filter(date==last(date))
kaggle_cutoff_2 <- kaggle_cutoff_2[kaggle_cutoff_2$date == max_date_2,]

#Ensure that delisted/bankrupt stocks have an accurate terminal value
df %>%
  group_by(symbol) %>% 
  filter(symbol %in% removed_tickers$symbol[removed_tickers$delisted == TRUE]) %>%
  filter(date == last(date))



# Data Processing ---------------------------------------------------------



#create empty columns for next 21 days and next 253 days % return
df <- df %>%
  mutate(return_21 = NA,
         return_253 = NA)

#populate % return column for each ticker symbol, but only for dates after 01-01-2007
for(x in unique(df$symbol)){
  tmp <- df[df$symbol == x & df$date >= as.Date("2007-01-01"),]
  if(nrow(tmp) > 21){
    tmp$return_21 <- c((tmp$adjusted[22:nrow(tmp)] - tmp$adjusted[1:(nrow(tmp)-21)])/tmp$adjusted[1:(nrow(tmp)-21)],rep(NA,times=21))
    df$return_21[df$symbol == x & df$date >= as.Date("2007-01-01")] <- tmp$return_21
  }
  if(nrow(tmp) > 253){
    tmp$return_253 <- c((tmp$adjusted[254:nrow(tmp)] - tmp$adjusted[1:(nrow(tmp)-253)])/tmp$adjusted[1:(nrow(tmp)-253)],rep(NA,times=253))
    df$return_253[df$symbol == x & df$date >= as.Date("2007-01-01")] <- tmp$return_253
  }
}

#For comparability of returns over different time periods, convert returns to 
#compound daily returns (CDRs): (ending_value/beginning_value)^(1/days) - 1
df <- df %>%
  mutate(cdr_21 = ((adjusted*(1+return_21))/adjusted)^(1/21) - 1,
         cdr_253 = ((adjusted*(1+return_253))/adjusted)^(1/253) - 1)

#Verify that all ticker symbols have at least one non-NA value for 21-day return
df %>%
  group_by(symbol) %>%
  summarize(good = any(!is.na(return_21))) %>%
  filter(good = FALSE)

#Get a list of symbols whose removal from the index is included in data
max_date <- max(df$date)
removed <- df %>%
  group_by(symbol) %>%
  summarize(last_date = last(date)) %>%
  filter(!last_date %in% c(max_date,max_date_1,max_date_2)) %>%
  pull(symbol)
sp500tickers[sp500tickers$symbol %in% removed,]

#For these symbols, return for their final days is calculated as difference 
#from terminal value
for(x in removed){
  tmp <- df[df$symbol == x & df$date >= as.Date("2007-01-01"),]
  terminal_price <- tmp$adjusted[nrow(tmp)]
  tmp$return_21[(nrow(tmp)-sum(is.na(tmp$return_21))+1):(nrow(tmp)-1)] <- 
    (terminal_price - tmp$adjusted[(nrow(tmp)-sum(is.na(tmp$return_21))+1):(nrow(tmp)-1)])/tmp$adjusted[(nrow(tmp)-sum(is.na(tmp$return_21))+1):(nrow(tmp)-1)]
  tmp$return_253[(nrow(tmp)-sum(is.na(tmp$return_253))+1):(nrow(tmp)-1)] <-
    (terminal_price - tmp$adjusted[(nrow(tmp)-sum(is.na(tmp$return_253))+1):(nrow(tmp)-1)])/tmp$adjusted[(nrow(tmp)-sum(is.na(tmp$return_253))+1):(nrow(tmp)-1)]
  df[df$symbol == x & df$date >= as.Date("2007-01-01"),] <- tmp
}

#Identify aberrant returns
df %>% filter(return_21 > 6 | return_21 < -0.85) %>% View()
df %>% filter(symbol == "LLL" & between(date,as.Date("2014-12-01"),as.Date("2016-01-01"))) %>% View() #One glaring error; also possibly not even the right price series
df %>% filter(symbol == "CBE2" & between(date,as.Date("2012-04-01"),as.Date("2013-01-01"))) %>% View() #Lots of errors
df %>% filter(symbol == "NYX" & between(date,as.Date("2013-11-01"),as.Date("2015-01-01"))) %>% View() #Last data point shouldn't be there
df %>% filter(symbol == "TIE" & between(date,as.Date("2012-01-01"),as.Date("2012-06-29"))) %>% View() #Lots of errors

#Delete tickers with systematic data entry errors
df <- df %>% filter(!symbol %in% c("TIE","CBE2","LLL","NYX"))

# #Correct erroneous data points
# df <- df[-which(df$symbol=="NYX" & df$date == as.Date("2014-12-31")),]

#filter out NAs and add empty SMA columns
df <- df %>%
  group_by(symbol) %>%
  filter(sum(is.na(adjusted))<2) %>%
  filter(!is.na(adjusted)) %>%
  mutate(sma_7 = NA,
         sma_20 = NA,
         sma_50 = NA,
         sma_100 = NA,
         sma_200 = NA,
         sma_500 = NA)

#add SMAs
for(x in unique(df$symbol)){
  tmp <- df[df$symbol == x,]
  if(nrow(tmp)>7){
    tmp$sma_7 <- SMA(tmp$adjusted,n=7)
    df$sma_7[df$symbol == x] <- tmp$sma_7
  }
  if(nrow(tmp)>20){
    tmp$sma_20 <- SMA(tmp$adjusted,n=20)
    df$sma_20[df$symbol == x] <- tmp$sma_20
  }
  if(nrow(tmp)>50){
    tmp$sma_50 <- SMA(tmp$adjusted,n=50)
    df$sma_50[df$symbol == x] <- tmp$sma_50
  }
  if(nrow(tmp)>100){
    tmp$sma_100 <- SMA(tmp$adjusted,n=100)
    df$sma_100[df$symbol == x] <- tmp$sma_100
  }
  if(nrow(tmp)>200){
    tmp$sma_200 <- SMA(tmp$adjusted,n=200)
    df$sma_200[df$symbol == x] <- tmp$sma_200
  }
  if(nrow(tmp)>500){
    tmp$sma_500 <- SMA(tmp$adjusted,n=500)
    df$sma_500[df$symbol == x] <- tmp$sma_500
  }
}

#add % distance from SMAs
df <- df %>%
  mutate(distance_from_7 = (adjusted - sma_7)/sma_7,
         distance_from_20 = (adjusted - sma_20)/sma_20,
         distance_from_50 = (adjusted - sma_50)/sma_50,
         distance_from_100 = (adjusted - sma_100)/sma_100,
         distance_from_200 = (adjusted - sma_200)/sma_200,
         distance_from_500 = (adjusted - sma_500)/sma_500)

#Plot distribution of distances to illustrate that they are normally distributed
df %>%
  filter(!is.na(distance_from_100) & between(distance_from_100,-1,1)) %>%
  ggplot(aes(x=distance_from_100)) +
  geom_histogram(bins=40) +
  geom_vline(xintercept=mean(df$distance_from_100,na.rm=TRUE)) +
  scale_x_continuous(limits = c(-1,1),
                     labels = scales::label_percent()) +
  labs(title = "Distribution of distances from 100-day SMA",
       x="Distance from 100-day SMA",
       y="Count")

#add empty z-score columns
df <- df %>%
  mutate(z_score_7 = NA,
         z_score_20 = NA,
         z_score_50 = NA,
         z_score_100 = NA,
         z_score_200 = NA,
         z_score_500 = NA)

#add z-score distance from EMAs, using only data prior to each date
#WARNING: This code takes a long time to run!
for(a in 1:nrow(df)){
  if(df$date[a] >= as.Date("2007-01-01")){
    tmp <- df[df$symbol == df$symbol[a] & df$date <= df$date[a],]
    df$z_score_7[a] <- (tmp$distance_from_7[nrow(tmp)] - mean(tmp$distance_from_7,na.rm=T))/sd(tmp$distance_from_7,na.rm=T)
    df$z_score_20[a] <- (tmp$distance_from_20[nrow(tmp)] - mean(tmp$distance_from_20,na.rm=T))/sd(tmp$distance_from_20,na.rm=T)
    df$z_score_50[a] <- (tmp$distance_from_50[nrow(tmp)] - mean(tmp$distance_from_50,na.rm=T))/sd(tmp$distance_from_50,na.rm=T)
    df$z_score_100[a] <- (tmp$distance_from_100[nrow(tmp)] - mean(tmp$distance_from_100,na.rm=T))/sd(tmp$distance_from_100,na.rm=T)
    df$z_score_200[a] <- (tmp$distance_from_200[nrow(tmp)] - mean(tmp$distance_from_200,na.rm=T))/sd(tmp$distance_from_200,na.rm=T)
    df$z_score_500[a] <- (tmp$distance_from_500[nrow(tmp)] - mean(tmp$distance_from_500,na.rm=T))/sd(tmp$distance_from_500,na.rm=T)
  }
}

#Add 20-day moving averages of the various z-scores
df_2 <- df %>%
  filter(sum(!is.na(z_score_7))>20) %>%
  mutate(zscore_average_7 = SMA(z_score_7,20)) 
df_3 <- df %>%
  filter(sum(!is.na(z_score_7))<20)
df <- bind_rows(df_2,df_3)
df_2 <- df %>%
  filter(sum(!is.na(z_score_20))>20) %>%
  mutate(zscore_average_20 = SMA(z_score_20,20)) 
df_3 <- df %>%
  filter(sum(!is.na(z_score_20))<20)
df <- bind_rows(df_2,df_3)
df_2 <- df %>%
  filter(sum(!is.na(z_score_50))>20) %>%
  mutate(zscore_average_50 = SMA(z_score_50,20)) 
df_3 <- df %>%
  filter(sum(!is.na(z_score_50))<20)
df <- bind_rows(df_2,df_3)
df_2 <- df %>%
  filter(sum(!is.na(z_score_100))>20) %>%
  mutate(zscore_average_100 = SMA(z_score_100,20)) 
df_3 <- df %>%
  filter(sum(!is.na(z_score_100))<20)
df <- bind_rows(df_2,df_3)
df_2 <- df %>%
  filter(sum(!is.na(z_score_200))>20) %>%
  mutate(zscore_average_200 = SMA(z_score_200,20)) 
df_3 <- df %>%
  filter(sum(!is.na(z_score_200))<20)
df <- bind_rows(df_2,df_3)
df_2 <- df %>%
  filter(sum(!is.na(z_score_500))>20) %>%
  mutate(zscore_average_500 = SMA(z_score_500,20)) 
df_3 <- df %>%
  filter(sum(!is.na(z_score_500))<20)
df <- bind_rows(df_2,df_3)

#Find diffs between z-score and 20-day SMA of z-score
df <- df %>%
  mutate(diff_7 = z_score_20 - zscore_average_7,
         diff_20 = z_score_20 - zscore_average_20,
         diff_50 = z_score_50 - zscore_average_50,
         diff_100 = z_score_100 - zscore_average_100,
         diff_200 = z_score_200 - zscore_average_200,
         diff_500 = z_score_500 - zscore_average_500)

#Add lifetime daily return to-date for individual stocks and for all stocks

#add empty z-score columns
df <- df %>%
  mutate(lifetime_return_to_date = NA,
         trading_days_to_date = NA)

#Add lifetime return to-date and number of trading days to-date
for(a in 1:nrow(df)){
  if(df$date[a] >= as.Date("2007-01-01")){
    tmp <- df[df$symbol == df$symbol[a] & df$date <= df$date[a],]
    df$lifetime_return_to_date[a] <- (tmp$adjusted[nrow(tmp)] - tmp$adjusted[1])/tmp$adjusted[1]
    df$trading_days_to_date[a] <- nrow(tmp)-1
  }
}

#Convert lifetime returns to compound daily returns (CDRs): 
#(ending_value/beginning_value)^(1/days) - 1
df <- df %>%
  mutate(lifetime_cdr_to_date = (adjusted/(adjusted/(1+lifetime_return_to_date)))^(1/trading_days_to_date) - 1)

#Add index-level average daily lifetime-to-date return for all stocks in the data set
sp500 <- df %>%
  filter(date >= as.Date("2007-01-01")) %>%
  ungroup() %>%
  group_by(date) %>%
  summarize(avg_sp500_cdr_to_date = mean(lifetime_cdr_to_date))
df <- left_join(df,sp500,by="date")

#Add average z-scores for the entire index for each date
sp500 <- df %>%
  filter(date >= as.Date("2007-01-01")) %>%
  ungroup() %>%
  group_by(date) %>%
  summarize(avg_sp500_zscore_7 = mean(z_score_7,na.rm=TRUE),
            avg_sp500_zscore_20 = mean(z_score_20,na.rm=TRUE),
            avg_sp500_zscore_50 = mean(z_score_50,na.rm=TRUE),
            avg_sp500_zscore_100 = mean(z_score_100,na.rm=TRUE),
            avg_sp500_zscore_200 = mean(z_score_200,na.rm=TRUE),
            avg_sp500_zscore_500 = mean(z_score_500,na.rm=TRUE))
df <- left_join(df,sp500,by="date")

#Take average of all z-scores for the stock and the index, add percentile rank
df <- df %>% 
  mutate(stock_zscore_avg = (z_score_7 +
                                   z_score_20 +
                                   z_score_50 +
                                   z_score_100 +
                                   z_score_200 +
                                   z_score_500)/6,
         sp500_zscore_avg = (avg_sp500_zscore_7 +
                                   avg_sp500_zscore_20 +
                                   avg_sp500_zscore_50 +
                                   avg_sp500_zscore_100 +
                                   avg_sp500_zscore_200 +
                                   avg_sp500_zscore_500)/6) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(pctrank = rank(stock_zscore_avg)/length(stock_zscore_avg))



# Data Partitioning -------------------------------------------------------



#Remove rows with NA returns
df_tmp <- df %>% 
  filter(!is.na(cdr_21))
nrow(df_tmp)

#Partition chronologically with 80% of dates in training set, and 10% in test & validation
end_train <- 8*round(length(df_tmp$date)/10)
train <- df_tmp[1:end_train,]
end_test <- 9*round(length(df_tmp$date)/10)
test <- df_tmp[(end_train+1):end_test,]
validation <- df_tmp[(end_test+1):nrow(df_tmp),]

# #Partition randomly
# # Validation set will be 20% of data
# set.seed(1, sample.kind="Rounding")
# test_index <- createDataPartition(y = df_tmp$cdr_21, times = 2, p = 0.1, list = FALSE)
# train <- df_tmp[-c(test_index[,1],test_index[,2]),]
# test <- df_tmp[test_index[,1],]
# validate <- df_tmp[test_index[,2],]
# rm(df_tmp)
# 
#Code for creating two partitions
# #Remove rows with NA returns
# df_21 <- df %>% 
#   filter(!is.na(return_21))
# nrow(df_21)
# df_253 <- df %>% 
#   filter(!is.na(return_253))
# nrow(df_253)
# 
# #Partition for forecasting 21-day return
# # Validation set will be 20% of data
# set.seed(1, sample.kind="Rounding")
# test_index <- createDataPartition(y = df_21$return_21, times = 2, p = 0.1, list = FALSE)
# train_21 <- df_21[-c(test_index[,1],test_index[,2]),]
# test_21 <- df_21[test_index[,1],]
# validate_21 <- df_21[test_index[,2],]
# 
# #Partition for forecasting 253-day return
# # Validation set will be 10% of data
# set.seed(1, sample.kind="Rounding")
# test_index <- createDataPartition(y = df_253$return_253, times = 2, p = 0.1, list = FALSE)
# train_253 <- df_253[-c(test_index[,1],test_index[,2]),]
# test_253 <- df_253[test_index[,1],]
# validate_253 <- df_253[test_index[,2],]

#Define function to get residual mean squared error of predictions
RMSE <- function(true_return, predicted_return){
  sqrt(mean((true_return - predicted_return)^2))
}

#Remove unnecessary variables & save workspace as stockreturnscapstone.RData
rm(df_1,df_2,df_3,df_253,tmp,terminal_price,sp500)
save.image(file = "stockreturnscapstone.RData")

#Load saved workspace
load("stockreturnscapstone.RData")




# Data Exploration --------------------------------------------------------



#Note: Should I partition data into ten periods rather than a random sample from
#all periods? This way I get a sense of the accuracy for predicting out-of-sample
#data.

#Calculate correlation coefficients for historical CDR against future return
data.frame(`return 21-day` = cor(train$lifetime_cdr_to_date,train$cdr_21,use="complete.obs"),
           `return 253-day` = cor(train$lifetime_cdr_to_date,train$cdr_253,use="complete.obs")) %>% 
  knitr::kable(caption="Correlation coefficients for historical CDR vs. future CDR")

#Plot historical CDR against future return
facet_plot <- train %>% 
  ungroup() %>%
  mutate(historical_cdr = round(lifetime_cdr_to_date*10000)/10000) %>%
  group_by(historical_cdr) %>%
  summarize(mean_cdr_21 = mean(cdr_21),
            mean_cdr_253 = mean(cdr_253,na.rm=TRUE),
            n=n()) %>%
  filter(n>20000)
facet_plot <- gather(facet_plot,duration,cdr,2:3)
facet_plot$duration[facet_plot$duration == "mean_cdr_21"] <- "21-day CDR"
facet_plot$duration[facet_plot$duration == "mean_cdr_253"] <- "253-day CDR"
facet_plot %>%
  ggplot(aes(x=historical_cdr,y=cdr,col=duration)) +
  geom_line() +
  geom_point(aes(size=n)) +
  labs(title="Historical compound daily return vs. next 21-days and 253-days compound daily return",
       x="Historical compound daily return",
       y="Next period compound daily return")

#calculate correlation coefficients for z-score vs. 21-day cdr
data.frame(z_score_7 = cor(train$z_score_7,train$cdr_21,use="complete.obs"),
           z_score_20 = cor(train$z_score_20,train$cdr_21,use="complete.obs"),
           z_score_50 = cor(train$z_score_50,train$cdr_21,use="complete.obs"),
           z_score_100 = cor(train$z_score_100,train$cdr_21,use="complete.obs"),
           z_score_200 = cor(train$z_score_200,train$cdr_21,use="complete.obs"),
           z_score_500 = cor(train$z_score_500,train$cdr_21,use="complete.obs")) %>% 
  knitr::kable(caption="Correlation coefficients for z-score vs. 21-day return")

#calculate correlation coefficients  for z-score vs. 253-day return
data.frame(z_score_7 = cor(train$z_score_7,train$cdr_253,use="complete.obs"),
           z_score_20 = cor(train$z_score_20,train$cdr_253,use="complete.obs"),
           z_score_50 = cor(train$z_score_50,train$cdr_253,use="complete.obs"),
           z_score_100 = cor(train$z_score_100,train$cdr_253,use="complete.obs"),
           z_score_200 = cor(train$z_score_200,train$cdr_253,use="complete.obs"),
           z_score_500 = cor(train$z_score_500,train$cdr_253,use="complete.obs")) %>% 
  knitr::kable(caption="Correlation coefficients for z-score vs. 253-day return")

# #Calculate linear models for z-score vs 21-day return
# line_7_21 <- lm(train$return_21~df$z_score_7)
# line_20_21 <- lm(train$return_21~df$z_score_20)
# line_50_21 <- lm(train$return_21~df$z_score_50)
# line_100_21 <- lm(train$return_21~df$z_score_100)
# line_200_21 <- lm(train$return_21~df$z_score_200)
# line_500_21 <- lm(train$return_21~df$z_score_500)
# 
# #Calculate linear models for z-score vs 253-day return
# line_7_253 <- lm(train$return_253~df$z_score_7)
# line_20_253 <- lm(train$return_253~df$z_score_20)
# line_50_253 <- lm(train$return_253~df$z_score_50)
# line_100_253 <- lm(train$return_253~df$z_score_100)
# line_200_253 <- lm(train$return_253~df$z_score_200)
# line_500_253 <- lm(train$return_253~df$z_score_500)

#stratify by z-score and plot mean and 95% CI for return for each stratum
facet_plot <- train %>%
  ungroup() %>%
  mutate(`7-day moving average` = round(z_score_7),
         `20-day moving average` = round(z_score_20),
         `50-day moving average` = round(z_score_50),
         `100-day moving average` = round(z_score_100),
         `200-day moving average` = round(z_score_200),
         `500-day moving average` = round(z_score_500)) %>%
  select(`7-day moving average`,`20-day moving average`,`50-day moving average`,
         `100-day moving average`,`200-day moving average`,`500-day moving average`,cdr_21)
facet_plot <- gather(facet_plot,moving_average,z_score,1:6) %>%
  mutate(moving_average = factor(moving_average,levels=c("7-day moving average",
                                                         "20-day moving average",
                                                         "50-day moving average",
                                                         "100-day moving average",
                                                         "200-day moving average",
                                                         "500-day moving average"))) %>%
  group_by(moving_average,z_score) %>%
  summarize(n = n(),
            mean_return = mean(cdr_21),
            se_return = sd(cdr_21)/sqrt(n)) %>% 
  filter(between(z_score,-8,8)) 
facet_plot %>%
  ggplot(aes(x=z_score,y=mean_return)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0),linetype=2) +
  geom_errorbar(aes(ymin = mean_return - 2*se_return, ymax = mean_return + 2*se_return)) +
  labs(title = "Mean return by z-score distance from various simple moving averages",
       x="Z-score stratum",
       y="Mean return") + 
  scale_y_continuous(limits = c(-.03,.02)) +
  facet_wrap( ~ moving_average)

#Recreate the above plot for 253-day return

#stratify by z-score and plot mean and 95% CI for return for each stratum
facet_plot <- train %>%
  ungroup() %>%
  filter(!is.na(return_253)) %>%
  mutate(`7-day moving average` = round(z_score_7),
         `20-day moving average` = round(z_score_20),
         `50-day moving average` = round(z_score_50),
         `100-day moving average` = round(z_score_100),
         `200-day moving average` = round(z_score_200),
         `500-day moving average` = round(z_score_500)) %>%
  select(`7-day moving average`,`20-day moving average`,`50-day moving average`,
         `100-day moving average`,`200-day moving average`,`500-day moving average`,cdr_253)
facet_plot <- gather(facet_plot,moving_average,z_score,1:6) %>%
  mutate(moving_average = factor(moving_average,levels=c("7-day moving average",
                                                         "20-day moving average",
                                                         "50-day moving average",
                                                         "100-day moving average",
                                                         "200-day moving average",
                                                         "500-day moving average"))) %>%
  group_by(moving_average,z_score) %>%
  summarize(n = n(),
            mean_return = mean(cdr_253),
            se_return = sd(cdr_253)/sqrt(n)) %>% 
  filter(between(z_score,-5,5)) 
facet_plot %>%
  ggplot(aes(x=z_score,y=mean_return)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0),linetype=2) +
  geom_errorbar(aes(ymin = mean_return - 2*se_return, ymax = mean_return + 2*se_return)) +
  labs(title = "Mean return by z-score distance from various simple moving averages",
       x="Z-score stratum",
       y="Mean return") + 
  scale_y_continuous() +
  facet_wrap( ~ moving_average)

#Double stratify 20-day moving average of z-score and its difference from same-day z-score
#and create a heat map showing 21-day return
facet_plot <- train %>% 
  ungroup() %>%
  mutate(`7-day moving average avg` = round(zscore_average_7),
         `20-day moving average avg` = round(zscore_average_20),
         `50-day moving average avg` = round(zscore_average_50),
         `100-day moving average avg` = round(zscore_average_100),
         `200-day moving average avg` = round(zscore_average_200),
         `500-day moving average avg` = round(zscore_average_500)) %>%
  mutate(`7-day moving average` = round(diff_7),
         `20-day moving average` = round(diff_20),
         `50-day moving average` = round(diff_50),
         `100-day moving average` = round(diff_100),
         `200-day moving average` = round(diff_200),
         `500-day moving average` = round(diff_500)) %>%
  select(`7-day moving average avg`,`20-day moving average avg`,`50-day moving average avg`,
         `100-day moving average avg`,`200-day moving average avg`,`500-day moving average avg`,
         `7-day moving average`,`20-day moving average`,
         `50-day moving average`,`100-day moving average`,
         `200-day moving average`,`500-day moving average`,
         cdr_21,cdr_253)
facet_plot <- gather(facet_plot,moving_average,ma_z_score,1:6)
facet_plot <- gather(facet_plot,moving_average,diff_z_score,1:6) %>%
  mutate(moving_average = factor(moving_average,levels=c("7-day moving average",
                                                         "20-day moving average",
                                                         "50-day moving average",
                                                         "100-day moving average",
                                                         "200-day moving average",
                                                         "500-day moving average"))) 
facet_plot <- facet_plot %>%
  group_by(moving_average,ma_z_score,diff_z_score) %>%
  summarize(n = n(),
            mean_return = mean(cdr_21),
            se_return = sd(cdr_21)/sqrt(n)) %>% 
  filter(between(ma_z_score,-8,8) & between(diff_z_score,-8,8)) 
facet_plot %>%
  ggplot(aes(x=ma_z_score,y=diff_z_score,fill=mean_return)) +
  geom_tile() +
  geom_hline(aes(yintercept=0),linetype=2) +
  geom_vline(aes(xintercept=0),linetype=2) +
  labs(title = "Mean 21-day return by z-score distance from various simple moving averages",
       x="Trailing 20-day average of z-score",
       y="Today's z-score difference from 20-day average") + 
  scale_fill_gradient(low="red",high="green") +
  facet_wrap( ~ moving_average)

#Double stratify 20-day moving average of z-score and its difference from same-day z-score
#and create a heat map showing 253-day return
facet_plot <- train %>% 
  filter(!is.na(cdr_253)) %>%
  ungroup() %>%
  mutate(`7-day moving average avg` = round(zscore_average_7),
         `20-day moving average avg` = round(zscore_average_20),
         `50-day moving average avg` = round(zscore_average_50),
         `100-day moving average avg` = round(zscore_average_100),
         `200-day moving average avg` = round(zscore_average_200),
         `500-day moving average avg` = round(zscore_average_500)) %>%
  mutate(`7-day moving average` = round(diff_7),
         `20-day moving average` = round(diff_20),
         `50-day moving average` = round(diff_50),
         `100-day moving average` = round(diff_100),
         `200-day moving average` = round(diff_200),
         `500-day moving average` = round(diff_500)) %>%
  select(`7-day moving average avg`,`20-day moving average avg`,`50-day moving average avg`,
         `100-day moving average avg`,`200-day moving average avg`,`500-day moving average avg`,
         `7-day moving average`,`20-day moving average`,
         `50-day moving average`,`100-day moving average`,
         `200-day moving average`,`500-day moving average`,
         cdr_21,cdr_253)
facet_plot <- gather(facet_plot,moving_average,ma_z_score,1:6)
facet_plot <- gather(facet_plot,moving_average,diff_z_score,1:6) %>%
  mutate(moving_average = factor(moving_average,levels=c("7-day moving average",
                                                         "20-day moving average",
                                                         "50-day moving average",
                                                         "100-day moving average",
                                                         "200-day moving average",
                                                         "500-day moving average"))) 
facet_plot <- facet_plot %>%
  group_by(moving_average,ma_z_score,diff_z_score) %>%
  summarize(n = n(),
            mean_return = mean(cdr_253),
            se_return = sd(cdr_253)/sqrt(n)) %>% 
  filter(between(ma_z_score,-8,8) & between(diff_z_score,-8,8)) 
facet_plot %>%
  ggplot(aes(x=ma_z_score,y=diff_z_score,fill=mean_return)) +
  geom_tile() +
  geom_hline(aes(yintercept=0),linetype=2) +
  geom_vline(aes(xintercept=0),linetype=2) +
  labs(title = "Mean 253-day return by z-score distance from various simple moving averages",
       x="Trailing 20-day average of z-score",
       y="Today's z-score difference from 20-day average") + 
  scale_fill_gradient(low="red",high="green") +
  facet_wrap( ~ moving_average)




# Building a Forecasting Model --------------------------------------------



#Calculate RMSE using a forecast of zero for all CDR values in test set
actual_21 <- test$cdr_21
actual_253 <- test$cdr_253[!is.na(test$cdr_253)]
model_rmses <- data.frame(model = "Zero",rmse_21_day = RMSE(actual_21,0),rmse_253_day = RMSE(actual_253,0))
model_rmses %>% 
  knitr::kable(caption="Model RMSEs")

#Calculate RMSE using average S&P 500 CDR to-date
actual_21 <- test$cdr_21
actual_253 <- test$cdr_253[!is.na(test$cdr_253)]
forecast_21 <- test$avg_sp500_cdr_to_date
forecast_253 <- test$avg_sp500_cdr_to_date[!is.na(test$cdr_253)]
model_rmses <- bind_rows(model_rmses,data.frame(model = "Mean S&P 500 CDR to-date",rmse_21_day = RMSE(actual_21,forecast_21),rmse_253_day = RMSE(actual_253,forecast_253)))  
model_rmses %>% 
  knitr::kable(caption="Model RMSEs")

#Calculate RMSE using individual stock's lifetime CDR to-date
actual_21 <- test$cdr_21 - test$avg_sp500_cdr_to_date
actual_253 <- test$cdr_253[!is.na(test$cdr_253)] - test$avg_sp500_cdr_to_date[!is.na(test$cdr_253)]
forecast_21 <- test$lifetime_cdr_to_date - test$avg_sp500_cdr_to_date
forecast_253 <- test$lifetime_cdr_to_date[!is.na(test$cdr_253)] - test$avg_sp500_cdr_to_date[!is.na(test$cdr_253)]
model_rmses <- bind_rows(model_rmses,data.frame(model = "Individual stock mean CDR to-date",rmse_21_day = RMSE(actual_21,forecast_21),rmse_253_day = RMSE(actual_253,forecast_253)))
model_rmses %>% 
  knitr::kable(caption="Model RMSEs")

#Attempt to forecast using regularization & five-fold cross-validation for tuning
#Set range of values to try for lambda
lambda <- seq(1, 4000, 50)

#Create folds for 5-fold cross validation to tune lambda
set.seed(1,sample.kind="Rounding")
folds <- createFolds(train$cdr_21, k = 5, list = TRUE, returnTrain = FALSE)

#Perform a five-fold cross-validation to tune lambda
lambdas <- map_dfr(.x=1:5,.f=function(curr_fold){
  #Create fold
  tune_set <- train[folds[[curr_fold]],]
  
  #See which value of lambda minimizes RMSE when predicting on tune_set
  lambdas <- map_dfr(.x=lambda,.f=function(lambda){
    
    #sweep out S&P 500 averages from the actuals
    actual_21 <- tune_set$cdr_21 - tune_set$avg_sp500_cdr_to_date
    actual_21 <- actual_21[!is.na(tune_set$cdr_21)]
    actual_253 <- tune_set$cdr_253 - tune_set$avg_sp500_cdr_to_date
    actual_253 <- actual_253[!is.na(tune_set$cdr_253)]
    
    #sweep out S&P 500 averages from the forecasts
    forecast_21 <- ((tune_set$adjusted/(tune_set$adjusted/(1+tune_set$lifetime_return_to_date)))^(1/(tune_set$trading_days_to_date + lambda)) - 1) - tune_set$avg_sp500_cdr_to_date
    forecast_21 <- forecast_21[!is.na(tune_set$cdr_21)]
    forecast_253 <- ((tune_set$adjusted/(tune_set$adjusted/(1+tune_set$lifetime_return_to_date)))^(1/(tune_set$trading_days_to_date + lambda)) - 1) - tune_set$avg_sp500_cdr_to_date
    forecast_253 <- forecast_253[!is.na(tune_set$cdr_253)]
    
    #calculate rmses
    rmses <- data.frame(fold = curr_fold,lambda,rmse_21_day = RMSE(actual_21,forecast_21),rmse_253_day = RMSE(actual_253,forecast_253))
  })
})

#Average rmses over the five folds
lambdas <- lambdas %>%
  group_by(lambda) %>%
  summarize(rmse_21_day = mean(rmse_21_day),rmse_253_day = mean(rmse_253_day))

#Plot values of lambda against rmses
lambdas <- gather(lambdas,cdr,rmse,2:3)
lambdas$cdr[lambdas$cdr == "rmse_21_day"] <- "21-day CDR"
lambdas$cdr[lambdas$cdr == "rmse_253_day"] <- "253-day CDR"
lambdas %>% ggplot(aes(x=lambda,y=rmse)) + 
  geom_point() +
  facet_wrap( ~ cdr,scales="free") +
  labs(title = "Tuning results for correction factor lambda")

#use the best value of lambda to forecast on the test set and calculate RMSE
#sweep out S&P 500 averages from the actuals
lambda <- lambdas %>%
  filter(cdr=="253-day CDR") %>% 
  filter(rmse == min(rmse)) %>%
  pull(lambda)
actual_21 <- test$cdr_21 - test$avg_sp500_cdr_to_date
actual_21 <- actual_21[!is.na(test$cdr_21)]
actual_253 <- test$cdr_253 - test$avg_sp500_cdr_to_date
actual_253 <- actual_253[!is.na(test$cdr_253)]

#sweep out S&P 500 averages from the forecasts
forecast_21 <- ((test$adjusted/(test$adjusted/(1+test$lifetime_return_to_date)))^(1/(test$trading_days_to_date + lambda)) - 1) - test$avg_sp500_cdr_to_date
forecast_21 <- forecast_21[!is.na(test$cdr_21)]
forecast_253 <- ((test$adjusted/(test$adjusted/(1+test$lifetime_return_to_date)))^(1/(test$trading_days_to_date + lambda)) - 1) - test$avg_sp500_cdr_to_date
forecast_253 <- forecast_253[!is.na(test$cdr_253)]

#calculate rmses
model_rmses <- bind_rows(model_rmses,data.frame(model = "Individual stock regularized mean CDR to-date",rmse_21_day = RMSE(actual_21,forecast_21),rmse_253_day = RMSE(actual_253,forecast_253))) 
model_rmses %>% 
  knitr::kable(caption="Model RMSEs")

#sweep out averages and stratify by z-score
train_loess <- train %>%
  ungroup() %>%
  mutate(`7-day moving average` = round(z_score_7),
         `20-day moving average` = round(z_score_20),
         `50-day moving average` = round(z_score_50),
         `100-day moving average` = round(z_score_100),
         `200-day moving average` = round(z_score_200),
         `500-day moving average` = round(z_score_500)) 
train_loess$swept_21 <- train_loess$cdr_21 - ((train_loess$adjusted/(train_loess$adjusted/(1+train_loess$lifetime_return_to_date)))^(1/(train_loess$trading_days_to_date + lambda)) - 1) - train_loess$avg_sp500_cdr_to_date
train_loess$swept_253 <- train_loess$cdr_253 - ((train_loess$adjusted/(train_loess$adjusted/(1+train_loess$lifetime_return_to_date)))^(1/(train_loess$trading_days_to_date + lambda)) - 1) - train_loess$avg_sp500_cdr_to_date
train_loess <- train_loess %>% select(`7-day moving average`,`20-day moving average`,`50-day moving average`,
         `100-day moving average`,`200-day moving average`,`500-day moving average`,swept_21)
train_loess <- gather(train_loess,moving_average,z_score,1:6) 
train_loess <- train_loess %>% mutate(moving_average = factor(moving_average,levels=c("7-day moving average",
                                                         "20-day moving average",
                                                         "50-day moving average",
                                                         "100-day moving average",
                                                         "200-day moving average",
                                                         "500-day moving average"))) %>%
  group_by(moving_average,z_score) %>%
  summarize(n = n(),
            mean_return = mean(swept_21),
            se_return = sd(swept_21)/sqrt(n))

#Plot all moving average zscores on the same chart and run an experimental loess fit
train_loess %>%
  ggplot(aes(x=z_score,y=mean_return)) +
  geom_point() +
  geom_smooth(span=.3) +
  labs(title = "Mean return by z-score distance from various simple moving averages",
       x="Z-score stratum",
       y="Mean return") + 
  scale_y_continuous(limits = c(-.03,.02))

#Fit a loess model
loess_model <- loess(mean_return ~ z_score,data=train_loess,span = .3)

#Predict returns for test data set using loess model
test_loess <- test %>% ungroup()
test_loess$swept_21 <- test_loess$cdr_21 - ((test_loess$adjusted/(test_loess$adjusted/(1+test_loess$lifetime_return_to_date)))^(1/(test_loess$trading_days_to_date + lambda)) - 1) - test_loess$avg_sp500_cdr_to_date
test_loess$swept_253 <- test_loess$cdr_253 - ((test_loess$adjusted/(test_loess$adjusted/(1+test_loess$lifetime_return_to_date)))^(1/(test_loess$trading_days_to_date + lambda)) - 1) - test_loess$avg_sp500_cdr_to_date
test_loess <- test_loess %>%
  mutate(average_z_score = (z_score_7+z_score_20+z_score_50+z_score_100+z_score_200+z_score_500)/6)
test_loess <- test_loess %>% select(swept_21,z_score = average_z_score)
test_loess <- test_loess %>% drop_na()
test_loess$forecast <- predict(loess_model,test_loess)
model_rmses <- bind_rows(model_rmses,data.frame(model = "Loess model",rmse_21_day = RMSE(test_loess$swept_21,test_loess$forecast),rmse_253_day=NA))


#Repeat the above analysis for 253-day return
#sweep out averages and stratify by z-score
train_loess <- train %>%
  ungroup() %>%
  mutate(`7-day moving average` = round(z_score_7),
         `20-day moving average` = round(z_score_20),
         `50-day moving average` = round(z_score_50),
         `100-day moving average` = round(z_score_100),
         `200-day moving average` = round(z_score_200),
         `500-day moving average` = round(z_score_500)) 
train_loess$swept_21 <- train_loess$cdr_21 - ((train_loess$adjusted/(train_loess$adjusted/(1+train_loess$lifetime_return_to_date)))^(1/(train_loess$trading_days_to_date + lambda)) - 1) - train_loess$avg_sp500_cdr_to_date
train_loess$swept_253 <- train_loess$cdr_253 - ((train_loess$adjusted/(train_loess$adjusted/(1+train_loess$lifetime_return_to_date)))^(1/(train_loess$trading_days_to_date + lambda)) - 1) - train_loess$avg_sp500_cdr_to_date
train_loess <- train_loess %>% select(`7-day moving average`,`20-day moving average`,`50-day moving average`,
                                      `100-day moving average`,`200-day moving average`,`500-day moving average`,swept_253) %>%
  filter(!is.na(swept_253))
train_loess <- gather(train_loess,moving_average,z_score,1:6) 
train_loess <- train_loess %>% mutate(moving_average = factor(moving_average,levels=c("7-day moving average",
                                                                                      "20-day moving average",
                                                                                      "50-day moving average",
                                                                                      "100-day moving average",
                                                                                      "200-day moving average",
                                                                                      "500-day moving average"))) %>%
  group_by(moving_average,z_score) %>%
  summarize(n = n(),
            mean_return = mean(swept_253),
            se_return = sd(swept_253)/sqrt(n))

#Plot all moving average zscores on the same chart and run an experimental loess fit
train_loess %>%
  ggplot(aes(x=z_score,y=mean_return)) +
  geom_point() +
  geom_smooth(span=.3) +
  labs(title = "Mean return by z-score distance from various simple moving averages",
       x="Z-score stratum",
       y="Mean return") + 
  scale_y_continuous(limits = c(-.03,.02))

#Fit a loess model
loess_model <- loess(mean_return ~ z_score,data=train_loess,span = .3)

#Predict returns for test data set using loess model
test_loess <- test %>% ungroup()
test_loess$swept_21 <- test_loess$cdr_21 - ((test_loess$adjusted/(test_loess$adjusted/(1+test_loess$lifetime_return_to_date)))^(1/(test_loess$trading_days_to_date + lambda)) - 1) - test_loess$avg_sp500_cdr_to_date
test_loess$swept_253 <- test_loess$cdr_253 - ((test_loess$adjusted/(test_loess$adjusted/(1+test_loess$lifetime_return_to_date)))^(1/(test_loess$trading_days_to_date + lambda)) - 1) - test_loess$avg_sp500_cdr_to_date
test_loess <- test_loess %>%
  mutate(average_z_score = (z_score_7+z_score_20+z_score_50+z_score_100+z_score_200+z_score_500)/6)
test_loess <- test_loess %>% select(swept_253,z_score = average_z_score)
test_loess <- test_loess %>% drop_na()
test_loess$forecast <- predict(loess_model,test_loess)
model_rmses$rmse_253_day[nrow(model_rmses)] <- RMSE(test_loess$swept_253,test_loess$forecast) 
model_rmses %>% 
  knitr::kable(caption="Model RMSEs")


# #Try out different span values for loess modeling and see which minimizes rmse
# spans <- map_dfr(.x=seq(.1,.7,by=.1),.f=function(x){
#   #Fit a loess model
#   loess_model_7 <- loess(mean_return ~ z_score,data=train_loess[moving_average=="7-day moving average"],span = x)
#   loess_model_20 <- loess(mean_return ~ z_score,data=train_loess[moving_average=="20-day moving average"],span = x)
#   loess_model_50 <- loess(mean_return ~ z_score,data=train_loess[moving_average=="50-day moving average"],span = x)
#   loess_model_100 <- loess(mean_return ~ z_score,data=train_loess[moving_average=="100-day moving average"],span = x)
#   loess_model_200 <- loess(mean_return ~ z_score,data=train_loess[moving_average=="200-day moving average"],span = x)
#   loess_model_500 <- loess(mean_return ~ z_score,data=train_loess[moving_average=="500-day moving average"],span = x)
#   
#   #Predict on train set and find which value minimizes rmse
#   train_loess <- train %>% ungroup()
#   train_loess$swept_21 <- train_loess$cdr_21 - ((train_loess$adjusted/(train_loess$adjusted/(1+train_loess$lifetime_return_to_date)))^(1/(train_loess$trading_days_to_date + lambda)) - 1) - train_loess$avg_sp500_cdr_to_date
#   train_loess$forecast <- 
#     train_loess %>%
#     mutate(average_z_score = (z_score_7+z_score_20+z_score_50+z_score_100+z_score_200+z_score_500)/6)
#   train_loess <- train_loess %>% select(swept_21,z_score = average_z_score)
#   train_loess <- train_loess %>% drop_na()
#   train_loess$forecast <- predict(loess_model,train_loess)
#   data.frame(span=x,rmse=RMSE(train_loess$swept_21,train_loess$forecast))
# })


#Validate model
#sweep out averages and stratify by z-score
train_loess <- train %>%
  ungroup() %>%
  mutate(`7-day moving average` = round(z_score_7),
         `20-day moving average` = round(z_score_20),
         `50-day moving average` = round(z_score_50),
         `100-day moving average` = round(z_score_100),
         `200-day moving average` = round(z_score_200),
         `500-day moving average` = round(z_score_500)) 
train_loess$swept_21 <- train_loess$cdr_21 - ((train_loess$adjusted/(train_loess$adjusted/(1+train_loess$lifetime_return_to_date)))^(1/(train_loess$trading_days_to_date + lambda)) - 1) - train_loess$avg_sp500_cdr_to_date
train_loess$swept_253 <- train_loess$cdr_253 - ((train_loess$adjusted/(train_loess$adjusted/(1+train_loess$lifetime_return_to_date)))^(1/(train_loess$trading_days_to_date + lambda)) - 1) - train_loess$avg_sp500_cdr_to_date
train_loess <- train_loess %>% select(`7-day moving average`,`20-day moving average`,`50-day moving average`,
                                      `100-day moving average`,`200-day moving average`,`500-day moving average`,swept_21)
train_loess <- gather(train_loess,moving_average,z_score,1:6) 
train_loess <- train_loess %>% mutate(moving_average = factor(moving_average,levels=c("7-day moving average",
                                                                                      "20-day moving average",
                                                                                      "50-day moving average",
                                                                                      "100-day moving average",
                                                                                      "200-day moving average",
                                                                                      "500-day moving average"))) %>%
  group_by(moving_average,z_score) %>%
  summarize(n = n(),
            mean_return = mean(swept_21),
            se_return = sd(swept_21)/sqrt(n))

#Fit a loess model
loess_model <- loess(mean_return ~ z_score,data=train_loess,span = .3)

#Predict returns for validation data set using loess model
test_loess <- validation %>% ungroup()
test_loess$swept_21 <- test_loess$cdr_21 - ((test_loess$adjusted/(test_loess$adjusted/(1+test_loess$lifetime_return_to_date)))^(1/(test_loess$trading_days_to_date + lambda)) - 1) - test_loess$avg_sp500_cdr_to_date
test_loess$swept_253 <- test_loess$cdr_253 - ((test_loess$adjusted/(test_loess$adjusted/(1+test_loess$lifetime_return_to_date)))^(1/(test_loess$trading_days_to_date + lambda)) - 1) - test_loess$avg_sp500_cdr_to_date
test_loess <- test_loess %>%
  mutate(average_z_score = (z_score_7+z_score_20+z_score_50+z_score_100+z_score_200+z_score_500)/6)
test_loess <- test_loess %>% select(swept_21,z_score = average_z_score)
test_loess <- test_loess %>% drop_na()
test_loess$forecast <- predict(loess_model,test_loess)
model_rmses <- bind_rows(model_rmses,data.frame(model = "Final model validation",rmse_21_day = RMSE(test_loess$swept_21,test_loess$forecast),rmse_253_day=NA))


#Repeat the above analysis for 253-day return
#sweep out averages and stratify by z-score
train_loess <- train %>%
  ungroup() %>%
  mutate(`7-day moving average` = round(z_score_7),
         `20-day moving average` = round(z_score_20),
         `50-day moving average` = round(z_score_50),
         `100-day moving average` = round(z_score_100),
         `200-day moving average` = round(z_score_200),
         `500-day moving average` = round(z_score_500)) 
train_loess$swept_21 <- train_loess$cdr_21 - ((train_loess$adjusted/(train_loess$adjusted/(1+train_loess$lifetime_return_to_date)))^(1/(train_loess$trading_days_to_date + lambda)) - 1) - train_loess$avg_sp500_cdr_to_date
train_loess$swept_253 <- train_loess$cdr_253 - ((train_loess$adjusted/(train_loess$adjusted/(1+train_loess$lifetime_return_to_date)))^(1/(train_loess$trading_days_to_date + lambda)) - 1) - train_loess$avg_sp500_cdr_to_date
train_loess <- train_loess %>% select(`7-day moving average`,`20-day moving average`,`50-day moving average`,
                                      `100-day moving average`,`200-day moving average`,`500-day moving average`,swept_253) %>%
  filter(!is.na(swept_253))
train_loess <- gather(train_loess,moving_average,z_score,1:6) 
train_loess <- train_loess %>% mutate(moving_average = factor(moving_average,levels=c("7-day moving average",
                                                                                      "20-day moving average",
                                                                                      "50-day moving average",
                                                                                      "100-day moving average",
                                                                                      "200-day moving average",
                                                                                      "500-day moving average"))) %>%
  group_by(moving_average,z_score) %>%
  summarize(n = n(),
            mean_return = mean(swept_253),
            se_return = sd(swept_253)/sqrt(n))

#Fit a loess model
loess_model <- loess(mean_return ~ z_score,data=train_loess,span = .3)

#Predict returns for validation data set using loess model
test_loess <- validation %>% ungroup()
test_loess$swept_21 <- test_loess$cdr_21 - ((test_loess$adjusted/(test_loess$adjusted/(1+test_loess$lifetime_return_to_date)))^(1/(test_loess$trading_days_to_date + lambda)) - 1) - test_loess$avg_sp500_cdr_to_date
test_loess$swept_253 <- test_loess$cdr_253 - ((test_loess$adjusted/(test_loess$adjusted/(1+test_loess$lifetime_return_to_date)))^(1/(test_loess$trading_days_to_date + lambda)) - 1) - test_loess$avg_sp500_cdr_to_date
test_loess <- test_loess %>%
  mutate(average_z_score = (z_score_7+z_score_20+z_score_50+z_score_100+z_score_200+z_score_500)/6)
test_loess <- test_loess %>% select(swept_253,z_score = average_z_score)
test_loess <- test_loess %>% drop_na()
test_loess$forecast <- predict(loess_model,test_loess)
model_rmses$rmse_253_day[nrow(model_rmses)] <- RMSE(test_loess$swept_253,test_loess$forecast) 
model_rmses %>% 
  knitr::kable(caption="Model RMSEs")