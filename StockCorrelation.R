
##########################################################################
# This script uses the tidyquant libaray to get historical stock prices
# and returns a correlation plot and matrix for each ticker symbol pair.
##########################################################################

##### 1. install and attach required packages #####
#install.packages("tidyquant") # run this line if the tidyquant package has not been installed

library(tidyquant) # attach the tidyquant library to the the session

##### 2. set variables #####
#set begin and end dates; to set a static date, use as.Date("2016-10-01")
end <- Sys.Date() # setting to current date 
start <- ymd(end) - years(2) # setting to current date minus 2 years

# set symbols to be included in analysis
tickers <- c("IWM", "QQQ", "DIA", "SPY", "GDX")

##### 3. data prep and correlation #####
#create single dataframe with rows for all tickers
df_tickers <- tickers %>%
  tq_get(get  = "stock.prices",
         from = start,
         to   = end)

#create empty dataframe to store close values for each ticker
data <- data.frame(date=as.Date("01/01/2000", format="%m/%d/%Y"), 
                 File="", User="", stringsAsFactors=FALSE)

data <- data[-1,] #delete the placeholder row to empty the dataframe

# loop over all tickers and merge to create single dataframe for analysis
for (i in tickers) {
  #create/reload working dataframe to be merged with analysis dataframe
  work_data <- subset(df_tickers, symbol==i,
                      select = c(date, close))
  names(work_data) <- c("date", i)
  
  #merge working dataframe with analysis dataframe
  data <- merge(x = data, y = work_data, by = "date", all = TRUE)
  }
# end of the for loop

#clean up analysis dataframe and drop working dataframe
data <- data[ , -which(names(data) %in% c("File","User"))]
cordata <- data[ , -which(names(data) %in% c("date"))] #remove date column for correlation analysis
rm(i)
rm(work_data)
rm(df_tickers)
rm(data)

## correlation analysis ##

pairs(cordata) # scatter plot matrix for each combination of variables

cor(cordata) # matrix for each combination of variables



#### 4. other functions that help with correlation analysis#####

# manual test for specific pairs that look interesting
cor.test(cordata$IWM, cordata$GDX)

# remove objects from workspace
rm(list=ls())
