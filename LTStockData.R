##nse2r is a package to get information of NSE Stock market to R. 
##we can get stock price , dayHigh, dayLow of stocks from this package
##Here We got a file from NSE for last three months data


install.packages("nse2r")
library(nse2r)

nse_app()

nse_stock_most_traded()
nse_index_quote()
nse_stock_year_high()
nse_index_list()
str(nse_stock_quote("LT"))
nse_stoc

nse_stock_year_high("SUZLON")

print(nse_stock_quote("LT")$dayHigh)

.libPaths()


?nse2r


plot(df$Date,df$High.Price)


setwd("C://Users//pjawaria//Desktop//NSE Stock Research")

getwd()

df <- read.csv("LT Apr-May-Jun.csv")
df

# Plot to see the day high and day low trend in stock market for LT Data
plot(x=df$Date,y=df$High.Price,xlab="Trading Date",ylab="Price")
lines(df$High.Price,type = "o", col="green")
lines(df$Low.Price, type = "o", col= "blue")

