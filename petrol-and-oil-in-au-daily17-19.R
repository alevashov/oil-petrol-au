library(lubridate)
library(tidyverse)
library(ggthemes)
library(quantmod)
library(tsibble)


# get crued oil prices in USD per barrel and AUD to USD rate, source is FRED  - https://fred.stlouisfed.org 
# clean up dates and recalculating price to AUD
#augmented by manually added data for 2 last moths

getSymbols("DCOILWTICO", src = "FRED")
getSymbols("DEXUSAL", src = "FRED")





# cleaning up and formatting FX rates
fx <- as.data.frame(DEXUSAL)

fx$date <- row.names(fx)
fx$date <- as.Date(fx$date)
row.names(fx)<- NULL
fx <- select(fx, date, DEXUSAL)

# filling gaps - dates where FX rate is mised, take previous date
fx <- as_tsibble(fx, key=id(), index=date) %>% fill_gaps(DEXUSAL=NA) %>% tidyr::fill(DEXUSAL, .direction = "down")

# cleaning up and formatting crude oil prices
oil <- as.data.frame(DCOILWTICO)
oil$date <- row.names(oil)
oil$date <- as.Date(oil$date)
oil <- select(oil, date, DCOILWTICO)
# filling gaps - dates where oild priceis mised, take previous date

oil <- as_tsibble(oil, key=id(), index=date) %>% fill_gaps(DCOILWTICO=NA) %>% tidyr::fill(DCOILWTICO, .direction = "down")

# joing data

df <- inner_join(oil, fx, by=c("date"))

names(df) <- c("date", "oil", "fx")


#convert to aud
df$oilaud <- df$oil/df$fx

# get rid of data we don't need 

df <- select (df,c("date", "oilaud"))  



        

# petrol retail prices, sourced from Dave Allie, https://medium.com/@daveallie/stealing-fuel-data-303f3327434c

petrol <- read_csv("Fuel-Prices-Dave.csv")
names(petrol) <- c("date", "petrol_price")
petrol$date <- as.Date(petrol$date)

df1 <- inner_join(petrol, df, by=c("date"))
write.csv(df1, file="petrol-oil.csv")

#### crude oil in AUD chart

df1 %>% ggplot(aes(date, oilaud)) +
        geom_line()+
        xlab("Date")+ylab ("Price per barrel, $AUD")+
        ggtitle("Crude oil price")+
        theme_economist()

# petrol price chart
df1 %>% ggplot(aes(date, petrol_price)) +
        geom_line()+
        xlab("Date")+ylab ("Price, cents per litre")+
        ggtitle("Retail petrol price")+
        theme_economist()



# petrol vs crude oil chart
df1 %>% ggplot(aes(date))+
        geom_line(aes(y=oilaud, colour= "Oil"))+
        geom_line(aes(y=petrol_price, colour="Petrol"))+
        scale_y_log10()+
        xlab("Date")+ylab ("Prices")+
        ggtitle("Retail petrol price vs crude oil price")+
        theme_economist()



# petrol vs crude oil scatterplot

df1 %>% ggplot(aes(oilaud, petrol_price, color=factor(year(date)))) +
        geom_point(size=3)+
        labs(title = "Petrol vs crude oil\n", x = "Crude oil, $ per barrel", y = "Petrol, cents per liter", color = "Colors for years\n")+
        geom_hline(yintercept = 158.89)+
        geom_vline(xintercept = 88.92)+
        theme_economist()

# correlation calculation

cor(df1$petrol_price, df1$oilaud)

