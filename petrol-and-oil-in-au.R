library(lubridate)
library(dplyr)
library(ggplot2)
library(ggthemes)


# get crued oil prices in USD per barrel and AUD to USD rate, source is FRED  - https://fred.stlouisfed.org 
# clean up dates and recalculating price to AUD
#augmented by manually added data for 2 last moths
oil <- read.csv("MCOILBRENTEU.csv")




fx <- read.csv("AUDUSD.csv")

oil <- oil %>% mutate(my=paste0(year(oil$DATE), "-", month(oil$DATE),"-", "01" )) 
fx <- fx %>% mutate(my=paste0(year(fx$DATE), "-", month(fx$DATE), "-", "01" )) %>% 
        filter(as.Date(DATE) >= as.Date("1987-05-01"))


df <- inner_join(oil, fx, by=c("my"))

names(df) <- c("date.oil", "oil", "my", "date.fx", "fx")


#convert to aud
df <- df %>% mutate (oilaud = oil/fx)

# get rid of data we don't need 
df$date <- as.Date(df$my)
df <- select (df,c("date", "oilaud"))  



        

# petrol retail prices

petrol <- read_csv("FuelPrices_ULP_Metro_WA.csv")
petrol <- select(petrol, c("Month", "Average"))

petrol <- petrol %>% mutate(my=paste0("01", "-", Month)) %>%
        mutate(date = as.Date(parse_date_time(my, "dmy"))) %>%
        select (c("date", "Average"))
        
names(petrol) <- c("date", "petrol_price")

df1 <- inner_join(df, petrol, by=c("date"))
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
        theme_economist()

# correlation calculation

cor(df1$petrol_price, df1$oilaud)



# petrol vs crude oil chart since 2015
df1 %>% filter(date >= as.Date("2015-01-01")) %>%
        ggplot(aes(date))+
        geom_line(aes(y=oilaud, colour= "Oil"))+
        geom_line(aes(y=petrol_price, colour="Petrol"))+
        xlab("Date")+ylab ("Prices")+
        ggtitle("Retail petrol price vs crude oil price")+
        theme_economist()

over90 <- df1 %>% filter(oilaud >90)

summary(over90)

## plot over90

over90 %>%  ggplot(aes(oilaud, petrol_price, color=factor(year(date)))) +
        geom_point(size=3)+
        labs(title = "Petrol vs crude oil\n", x = "Crude oil, $ per barrel", y = "Petrol, cents per liter", color = "Colors for years\n")+        theme_economist()
        

# plot over 100
over100 <- df1 %>% filter(oilaud >100)
over100 %>%  ggplot(aes(oilaud, petrol_price, color=factor(year(date)))) +
        geom_point(size=3)+
        labs(title = "Petrol retail in Australia vs crude oil, when crude oil over AU$100\n", x = "Crude oil, $ per barrel", y = "Petrol, cents per liter", color = "Colors for years\n")+
        geom_hline(yintercept = 157.3)+
        geom_vline(xintercept = 104.29)+
        theme_economist()

