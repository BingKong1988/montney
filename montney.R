library(dplyr)
library(ggplot2)
library(lubridate)
rawdata <- read.csv("Frac_Export.csv")
clean <- filter(rawdata, Completed.Zone == "Montney")
clean$Completed.Zone <- droplevels(clean$Completed.Zone)
clean[[3]] <- as.Date(as.character(clean[[3]]),"%m/%d/%Y")
clean <- mutate(clean, 
                year = format(clean[[3]], "%Y"),
                yearmonth = format(clean[[3]], "%Y%m"))


tgt <- filter(clean, year > 2000)
ggplot(tgt, aes(factor(tgt$yearmonth))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5)) +
      scale_x_discrete(breaks=function(x) x[as.numeric(x) %% 20 == 1]) +
      geom_bar() +
      labs(x = "Time(YearMonth)",
           y = "Count of new completed well of a month",
           title = "Number of monthly completed well in Montney formation")
 
wti <- read.csv("DCOILWTICO.csv", na.strings = ".")
brent <- read.csv("DCOILBRENTEU.csv", na.strings = ".")
wti[[1]] <- as.Date(wti[[1]], "%Y-%m-%d")
brent[[1]] <- as.Date(brent[[1]], "%Y-%m-%d")
wti <- wti[!is.na(wti[[2]]), ]
brent <- brent[!is.na(brent[[2]]), ]
oilprice <- merge(wti, brent, by.x = "DATE", by.y = "DATE", all.x = T)
names(oilprice) <- c("date", "wti", "brent")


g <- ggplot(oilprice[oilprice$date > "2000-01-01",], aes(x = date))
g + 
      geom_line(aes(y = wti), color = "green", alpha = 0.3) +
      geom_line(aes(y = brent), color = "red", alpha = 0.3) +
      labs(x = "Time(Year)",
           y = "Oil Price(USD)") +
      scale_y_continuous(sec.axis = sec_axis(~.+5, name = "Relative humidity [%]"))

wellcount <- as.data.frame(table(clean$yearmonth))
names(wellcount) <- c("date", "count")
wellcount$date <- as.Date(paste(wellcount$date, "01", sep = ""), "%Y%m%d")
count.n.price <- merge(wellcount, oilprice, by.x = "date", by.y = "date", all.x = T)
ggplot(data = count.n.price[count.n.price$date > "2000-01-01",]) +
      geom_bar(mapping = aes(x = date, y = count), stat = "identity") +
      geom_line(data = oilprice[oilprice$date > "2000-01-01",], 
                mapping = aes(x = date, y = wti * 0.8, colour = "WTI"), 
                alpha = 0.6, na.rm = T) +
      geom_line(data = oilprice[oilprice$date > "2000-01-01",], 
                mapping = aes(x = date, y = brent * 0.8, colour = "BRENT"), 
                alpha = 0.6, na.rm = T) +
      scale_color_manual("",
                         breaks = c("WTI", "BRENT"),
                         values = c("red", "blue")) +
      scale_y_continuous(breaks = seq(0, 120, 20), 
                         labels = seq(0, 120, 20),
                         sec.axis = sec_axis(~./0.8, 
                                             name = "Crude oil price/USD", 
                                             breaks = seq(0,150,25), 
                                             labels = seq(0,150,25))) +
      scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      labs(x = "Time/Year",
           y = "Count of monthly completed wells",
           title = "Number of monthly completed well in Montney formation") +
      theme(plot.title = element_text(hjust = 0.5, face="bold", size=16, 
                                      color = "grey58", family = "serif"),
            axis.title = element_text(hjust = 0.5, face="bold", size=12, 
                                      color = "grey58", family = "serif"),
            axis.text = element_text(hjust = 0.5, size=8, 
                                     color = "grey28", family = "serif"),
            legend.text = element_text(hjust = 0.5, face="bold", size=8, 
                                       color = "grey58", family = "serif"))











