######sakshi mahesh_CS_555
####Avacados effect on US Economy
####final project
library(tidyverse)
library(tibble)
library(tibbletime)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(scales)

install.packages("tibbletime")
suppressPackageStartupMessages(library(tibbletime))

df <- read.csv("C:/selfstudy_R_CS/avocado.csv", nrows = 1000)

original_df <- df

#Avacado price by type
#1 is for small hass
#2 for large hass
#3 for extra large hass
#the x axis shows the type of avacados 1, 2 and 3 of its average price
options(repr.plot.width=15, repr.plot.height=10)
ggplot(df, aes(x=AveragePrice, fill=type)) + geom_density() + facet_wrap(~type) + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), legend.position="bottom") + labs(title="Avocado Price ") + scale_fill_brewer(palette="Set1")
#overview of type of avacados
vol_type <- df %>% group_by(type) %>% summarise(avg.vol=mean(Total.Volume))  %>% mutate(pct=prop.table(avg.vol) * 100) 
vol_type

#####3Types of Avacado 

# Change the date column from factor to date
df$Date <- as.Date(df$Date, "%Y-%m-%d")
class(df$Date)

# Sort the dates
df <- df[order(as.Date(df$Date, format="%Y-%m-%d")),]


price_trend <- df %>% select(Date, AveragePrice, type) %>%
  ggplot(aes(x=Date, y=AveragePrice)) + geom_area(aes(color=type, fill=type), alpha = 0.3, position = position_dodge(0.8)) + 
  theme_minimal() +  scale_color_manual(values = c("#ED7921", "#62BE51")) + scale_fill_manual(values = c("#FD833E", "#B8FC5F"))

price_trend

print(lm(df$AveragePrice ~ df$Total.Volume))
print(summary(lm(df$AveragePrice ~ df$Total.Volume)))
options(scipen=10000)

plot(df$AveragePrice~df$Total.Volume , col="blue", main="regressionmodel",
abline(lm(df$Total.Volume ~df$AveragePrice)), pch=10, xlab="total avocados", ylab="average price of avacados") 
#scale_x_continuous(labels = label_comma()))

#facet graphs ie showing indivitual category of the type of avacado
ggplot(data = df, aes(x = Date, y = AveragePrice, col=type)) +
  geom_line() +
  facet_wrap(~ type) + theme_minimal() + theme(legend.position="bottom")

# Filter by type
organic <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type == "organic")
conventional <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type == "conventional")
organic <- as_tbl_time(organic, index=Date)
organic <- as_period(organic, '1 month')


# Conventional Avocados
conventional <- as_tbl_time(conventional, index=Date)

conventional <- as_period(conventional, '1 month')
head(conventional)

#######Correlation between prices and total volume
options(repr.plot.width=6, repr.plot.height=8)
conventional_monthly <- conventional %>%
  ggplot(aes(x=Date, y=AveragePrice)) + geom_line(color="#58D68D") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#D5D8DC")) + 
  labs(title="Conventional type") + geom_hline(yintercept=max(conventional$AveragePrice), linetype="dashed", color = "yellow") + 
  geom_hline(yintercept=min(conventional$AveragePrice), linetype="dashed", color = "grey")

# creating a volume chart to analyze volume and averge price
conventional_volume <- conventional %>%
  ggplot(aes(x=Date, y=Total.Volume)) + geom_bar(stat='identity', fill="#58D68D", color="black") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#D5D8DC")) + 
  geom_smooth(method="loess", color="red")

organic_monthly <- organic %>% 
  ggplot(aes(x=Date, y=AveragePrice)) + geom_line(color="#58D68D") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#D5D8DC")) + 
  labs(title="Organic type") + geom_hline(yintercept=max(organic$AveragePrice), linetype="dashed", color = "black") + 
  geom_hline(yintercept=min(organic$AveragePrice), linetype="dashed", color = "yellow")

organic_volume <- organic %>%
  ggplot(aes(x=Date, y=Total.Volume)) + geom_bar(stat='identity', fill="#58D68D",color="black") + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#D5D8DC")) + geom_smooth(method="loess", color="red")

plot_grid(conventional_monthly, organic_monthly,conventional_volume, organic_volume, nrow=2, ncol=2)

install.packages("cowplot")

####analyzing seasonal patterns

seasonal_df <- original_df
seasonal_df$month_year <- format(as.Date(original_df$Date), "%Y-%m")
seasonal_df$month <- format(as.Date(original_df$Date), "%m")
seasonal_df$year <- format(as.Date(original_df$Date), "%Y")
seasonal_df$monthabb <- sapply(seasonal_df$month, function(x) month.abb[as.numeric(x)])
seasonal_df$monthabb = factor(seasonal_df$monthabb, levels = month.abb)


# # seasonal patterns with conventional avocados
ggplot(seasonal_df, aes(x = AveragePrice, fill = as.factor(year))) + 
  geom_density(alpha = .5) + 
  theme_economist() +
  facet_wrap(~ year) + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F9E79F")) + 
  guides(fill = FALSE) + labs(title="Distribution of Prices by year", x = 'Average Price', y = 'Density') + 
  scale_fill_manual(values=c("#2E64FE", "#40FF00", "#FE642E", "#FE2E2E"))


#### Finding seasonality patterns
conv_patterns <- seasonal_df %>% select(monthabb, AveragePrice, type) %>% filter(type == "conventional") %>%
  group_by(monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#F35D5D", aes(size=avg)) + geom_line(group=1, color="#7FB3D5") + 
  theme_economist() + theme(legend.position="none", plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#7FB3D5")) + 
  labs(title="Conventional Avocados", x="Month", y="Average Price")


org_patterns <- seasonal_df %>% select(monthabb, AveragePrice, type) %>% filter(type == "organic") %>%
  group_by(monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#F35D5D", aes(size=avg)) + geom_line(group=1, color="#58D68D") + 
  theme_economist() + theme(legend.position="none", plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#7FB3D5")) + 
  labs(title="Organic Avocados", x="Month", y="Average Price")

plot_grid(conv_patterns, org_patterns, nrow=2)


#### detailed avg price by years and months
options(repr.plot.width=8, repr.plot.height=6) 
conv_pat_yearly <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#58D68D") + geom_line(group=1, color="#E74C3C") + facet_wrap(~as.factor(year)) + 
  theme_minimal() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"), axis.text.x = element_text(angle = 90)) + 
  labs(title="Seasonal Fluctuations \n Convenctional Avocados", x="Month", y="Average Price")

org_pat_yearly <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#58D68D") + geom_line(group=1, color="#E74C3C") + facet_wrap(~as.factor(year)) + 
  theme_minimal() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"), axis.text.x = element_text(angle = 90)) + 
  labs(title="Seasonal Fluctuations \n Organic Avocados", x="Month", y="Average Price")

plot_grid(conv_pat_yearly, org_pat_yearly, nrow=2)


####Market volume 2015-2017
##not enough dataset for the year 2018
head(seasonal_df)
volume_conv <- seasonal_df %>% select(year, monthabb, Total.Volume, region, type) %>% 
  filter(type == "conventional", year == c("2015", "2016", "2017"), 
         region == c("Northeast", "SouthCentral", "MidSouth", "Southeast", "West")) %>% 
  group_by(year, monthabb, region) %>% summarize(avg.vol=mean(Total.Volume)) %>%
  ggplot(aes(x=monthabb, y=avg.vol)) + geom_point(color="#5D6D7E") + geom_line(group=1, color="#FE642E") +
  facet_grid(region ~ year)+ 
  theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#58D68D"), axis.text.x = element_text(angle = 90)) + 
  labs(title="Market Volume \n Convenctional Avocados", x="Month", y="Average Volume")

volume_conv

volume_org <- seasonal_df %>% select(year, monthabb, Total.Volume, region, type) %>% 
  filter(type == "organic", year == c("2015", "2016", "2017"), 
         region == c("Northeast", "SouthCentral", "MidSouth", "Southeast", "West")) %>% 
  group_by(year, monthabb, region) %>% summarize(avg.vol=mean(Total.Volume)) %>%
  ggplot(aes(x=monthabb, y=avg.vol)) + geom_point(color="#5D6D7E") + geom_line(group=1, color="#819FF7") +
  facet_grid(region ~ year)+ 
  theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#58D68D"), axis.text.x = element_text(angle = 90)) + 
  labs(title="Market Volume \n Organic Avocados", x="Month", y="Average Volume")

volume_org

######thank you#######
