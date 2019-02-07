library(tidyr)  #for data manipulation
library(dplyr)  #for data manipulation
library(ggplot2)#for the plots
library(scales) #helps us do some rounding later

# functions
# write function to strip commas from input and then convert to numeric 
conversion <- function(x){
  as.numeric(gsub(",","", x)) 
}
round_means_up <- function(x){
  ceiling(mean(x))
}

# read the table I made
data <- read.csv("sa-data.csv")

# clean only the data you need cleaned
data[,2:11] <- lapply(data[,2:11], conversion)


data <- data %>%
  gather("quarter", "population", 2:11) %>%
  separate(quarter, into=c('x','y', 'year'), sep = "\\.") %>%
  group_by(region, year) %>%
  summarise(population = round_means_up(population))

g <- ggplot(data, aes(x = year, y = population, fill=region)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Year") + ylab("Working Population") + 
  ggtitle("Total Workforce in Provinces, 2013-2015") +
  scale_y_continuous(breaks = seq(0, max(data$population), 10000000 ), labels = comma) +
  coord_flip()

g