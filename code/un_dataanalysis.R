library(tidyverse)
getwd()
gapminder_data <- read_csv("data/gapminder_data.csv")

summarize(gapminder_data, averageLifeExp=mean(lifeExp), medianLifeExp=median(lifeExp))

# learning to pipe

gapminder_summary <- gapminder_data %>% summarize(averageLifeExp=mean(lifeExp))
gapminder_summary

# filtering

gapminder_summary_2007<-gapminder_data%>%filter(year == 2007)%>%summarize(average=mean(lifeExp))

# filtering the dataset
gapminder_data%>%summarize(Firstyear=min(year))
gapminder_data%>%filter(year == 1952)%>%summarize(averageGDPpercapita=mean(gdpPercap))

# Grouping rows using group_by()

gapminder_data%>%
  group_by(year, continent)%>%
  summarize(average=mean(lifeExp), std_dev = sd(lifeExp))

# mutate function (adds a new column onto data frame)

gapminder_data%>%
  mutate(gdp = pop * gdpPercap)

# mutate a new column to make pop in millions

gapminder_data%>%
  mutate(population_in_millions = pop / 1000000)

gapminder_data %>%  
  mutate(gdp = pop * gdpPercap, popInMillions = pop / 1000000)  

# select function

gapminder_data%>%
  select(pop, year)

gapminder_data%>%
  select(-continent, -pop)

# changing shape of data
#pivot_wider

gapminder_data%>%
  select(country, continent, year, lifeExp)%>%
  pivot_wider(names_from = year, values_from = lifeExp)
