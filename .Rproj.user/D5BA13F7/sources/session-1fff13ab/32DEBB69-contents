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

# Working with messy data

co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))

co2_emissions_dirty%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))%>%pivot_wider(names_from=series, values_from=value)

# filtering rows and removing columns

co2_emissions_dirty_2005 <- co2_emissions_dirty%>%filter(year == 2005)%>%
  select(-year)

co2_emissions <- co2_emissions_dirty%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))%>%pivot_wider(names_from=series, values_from=value)%>%filter(year == 2005)%>%
  select(-year)

co2_emissions

# bringing in 2007 population data

gapminder_data_2007 <- read_csv("data/gapminder_data.csv")%>%filter(year == 2007)%>%
  select(country, pop, lifeExp, gdpPercap)

joined_co2_pop<-inner_join(gapminder_data_2007, co2_emissions, by = "country")

anti_join(co2_emissions, gapminder_data_2007, by = "country")

anti_join(gapminder_data_2007, co2_emissions, by = "country")

full_join(co2_emissions, gapminder_data_2007)%>%View()

co2_emissions%>%
  right_join(gapminder_data_2007)

# write object to a csv

write_csv(joined_co2_pop, file = "data/joined_co2_pop.csv")

