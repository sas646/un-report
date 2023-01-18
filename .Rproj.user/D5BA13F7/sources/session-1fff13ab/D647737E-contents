# Analyze life expectancy and CO2 emissions vs population with gapminder data
# Date: Jan 17th, 2023
# Author: Samantha Scott

# load in packages necessary for analysis
library("tidyverse")
library("readr")

# Read in data for analysis
gapminder_1997 <- read_csv("gapminder_1997.csv")

# Plotting data for visualization

ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap) + 
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy (yrs)") +
  geom_point() +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) +
  scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000) +
  labs(size = "Population(in millions)") +
  aes(shape = continent)

# Shorthand ggplot
ggplot(data = gapminder_1997,
       aes(x = gdpPercap, y = lifeExp, color = continent, 
           shape = continent, size = pop)) +
  labs(x = "GDP Per Capita", y = "Life Expectency", 
       title = "Do people in wealthy countries live longer?", 
       size = "Population (in milions)") +
  geom_point()

# Read in all of the data from gapminder (more years than 1997)
gapminder_data <- read_csv("gapminder_data.csv")
view(gapminder_data)
dim(gapminder_data)
head(gapminder_data)
tail(gapminder_data)

# Challenge: Predicting the output
ggplot(data = gapminder_data) +
  aes(x=year, y=lifeExp, color=continent, group = country) +
  geom_line()

# learn about data
str(gapminder_data)

# Make a boxplot with x = continent and y = lifespan
ggplot(data = gapminder_1997,
       aes(x = continent, y = lifeExp)) +
  labs(x = "GDP Per Capita", y = "Life Expectency", 
       title = "Do people in wealthy countries live longer?") +
  geom_boxplot()

# Make a violin plot with x = continent and y = lifespan
ggplot(data = gapminder_1997,
       aes(x = continent, y = lifeExp, color = continent)) +
  labs(x = "GDP Per Capita", y = "Life Expectency", 
       title = "Do people in wealthy countries live longer?") +
  geom_violin() +
  geom_jitter(aes(size = pop))

# histogram
ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_classic()

# install and load in ggprism library
install.packages("ggprism")
library("ggprism")

# histogram - with prism theme
ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_prism()

# make multiple plots at one time using facet_wrap or facet_grid
ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(rows = vars(continent))

# save plot
ggsave("awesome_plot.jpg", width = 6, height = 4)

# save plot as tiff
ggsave("awesome_plot.tiff", device = "tiff", width = 6, height = 4)
