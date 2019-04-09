#### - Load the libraries - ####

require(tidyverse)
require(broom)
require(scales)

#### - Set the plot theme to light - ####

theme_set(theme_light())

#### - Load the opex csv file - ####

data <- read.csv(file = "opex.csv",
                 header = TRUE,
                 sep = ";",
                 dec = ",",
                 fill = TRUE,
                 stringsAsFactors = TRUE)

data$ACCOUNT <- as.factor(data$ACCOUNT)

#### - Project plot - ####

data %>%
  filter(PROJECT != "NEXT EINSTEIN FORUM") %>% 
  group_by(FUNCTION, PROJECT) %>% 
  summarise(USD = sum(YEAR_2016)) %>% 
  ggplot() + 
  aes(x = fct_reorder(PROJECT, USD, sum), y = USD, fill = FUNCTION) +
  geom_col() + 
  coord_flip() + 
  scale_y_continuous(labels = dollar_format()) + 
  labs( x = "PROJECT",
        y = "USD AMOUNTS") + 
  expand_limits( y = 0)

####- Function plot - ####

data %>%
  filter(PROJECT != "NEXT EINSTEIN FORUM") %>% 
  ggplot() + 
  aes(x = fct_reorder(FUNCTION, YEAR_2016, sum), y = YEAR_2016,col = PROJECT) + 
  geom_point() + 
  geom_jitter(height = .25, width = .15) +
  coord_flip() + 
  scale_y_continuous(labels = dollar_format()) + 
  labs( x = "PROJECT",
        y = "USD AMOUNTS") +
  theme(legend.position = "none")
