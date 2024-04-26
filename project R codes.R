libraries <- c("ggplot2","sf","rworldmap","tidyverse","magrittr",
               "leaflet", "dplyr", "rvest", "xml2","rvest",
               "maps","mapdata","RgoogleMaps","lubridate","rnaturalearth","dplyr","rnaturalearthdata","RColorBrewer","httr")
lapply(libraries, require, character.only = TRUE)


url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv"

chocolate <- read.csv(url)

head(chocolate, 6)
str(chocolate)

library(dplyr)

#ii
chocolate <- chocolate %>%
  mutate(across(where(is.character), as.factor))
str(chocolate)

#iii

smmry <- chocolate %>%
  group_by(company_location) %>%
  summarise(
    mean_rating = mean(rating, na.rm = TRUE),
    sd_rating = sd(rating, na.rm = TRUE),
    median_rating = median(rating, na.rm = TRUE),
    range_rating = max(rating, na.rm = TRUE) - min(rating, na.rm = TRUE)
  )
head(smmry, 10)

#iv

filtered <- filter(chocolate, review_date == 2020 & country_of_bean_origin == "Colombia")
filtered

#v

mean_loc <- chocolate %>%
  group_by(company_location) %>%
  summarise(
    mean_rating = mean(rating, na.rm = T),
    mean_cocoa_percentage = mean(as.numeric(gsub("%", "", cocoa_percent)), na.rm = T)
  )

print(mean_loc, n = 15)

#vi

selection <- chocolate[, c("company_manufacturer", "company_location", "country_of_bean_origin")]

head(selection, 10)

#vii

filtered2 <- chocolate[chocolate$company_location == "Switzerland" & chocolate$rating >= 3.25 & 
                         chocolate$rating <= 3.5, ]
filtered2 <- head(filtered2, 5)

filtered2

#viii

mean_rating_location <- chocolate %>%
  group_by(company_location) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE)) %>%
  arrange(desc(mean_rating))
mean_rating_location

#ix

count_bonnat <- chocolate %>%
  filter(company_manufacturer == "Bonnat") %>%
  group_by(country_of_bean_origin) %>%
  count()
count_bonnat

#x

chocolate_new <- chocolate %>%
  mutate(Rating_Percentage = rating * 25)

chocolate_new <- chocolate_new %>%
  mutate(Class = case_when(
    Rating_Percentage < 25 ~ "Low",
    Rating_Percentage >= 25 & Rating_Percentage < 50 ~ "Medium",
    Rating_Percentage >= 50 & Rating_Percentage < 75 ~ "Tasty",
    Rating_Percentage >= 75 ~ "Excellent",
    TRUE ~ "Unknown"
  ))
head(chocolate_new)



#Question 3

nmmaps <- read.csv("https://www.cedricscherer.com/data/chicago-nmmaps-custom.csv")
str(nmmaps)

#i
library(ggplot2)

# Ensure that year is a factor variable
nmmaps$year <- as.factor(nmmaps$year)

# Create a scatter plot with facet_wrap()
ggplot(nmmaps, aes(x = date, y = temp, color = season)) +
  geom_point() +
  facet_wrap(~year) +
  labs(x = "Date", y = "Temperature", color = "Season") +
  theme_minimal()


#ii

library(ggplot2)
nmmaps$season <- factor(nmmaps$season, levels = c("Winter", "Spring", "Summer", "Fall"))

ggplot(nmmaps, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Date", y = "Temperature", color = "Season") +
  theme_minimal()

#iii

cor(nmmaps$temp, nmmaps$dewpoint)


ggplot(nmmaps, aes(x = temp, y = dewpoint)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Temperature", y = "Dewpoint") +
  theme_minimal()

#Question 4




my_url <- "https://www.latlong.net/category/cathedrals-209-54.html"

ne <- GET(my_url)

html_com <- content(ne, "text")
html_cathedral <- read_html(html_com)
my_tables <- html_cathedral |>
  html_nodes("table") |> 
  html_table()



cathedrals <- do.call(rbind, my_tables)
cathedrals <- as.data.frame(cathedrals)

str(cathedrals)


my_map <- leaflet(data = cathedrals) %>%
  addTiles() %>%
  setView(lng = -3.70359, lat = 40.41677, zoom = 6)


my_map <- my_map %>%
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                   radius = 5, color = "red",
                   popup = ~ cathedrals$`Place Name` )

my_map








