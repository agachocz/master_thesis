
# Wczytanie danych
data <- readRDS("data.rds")

str(data[,351:440])
unique(data$V1)

# Wybrane zmienne
# V1 - Wave (czy jest tylko jedna?)
# V2 - 

library(dplyr)

dem_data <- data %>% select(wave = V1, country = V2, country_split = V2A,
                    leader = V127, experts = V128, army = V129, democracy = V130)

# Flagi w pliku questionare
unique(dem_data$country_split)
unique(dem_data$country)


