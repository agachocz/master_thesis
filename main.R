# Pakiety
install.packages("dplyr")
library(dplyr)
install.packages("lavaan")
library(lavaan)
install.packages("semTools")
library(semTools)
install.packages("semPlot")
library(semPlot)

# Wczytanie danych
data <- readRDS("data.rds")


# Kody krajów UE
country_nr <- c(40, 56, 100, 192, 203, 208, 233, 246, 350, 276, 300, 348, 380, 428, 440, 442, 372, 528, 616,
                620, 703, 705, 724, 756, 826, 470, 642)
country_name <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland",
                  "France", "Germany", "Greece", "Hungary", "Italy", "Latvia", "Lithuania", "Luxembourg",
                  "Ireland", "Netherlands", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain",
                  "Switzerland", "Great Britain", "Malta", "Romania")

countries <- cbind(country_nr, country_name)

# Wybrane zmienne
# V2 - Country (nr)
# V3 - Country_split - raczej nie ma znaczenia, skoro analiza nie jest przeprowadzana na przestrzeni czasu
# V127 - leader - silny lider powinien rządzić
# V128 - experts - Rządy ekspertów
# V129 - army - Arma powinna rządzić
# V130 - democracy - Rządy demokratyczne 


dem_data <- data %>% select(wave = V1, country = V2, country_split = V2A,
                    leader = V127, experts = V128, army = V129, democracy = V130) %>%
            filter(country %in% country_nr)

# Flagi w pliku questionare
unique(dem_data$country_split)
unique(dem_data$country)

model <- 'dem_model =~ leader + experts + army + democracy'

fit <- cfa(model, data = dem_data)
summary(fit, standardized = TRUE, fit.measures = TRUE)

