# Pakiety
install.packages("dplyr")
library(dplyr)
install.packages("lavaan")
library(lavaan)
install.packages("semTools")
library(semTools)


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

levels <- c(1:4) # Kody ujemne oznaczają braki odpowiedzi
# Poniżej poleceniem filter_at wybieram tylko te wiersze, które mają odpowiedź na każde pytanie
dem_data <- data %>% select(wave = V1, country = V2,
                    leader = V127, experts = V128, army = V129, democracy = V130) %>%
            filter(country %in% country_nr) %>%
            filter_at(vars(leader, experts, army, democracy), all_vars(. %in% levels))

head(dem_data)

# Wstępna analiza korelacji
cor(dem_data[,3:6], method="spearman")
# Democracy jest ujemnie skorelowana z wszystkimi pozostałymi
# Niedemokratyczne poglądy - chociaż w praktyce też nie da się ich łącznie zastosować -
# częściej występują razem
# Najsilniej skorelowane są army oraz leader


dem_data$leader = factor(dem_data$leader, ordered = T)
dem_data$experts = factor(dem_data$experts, ordered = T)
dem_data$army = factor(dem_data$army, ordered = T)
dem_data$democracy = factor(dem_data$democracy, ordered = T)

# Struktura zmiennej latentnej
model <- 'dem_model =~ leader + experts + army + democracy'
or.vars <- c("leader", "experts", "army", "democracy")


# Dopasowanie modelu CFA

fit <- cfa(model, data = dem_data, ordered=or.vars)
summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Modele do testowania równoważności pomiarowej

configural <- cfa(model, data=dem_data, ordered=or.vars, group="country")
weak <- cfa(model, data=dem_data, ordered=or.vars, group="country", group.equal="loadings")
strong <- cfa(model, data=dem_data, ordered=or.vars, group="country", group.equal=c("loadings", "intercepts"))

# Dodatkowy poziom, zwykle pomijany, bo nie jest konieczny do badań porównawczych
strict <- cfa(model, data=dem_data, group="country", group.equal=c("loadings", "intercepts", "residuals"))

#Test

anova(configural, weak)
# P-value bardzo małe, z czego by wynikało, że słaba równoważność nie jest spełniona

modelsCat <- list(fit.configural = configural, fit.loadings = weak)
partialInvarianceCat(modelsCat, type = "metric")

