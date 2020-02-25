# koncepcja: szukanie podobnych krajów metodą k najbliższych sąsiadów

# Przegląd macierzy korelacji
unique(dem_data$country)

dem_data %>% filter(country == "Poland") %>% select(taxRich : importance) %>%
  cor(method="spearman") %>%
  corrplot(method="circle", order='hclust')

dem_data %>% filter(country == "Estonia") %>% select(taxRich : importance) %>%
  cor(method="spearman") %>%
  corrplot(method="circle", order='hclust')

dem_data %>% filter(country == "Germany") %>% select(taxRich : importance) %>%
  cor(method="spearman") %>%
  corrplot(method="circle", order='hclust')

dem_data %>% filter(country == "Romania") %>% select(taxRich : importance) %>%
  cor(method="spearman") %>%
  corrplot(method="circle", order='hclust')

dem_data %>% filter(country == "Slovenia") %>% select(taxRich : importance) %>%
  cor(method="spearman") %>%
  corrplot(method="circle", order='hclust')


# włączenie wszystkich państw europejskich

country_nr <- c(40, 56, 100, 192, 203, 208, 233, 246, 350, 276, 300, 348, 380, 428, 440, 442, 372, 528, 616,
                620, 703, 705, 724, 756, 826, 470, 642, # koniec UE
                112, 268, 398, 643, 752, 792, 804)
country_name <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland",
                  "France", "Germany", "Greece", "Hungary", "Italy", "Latvia", "Lithuania", "Luxembourg",
                  "Ireland", "Netherlands", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain",
                  "Switzerland", "Great Britain", "Malta", "Romania", # koniec UE
                  "Belarus", "Georgia", "Kazakhstan", "Russia", "Sweden", "Turkey", "Ukraine")

countries <- data.frame(country_nr, country_name)


# wszystkie kraje
country_nr <- c(12, 31, 32, 36, 48, 51, 76, 112, 152, 156, 158, 170, 196, 218, 233, 268, 275, 276, 288, 332, 344, 
                356, 368, 392, 398, 400, 410, 414, 417, 422, 434, 458, 484, 504, 554, 566, 586, 604, 608, 616, 642,
                643, 646, 702, 710, 716, 752, 764, 788, 792, 804, 818, 840, 858, 860, 887)
country_name <- c("Algeria",  "Azerbaijan", "Argentina",  "Australia",  "Bahrain",  "Armenia",  "Brazil", "Belarus",  "Chile",
                  "China",  "Taiwan", "Colombia", "Cyprus", "Ecuador", "Estonia", "Georgia",  "Palestine",  "Germany", "Ghana",
                  "Haiti",  "Hong Kong",  "India",  "Iraq", "Japan",  "Kazakhstan", "Jordan", "South Korea", "Kuwait", "Kyrgyzstan",
                  "Lebanon",  "Libya",  "Malaysia", "Mexico", "Morocco", "New Zealand", "Nigeria", "Pakistan",  "Peru",  "Philippines",  
                  "Poland", "Romania",  "Russia", "Rwanda", "Singapore", "South Africa", "Zimbabwe", "Sweden", "Thailand",  "Tunisia",
                  "Turkey", "Ukraine",  "Egypt",  "United States",  "Uruguay",  "Uzbekistan", "Yemen")


dem_data <- data %>% select(country = V2,
                            taxRich = V131, religiousLaw = V132, freeElection = V133, helpUnemp = V134,
                            armyTakesOver = V135, civilRights = V136, equalIncome = V137, 
                            obeyRulers = V138, genderEquality = V139, importance = V140) %>%
  filter(country %in% country_nr) %>%
  filter_at(vars(taxRich:importance), all_vars(. > 0))
# Ustawienie zmiennych w tym samym "kierunku"

# Zmiana oznaczeń numerycznych na nazwy krajów
dem_data$country <- as.factor(dem_data$country)
dem_data$country <- mapvalues(dem_data$country, from = country_nr, to = country_name)

head(dem_data)

# korelacje

countries <- unique(dem_data$country)

cor.data <- data.frame()

for(c in countries){
  
  #c = "Poland"
  cor.matrix <- dem_data %>% filter(country == c) %>% select(taxRich : importance) %>%
    cor(method="spearman")
  
  cor.vec <- vector()
  
   for(i in 2:10){
    cor.vec <- c(cor.vec, cor.matrix[i, 1:(i-1)])   
   }
  
  cor.data <- rbind(cor.data, cor.vec)
}

cor.data <- cbind(countries, cor.data)

summary(cor.data)

# KNN

install.packages("cluster")
library(cluster)

clusters <- kmeans(cor.data[,-1], 3)

plot(cor.data, col=clusters$cluster)
