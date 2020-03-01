# koncepcja: szukanie podobnych krajów metodą aglomeracyjną


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

# cor.data <- cbind(countries, cor.data)
rownames(cor.data) <- countries

summary(cor.data)


# drzewa
library(cluster)

clustering <- agnes(cor.data, method="complete")
plot(clustering)

tree <- cutree(clustering, 3) #dendrogram
tree

countries[tree == 1]
countries[tree == 2]
countries[tree == 3]



# analiza CFA

cor.matrix <- cor(dem_data[,2:11], method="spearman")
corrplot(cor.matrix, method="circle", order='hclust')

# Test losowości
cor.vec <- vector()

for(i in 2:10){
  cor.vec <- c(cor.vec, cor.matrix[i, 1:(i-1)])   
}

uniform <- runif(45, -1, 1)

test_unif <- ks.test(cor.vec, uniform)





# Struktura modelu
model <- '
fundamentalistic =~ obeyRulers + religiousLaw + armyTakesOver + equalIncome;
economic =~ taxRich + equalIncome + helpUnemp;
liberal =~ freeElection + genderEquality + civilRights + importance + helpUnemp'


fit <- cfa(model, data = dem_data)
summary_fit <- summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
reliability(fit)

# schematy równań strukturalnych
semPaths(object = fit,
         whatLabels = "std",           
         edge.label.cex = 1,           
         layout = "tree",          
         what = "std", edge.color = "black")

# modificationIndices(fit, standardized = T, sort = T)

# NIE MOŻNA ZNALEŹĆ ROZWIĄZANIA
configural <- cfa(model, data=dem_data, group="country")
weak <- cfa(model, data=dem_data, group="country", group.equal="loadings")

summary(configural, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
###

measurementInvariance(model = model, data = dem_data, group="country")



# Podział na grupy wg. drzewa

# GRUPA 1
group1_countries <- countries[tree == 1]
group1_data <- dem_data %>% filter(country %in% group1_countries)

group1_cor <- cor(group1_data[,2:11], method="spearman")
corrplot(group1_cor, method="circle", order='hclust')

# Struktura modelu dla pierwszej grupy
group1_model <- '
fundam_econ =~ obeyRulers + religiousLaw + armyTakesOver + 
                taxRich + equalIncome + helpUnemp;
liberal =~ freeElection + genderEquality + civilRights + importance + helpUnemp'

group1_fit <- cfa(group1_model, data = group1_data)
summary(group1_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

measurementInvariance(model = group1_model, data = group1_data, group="country")

# schemat równań strukturalnych
semPaths(object = group1_fit,
         whatLabels = "std",           
         edge.label.cex = 1,           
         layout = "tree",          
         what = "std", edge.color = "black")


# GRUPA 2
group2_countries <- countries[tree == 2]
group2_data <- dem_data %>% filter(country %in% group2_countries)

group2_cor <- cor(group2_data[,2:11], method="spearman")
corrplot(group2_cor, method="circle", order='hclust')

# Struktura modelu dla pierwszej grupy
group2_model <- '
fundamentalistic =~ obeyRulers + religiousLaw + armyTakesOver; 
economic =~ taxRich + equalIncome + helpUnemp;
liberal =~ freeElection + genderEquality + civilRights + importance + helpUnemp'

group2_fit <- cfa(group2_model, data = group2_data)
summary(group2_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

measurementInvariance(model = group2_model, data = group2_data, group="country")

# schemat równań strukturalnych
semPaths(object = group2_fit,
         whatLabels = "std",           
         edge.label.cex = 1,           
         layout = "tree",          
         what = "std", edge.color = "black")

# GRUPA 3
group3_countries <- countries[tree == 3]
group3_data <- dem_data %>% filter(country %in% group3_countries)

group3_cor <- cor(group3_data[,2:11], method="spearman")
corrplot(group3_cor, method="circle", order='hclust')

# Struktura modelu dla pierwszej grupy
group3_model <- '
fundamentalistic =~ obeyRulers + religiousLaw + armyTakesOver; 
liberal_economic =~ taxRich + equalIncome + helpUnemp +
                     freeElection + genderEquality + civilRights + importance'

group3_fit <- cfa(group3_model, data = group3_data)
summary(group3_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

measurementInvariance(model = group3_model, data = group3_data, group="country")

# schemat równań strukturalnych
semPaths(object = group3_fit,
         whatLabels = "std",           
         edge.label.cex = 1,           
         layout = "tree",          
         what = "std", edge.color = "black")


# Próba z najbliższą podgrupą

small_group_countries <- c("Kazakhstan", "Russia")
small_group_data <- dem_data %>% filter(country %in% small_group_countries)

small_group_cor <- cor(small_group_data[,2:11], method="spearman")
corrplot(small_group_cor, method="circle", order='hclust')

# Struktura modelu dla pierwszej grupy
small_group_model <- '
liberal_economic =~ taxRich + equalIncome + helpUnemp + obeyRulers +
                     freeElection + genderEquality + civilRights'

small_group_fit <- cfa(small_group_model, data = small_group_data, orthogonal = T)
summary(small_group_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

measurementInvariance(model = small_group_model, data = small_group_data, group="country")


small_group_countries <- c("Kazakhstan", "Russia")
small_group_data <- dem_data %>% filter(country %in% small_group_countries)

small_group_cor <- cor(small_group_data[,2:11], method="spearman")
corrplot(small_group_cor, method="circle", order='hclust')



# Druga Próba z najbliższą podgrupą

small_group_countries_2 <- c("Poland", "Estonia")
small_group_data_2 <- dem_data %>% filter(country %in% small_group_countries_2)

small_group_cor_2 <- cor(small_group_data_2[,2:11], method="spearman")
corrplot(small_group_cor_2, method="circle", order='hclust')

# Struktura modelu dla pierwszej grupy
small_group_model_2 <- '
fundamentalistic =~ obeyRulers + religiousLaw + armyTakesOver; 
economic =~ taxRich + equalIncome + helpUnemp;
liberal =~ freeElection + genderEquality + civilRights + importance'

small_group_fit_2 <- cfa(small_group_model_2, data = small_group_data_2)
summary(small_group_fit_2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

measurementInvariance(model = small_group_model_2, data = small_group_data_2, group="country")
