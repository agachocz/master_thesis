# koncepcja: szukanie podobnych krajów metodą aglomeracyjną

#czcionka
font_import(pattern = "lmroman*")
font_import()

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
#country_nr <- c(12, 31, 32, 36, 48, 51, 76, 112, 152, 156, 158, 170, 196, 218, 233, 268, 275, 276, 288, 332, 344, 
#                356, 368, 392, 398, 400, 410, 414, 417, 422, 434, 458, 484, 504, 554, 566, 586, 604, 608, 616, 642,
#                643, 646, 702, 710, 716, 752, 764, 788, 792, 804, 818, 840, 858, 860, 887)
#country_name <- c("Algeria",  "Azerbaijan", "Argentina",  "Australia",  "Bahrain",  "Armenia",  "Brazil", "Belarus",  "Chile",
#                  "China",  "Taiwan", "Colombia", "Cyprus", "Ecuador", "Estonia", "Georgia",  "Palestine",  "Germany", "Ghana",
#                  "Haiti",  "Hong Kong",  "India",  "Iraq", "Japan",  "Kazakhstan", "Jordan", "South Korea", "Kuwait", "Kyrgyzstan",
#                  "Lebanon",  "Libya",  "Malaysia", "Mexico", "Morocco", "New Zealand", "Nigeria", "Pakistan",  "Peru",  "Philippines",  
#                  "Poland", "Romania",  "Russia", "Rwanda", "Singapore", "South Africa", "Zimbabwe", "Sweden", "Thailand",  "Tunisia",
#                  "Turkey", "Ukraine",  "Egypt",  "United States",  "Uruguay",  "Uzbekistan", "Yemen")


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

write.table(dem_data, "dem_data.csv")

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
library(ggdendro)

countr.pl <- c("Białoruś", "Estonia", "Gruzja", "Niemcy", "Kazahstan", "Holandia", "Polska",
               "Rumunia", "Rosja", "Słowenia", "Hiszpania", "Szwecja", "Turcja", "Ukraina")
rownames(cor.data) <- countr.pl
clustering <- agnes(cor.data, method="complete")

dg <- as.dendrogram(clustering)


ggdendrogram(dg) + labs(y = "Wysokość") + 
  theme(panel.grid.major.y = element_line(size = 0.5, color = "lightgrey"),
        panel.grid.minor.y = element_line(size = 0.5, color = "lightgrey"),
        text = element_text(family = "serif"))


tree <- cutree(clustering, 3) #dendrogram
tree

countries[tree == 1]
countries[tree == 2]
countries[tree == 3]

sil <- silhouette(tree, clustering$diss)

sil_data <- data.frame(countr.pl, cluster = sil[,1], width = sil[,3])
sil_data$countr.pl <- reorder(sil_data$countr.pl, sil_data$cluster)
sil_data$cluster <- as.factor(sil_data$cluster)

ggplot(sil_data) + 
  geom_col(aes(x = countr.pl, y = width, fill = cluster)) +
  scale_fill_manual(values = c("darkgreen", "blue", "red")) +
  coord_flip() + labs(x = "Państwo", y = "Wskaźnik sylwetkowy", fill = "Grupa") +
  theme(text = element_text(family = 'serif'))

# analiza CFA

cor.matrix <- cor(dem_data[,2:11], method="spearman")
cortest.bartlett(cor.matrix, n = dim(dem_data)[1])

quest.short <- c("X1: Opodatkowanie bogatych",
                  "X2: Autorytety religijne",
                  "X3: Wolne wybory",
                  "X4: Pomoc bezrobotnym",
                  "X5: Wojsko przejmuje władzę",
                  "X6: Prawa obywatelskie",
                  "X7: Wyrównywanie dochodów",
                  "X8: Posłuszenstwo rządzącym",
                  "X9: Równouprawnienie",
                  "X10: Ważność demokracji")

colnames(cor.matrix) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")
rownames(cor.matrix) <- quest.short

corrplot(cor.matrix, method="circle", order='hclust', tl.col = "black", tl.cex = 0.7)






# Struktura modelu
model <- '
I =~ obeyRulers + religiousLaw + armyTakesOver + equalIncome;
II =~ taxRich + equalIncome + helpUnemp;
III =~ freeElection + genderEquality + civilRights + importance + helpUnemp'


fit <- cfa(model, data = dem_data, estimator = "MLM")
summary_fit <- summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
reliability(fit)

quest.min <- c(obeyRulers = "X8", religiousLaw = "X2", armyTakesOver = "X5", 
               equalIncome = "X7", taxRich = "X1", helpUnemp = "X4", freeElection = "X3", 
               genderEquality = "X9", civilRights = "X6", importance = "X10", "I", "II", "III")

# schematy równań strukturalnych
semPaths(object = fit,
         whatLabels = "std",           
         edge.label.cex = 1, nCharNodes = 0,         
         layout = "circle2",  sizeMan = 7, sizeMan2 = 7, shapeMan = "square",
         sizeLat = 7, sizeLat2 = 7, shapeLat = "circle",
         sizeInt = 5, sizeInt2 = 5,    
         what = "std", edge.color = "black",
         nodeLabels = quest.min, optimizeLatRes = T)


# modificationIndices(fit, standardized = T, sort = T)

# NIE MOŻNA ZNALEŹĆ ROZWIĄZANIA

measurementInvariance(model = model, data = dem_data, group="country", estimator = "MLR")



# Podział na grupy wg. drzewa

# GRUPA 1
group1_countries <- countries[tree == 1]
group1_data <- dem_data %>% filter(country %in% group1_countries)

group1_cor <- cor(group1_data[,2:11], method="spearman")
corrplot(group1_cor, method="circle", order='hclust')

cortest.bartlett(group1_cor, n = dim(group1_data)[1])

# Struktura modelu dla pierwszej grupy
group1_model <- '
fundamentalistic =~ obeyRulers + religiousLaw + armyTakesOver;
economic =~ taxRich + equalIncome + helpUnemp + obeyRulers;
liberal =~ freeElection + genderEquality + civilRights + importance + helpUnemp'

group1_fit <- cfa(group1_model, data = group1_data, estimator = "MLR")
group1_summary <- summary(group1_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
reliability(group1_fit)
measurementInvariance(model = group1_model, data = group1_data, group="country", estimator = "MLR")

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
cortest.bartlett(group2_cor, n = dim(group2_data)[1])


# Struktura modelu dla pierwszej grupy
group2_model <- '
fundamentalistic =~ obeyRulers + religiousLaw + armyTakesOver; 
economic =~ taxRich + equalIncome + helpUnemp;
liberal =~ freeElection + genderEquality + civilRights + importance + helpUnemp'

group2_fit <- cfa(group2_model, data = group2_data, estimator = "MLR")
group2_summary <- summary(group2_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
reliability(group2_fit)

group2_mi <- measurementInvariance(model = group2_model, data = group2_data, 
                                   group="country", estimator = "MLR")



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
cortest.bartlett(group3_cor, n = dim(group3_data)[1])



# Struktura modelu dla pierwszej grupy
group3_model <- '
fundamentalistic =~ obeyRulers + religiousLaw + armyTakesOver; 
liberal_economic =~ taxRich + equalIncome + helpUnemp +
                     freeElection + genderEquality + civilRights + importance + obeyRulers'

group3_fit <- cfa(group3_model, data = group3_data, estimator = "MLR")
group3_summary <- summary(group3_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
reliability(group3_fit)
measurementInvariance(model = group3_model, data = group3_data, group="country", estimator = "MLR")

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
