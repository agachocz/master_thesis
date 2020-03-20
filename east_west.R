# podział na 2 grupy
# zachód: Niemcy, Holandia, Szwecja, Hiszpania (?)
# wschód (za żelazną kurtyną): Białoruś, Estonia, Polska, Gruzja
# Rumunia, Kazahstan, Rosja, Ukraina, Słowenia (?)
# Turcja wypada z badania

# EAST

east_countries <- c("Belarus", "Estonia", "Georgia", "Kazakhstan", "Poland",
                    "Romania", "Russia", "Slovenia", "Ukraine")
east_data <- dem_data %>% filter(country %in% east_countries)

east_cor <- cor(east_data[,2:11], method="spearman")
corrplot(east_cor, method="circle", order='hclust')
cortest.bartlett(east_cor, n = dim(east_data)[1])

east_model <- '
fundamentalistic =~ obeyRulers + religiousLaw + armyTakesOver;
economic =~ taxRich + equalIncome + helpUnemp;
liberal =~ freeElection + genderEquality + civilRights + importance + helpUnemp + taxRich'

east_fit <- cfa(east_model, data = east_data)
summary(east_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

measurementInvariance(model = east_model, data = east_data, group="country")
# wykonało się z ostrzeżeniami o kowariancjach

# schemat równań strukturalnych
semPaths(object = east_fit,
         whatLabels = "std",           
         edge.label.cex = 1,           
         layout = "tree",          
         what = "std", edge.color = "black")



# WEST

west_countries <- c("Germany", "Netherlands", "Spain", "Sweden")
west_data <- dem_data %>% filter(country %in% west_countries)

west_cor <- cor(west_data[,2:11], method="spearman")
corrplot(west_cor, method="circle", order='hclust')
cortest.bartlett(west_cor, n = dim(west_data)[1])

west_model <- '
fundamentalistic =~ obeyRulers + religiousLaw + armyTakesOver;
economic =~ taxRich + equalIncome + helpUnemp;
liberal =~ freeElection + genderEquality + civilRights + importance + helpUnemp + taxRich'

west_fit <- cfa(west_model, data = west_data)
summary(west_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

measurementInvariance(model = west_model, data = west_data, group="country")
# uruchomiło się poprawnie 

# schemat równań strukturalnych
semPaths(object = east_fit,
         whatLabels = "std",           
         edge.label.cex = 1,           
         layout = "tree",          
         what = "std", edge.color = "black")