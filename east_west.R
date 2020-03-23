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
economic =~ taxRich + equalIncome + helpUnemp + obeyRulers;
liberal =~ freeElection + genderEquality + civilRights + importance +
            helpUnemp'

east_fit <- cfa(east_model, data = east_data, estimator = "MLR")
summary(east_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
reliability(east_fit)

measurementInvariance(model = east_model, data = east_data,
                      group="country", estimator = "MLR")
# wykonało się z ostrzeżeniami o kowariancjach

# schemat równań strukturalnych
semPaths(object = east_fit,
         whatLabels = "std",           
         edge.label.cex = 1,           
         layout = "tree",          
         what = "std", edge.color = "black")
