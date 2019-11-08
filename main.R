# Pakiety
install.packages("dplyr")
library(dplyr)
install.packages("lavaan")
library(lavaan)
install.packages("semTools")
library(semTools)
install.packages("psych")
library(psych)
install.packages("corrplot")
library(corrplot)

# Wczytanie danych
data <- readRDS("data.rds")


# Kody krajów UE - niewiele z nich ma odpowiedzi na te pytania,
# można ewentualnie rozszerzyć na pozostałe europejskie
country_nr <- c(40, 56, 100, 192, 203, 208, 233, 246, 350, 276, 300, 348, 380, 428, 440, 442, 372, 528, 616,
                620, 703, 705, 724, 756, 826, 470, 642)
country_name <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland",
                  "France", "Germany", "Greece", "Hungary", "Italy", "Latvia", "Lithuania", "Luxembourg",
                  "Ireland", "Netherlands", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain",
                  "Switzerland", "Great Britain", "Malta", "Romania")

countries <- cbind(country_nr, country_name)

# Wybrane zmienne
# V2 - Country (nr)

# Skala 1 : 1 - Bardzo dobrze, 4 - Bardzo źle
# V127 - leader - silny lider rządzi
# V128 - experts - Rządy ekspertów
# V129 - army - Arma rządzi
# V130 - democracy - Rządy demokratyczne

# Skala 2 : 1 - Nie jest istotną cechą demokracji, 10 - jest istotną cechą demokracji
# V131 - taxRich - Rząd opodatkowuje bogatych i oddaje ubogim
# V132 - religiousLaw - Autorytety religijne interpretują prawo
# V133 - freeElection - Liderzy są wybierani w wolnych wyborach
# V134 - helpUnemp - Bezrobotni otrzymują pomoc od państwa
# V135 - armyTakesOver - Armia przejmuje władzę jeśli rząd jest niekompetentny
# V136 - civilRights - Prawa obywatelskie chronią człowieka przed opresją państwa
# V137 - equalIncome - Państwo dba o równość dochodów
# V138 - obeyRulers - Ludzie są posłuszni rządzącym
# V139 - genderEquality - Kobiety mają takie same prawa jak mężczyźni

# V140 - importance - 1: Demokracja nie jest istotna - 10: Demokracja jest absolutnie istotna


# Kody ujemne oznaczają braki odpowiedzi
# Poniżej poleceniem filter_at wybieram tylko te wiersze, które mają odpowiedź na każde pytanie
dem_data <- data %>% select(wave = V1, country = V2,
                    leader = V127, experts = V128, army = V129, democracy = V130,
                    taxRich = V131, religiousLaw = V132, freeElection = V133, helpUnemp = V134,
                    armyTakesOver = V135, civilRights = V136, equalIncome = V137, 
                    obeyRulers = V138, genderEquality = V139, importance = V140) %>%
            filter(country %in% country_nr) %>%
            filter_at(vars(leader:importance), all_vars(. > 0))

head(dem_data)



# Wstępna analiza korelacji - "polychoric correlation"
poly.cor <- polychoric(dem_data[,3:6])
poly.cor.matrix <- poly.cor$rho
corrplot(poly.cor.matrix, method="circle")
# Democracy jest ujemnie skorelowana z wszystkimi pozostałymi
# Niedemokratyczne poglądy - chociaż w praktyce też nie da się ich łącznie zastosować -
# częściej występują razem
# Najsilniej skorelowane są army oraz leader

# Dla pozostałych pytań (które mają dłuższą skalę odpowiedzi)
# Stosuję korelację rangową
# (funkcja polychoric zwraca błąd, że przy więcej niż 8 wartości nie sensu jej stosowanie)
cor.matrix <- cor(dem_data[,7:16], method="spearman")
corrplot(cor.matrix, method="circle", order='hclust')

# Są tu trzy główne skupiska:
# "fundamentalistyczne": istotność autorytetów religijnych, posłuszeństwo wobec władzy i armia,
#                         która może przejąć władzę, gdy rząd jest niekompetentny
# "ekonomiczne": państwo demokratyczne powinno opodatkować bogatych, a wspierać biednych,
#                 w demokracji powinny być w miarę równomierne zarobki
# "liberalne": demokracja jest ważna, wiąże się z prawami człowieka, równością płci, wolnymi wyborami
# Dodatkowo: państwo wspierające bezrobotnych pasuje zarówno do "libearlnej" jak i "ekonomiecznej"
#            "liberalna" najbardziej "kłóci się" z "fundamentalistyczną"



# Zmiana na kategoryczne uporządkowane

# apply zmienia je tylko na zmienne tekstowe ???
# dem_data[,3:16] <- apply(dem_data[,3:16], MARGIN = 2, FUN = factor, ordered = T) 

dem_data$leader = factor(dem_data$leader, ordered = T)
dem_data$experts = factor(dem_data$experts, ordered = T)
dem_data$army = factor(dem_data$army, ordered = T)
dem_data$democracy = factor(dem_data$democracy, ordered = T)

dem_data$taxRich = factor(dem_data$taxRich, ordered = T)
dem_data$religiousLaw = factor(dem_data$religiousLaw, ordered = T)
dem_data$freeElection = factor(dem_data$freeElection, ordered = T)
dem_data$helpUnemp = factor(dem_data$helpUnemp, ordered = T)
dem_data$armyTakesOver = factor(dem_data$armyTakesOver, ordered = T)
dem_data$civilRights = factor(dem_data$civilRights, ordered = T)
dem_data$equalIncome = factor(dem_data$equalIncome, ordered = T)
dem_data$obeyRulers = factor(dem_data$obeyRulers, ordered = T)
dem_data$genderEquality = factor(dem_data$genderEquality, ordered = T)
dem_data$importance = factor(dem_data$importance, ordered = T)




# Struktura modelu
model <- 'not_democratic =~ leader + experts + army;
          democratic =~ democracy;
          fundamentalistic =~ obeyRulers + religiousLaw + armyTakesOver;
          economic =~ taxRich + equalIncome + helpUnemp;
          liberal =~ freeElection + genderEquality + civilRights + importance'



# Dopasowanie modelu CFA

fit <- cfa(model, data = dem_data)
summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Modele do testowania równoważności pomiarowej ~~ błędy

configural <- cfa(model, data=dem_data, group="country")
weak <- cfa(model, data=dem_data, group="country", group.equal="loadings")
strong <- cfa(model, data=dem_data, group="country", group.equal=c("loadings", "intercepts"))

# Dodatkowy poziom, zwykle pomijany, bo nie jest konieczny do badań porównawczych
strict <- cfa(model, data=dem_data, group="country", group.equal=c("loadings", "intercepts", "residuals"))

#Test

anova(configural, weak)
# P-value bardzo małe, z czego by wynikało, że słaba równoważność nie jest spełniona

# modelsCat <- list(fit.configural = configural, fit.loadings = weak)
# partialInvarianceCat(modelsCat, type = "metric")
