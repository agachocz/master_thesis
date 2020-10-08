library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(captioner)
library(corrplot)
library(cluster)
library(semTools)
library(semPlot)
library(lavaan)
library(ggsci)
library(ggpubr)
library(ggdendro)
library(scales)
library(psych)

dem_data <- read.table("dem_data_even.csv")
questions <- c("Taxes for rich, aid for poor",
               "Influence of religious authorities",
               "Free elections",
               "Help for unemployed",
               "Army can take over",
               "Civil rights",
               "Income equality",
               "Obeying rulers",
               "Gender equality",
               "Importance of democracy")
names(questions) <- c("taxRich", "religiousLaw", "freeElection", "helpUnemp", 
                      "armyTakesOver", "civilRights", "equalIncome", "obeyRulers",
                      "genderEquality", "importance")

dem_data %>% 
  gather(key = "variable", value = "answer", -country, factor_key = T) %>%
  ggplot(aes(answer)) + geom_histogram(bins = 10, col = "black") +
  facet_wrap(~variable, nrow = 5, scales = "free", 
             labeller = labeller(variable = questions)) + theme_light() +
  theme(text = element_text(family = "serif")) +
  scale_x_continuous("Answer", breaks = 1:10) +
  scale_y_continuous("Number of answers")

cor.matrix <- cor(dem_data[,2:11], method="spearman")

quest.short <- c("X1: Taxing the rich",
                 "X2: Religious authorities",
                 "X3: Free elections",
                 "X4: Help for unemployed",
                 "X5: Army takes over",
                 "X6: Civil rights",
                 "X7: Income equality",
                 "X8: Obeying rulers",
                 "X9: Gender equality",
                 "X10: Importance of democracy")

colnames(cor.matrix) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")
rownames(cor.matrix) <- quest.short

par(family = 'serif')
corrplot(cor.matrix, method="circle", order='hclust', tl.col = "black", tl.cex = 0.7)
cortest.bartlett(cor.matrix, n = dim(dem_data)[1])

model <- '
fundamentalistic =~ obeyRulers + religiousLaw + armyTakesOver + equalIncome;
economic =~ taxRich + equalIncome + helpUnemp;
liberal =~ freeElection + genderEquality + civilRights + importance + helpUnemp'


fit <- cfa(model, data = dem_data, estimator = "MLR")
summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
reliability(fit)


quest.min <- c(obeyRulers = "X8", religiousLaw = "X2", armyTakesOver = "X5", 
               equalIncome = "X7", taxRich = "X1", helpUnemp = "X4", freeElection = "X3", 
               genderEquality = "X9", civilRights = "X6", importance = "X10", "I", "II", "III")

par(family = 'serif')
semPaths(object = fit,
         whatLabels = "std",           
         edge.label.cex = 1, nCharNodes = 0,         
         layout = "circle2",  sizeMan = 7, sizeMan2 = 7, shapeMan = "square",
         sizeLat = 7, sizeLat2 = 7, shapeLat = "circle",
         sizeInt = 5, sizeInt2 = 5,    
         what = "std", edge.color = "black",
         nodeLabels = quest.min, optimizeLatRes = T)

coeff <- standardizedSolution(fit)

# wagi
fnd_all_w <- coeff$est.std[1:4]
lib_all_w <- coeff$est.std[8:12]
econ_all_w <- coeff$est.std[5:7]

all_stats <- dem_data %>% mutate(I = (obeyRulers*fnd_all_w[1]+religiousLaw*fnd_all_w[2]+ 
                                        armyTakesOver*fnd_all_w[3]+equalIncome*fnd_all_w[4])/sum(fnd_all_w),
                                 II = (taxRich*econ_all_w[1]+equalIncome*econ_all_w[2]+ 
                                         helpUnemp*econ_all_w[3])/sum(econ_all_w),
                                 III = (freeElection*lib_all_w[1]+genderEquality*lib_all_w[2]+
                                          civilRights*lib_all_w[3]+importance*lib_all_w[4]+
                                          helpUnemp*lib_all_w[5])/sum(lib_all_w)) %>%
  mutate(leading = if_else(I > II, if_else(I > III, "I", "III"),
                           if_else(II > III, "II", "III")))

bxp <- all_stats %>% select(I, II, III) %>% gather(key="Concept", value="Value") %>%
  ggboxplot(x = "Concept", y = "Value",
            color = "Concept", palette = c("blue", "orange", "darkgreen")) +
  theme(legend.position = "none", text = element_text(family = "serif")) + 
  theme_light()

levels <- c("I", "II", "III")
all_stats$leading <- factor(all_stats$leading, levels)

bar <- all_stats %>% select(leading) %>% 
  ggplot(aes(as.factor(leading))) + 
  geom_bar(aes(y = ..count../sum(..count..), fill = "leading"),
           fill=c("blue", "orange", "darkgreen")) + 
  geom_text(aes(y = (..count..)/sum(..count..), 
                label = scales::percent(round(..count../sum(..count..), 2), accuracy = 1L)),
            stat = "count", vjust=-0.5, size=4, family = 'serif') +
  scale_y_continuous(labels = percent) +
  scale_x_discrete("Dominant concept", drop=F) +
  labs(y = "Percent of sample") + expand_limits(y = c(0, 1))  + 
  theme_minimal() + theme(text = element_text(family = "serif"))


yplot <- all_stats %>% select(I, II, III) %>% gather(key="Concept", value="Value") %>%
  ggdensity("Value", fill = "Concept", palette = c("blue", "orange", "darkgreen")) +
  rotate() + clean_theme()

all_stats_summary <- all_stats %>% summarise(fnd_mean = mean(I), 
                                             econ_mean = mean(II),
                                             lib_mean = mean(III))

ggarrange(bxp, yplot, bar, NULL,
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(2, 1),
          common.legend = TRUE)

dg <- as.dendrogram(clustering)

ggdendrogram(dg) + labs(y = "Wysokość") + 
  theme(panel.grid.major.y = element_line(size = 0.5, color = "lightgrey"),
        panel.grid.minor.y = element_line(size = 0.5, color = "lightgrey"),
        text = element_text(family = "serif"))


tree <- cutree(clustering, 3) #dendrogram
sil <- silhouette(tree, clustering$diss)

sil_data <- data.frame(countr.pl, cluster = sil[,1], width = sil[,3])
sil_data$countr.pl <- reorder(sil_data$countr.pl, sil_data$cluster)
sil_data$cluster <- as.factor(sil_data$cluster)

ggplot(sil_data) + 
  geom_col(aes(x = countr.pl, y = width, fill = cluster)) +
  scale_fill_manual(values = c("darkgreen", "blue", "red")) +
  coord_flip() + labs(x = "Państwo", y = "Wskaźnik sylwetkowy", fill = "Grupa") +
  theme(text = element_text(family = "serif"))

group1_countries <- countries[tree == 1]
group1_data <- dem_data %>% filter(country %in% group1_countries)

group1_cor <- cor(group1_data[,2:11], method="spearman")

colnames(group1_cor) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")
rownames(group1_cor) <- quest.short

par(family = 'serif')
corrplot(group1_cor, method="circle", order='hclust', tl.col = "black", tl.cex = 0.7)

group1_model <- '
I =~ obeyRulers + religiousLaw + armyTakesOver;
IIlud =~ taxRich + equalIncome + helpUnemp + obeyRulers;
III =~ freeElection + genderEquality + civilRights + importance + helpUnemp'

group1_fit <- cfa(group1_model, data = group1_data, estimator = "MLM")

quest.min <- c(obeyRulers = "X8", religiousLaw = "X2", armyTakesOver = "X5", 
               equalIncome = "X7", taxRich = "X1", helpUnemp = "X4", freeElection = "X3", 
               genderEquality = "X9", civilRights = "X6", importance = "X10", 
               "I", "IIlud", "III")

par(family = 'serif')
semPaths(object = group1_fit,
         whatLabels = "std",           
         edge.label.cex = 1, nCharNodes = 0,         
         layout = "circle2",  sizeMan = 7, sizeMan2 = 7, shapeMan = "square",
         sizeLat = 7, sizeLat2 = 7, shapeLat = "circle",
         sizeInt = 5, sizeInt2 = 5,    
         what = "std", edge.color = "black",
         nodeLabels = quest.min, optimizeLatRes = T)

coeff <- standardizedSolution(group1_fit)

# wagi
fnd_1_w <- coeff$est.std[1:3]
lib_1_w <- coeff$est.std[8:12]
econ_1_w <- coeff$est.std[4:7]

gr1_stats <- group1_data %>% mutate(I = (obeyRulers*fnd_1_w[1]+religiousLaw*fnd_1_w[2]+ 
                                           armyTakesOver*fnd_1_w[3])/sum(fnd_1_w),
                                    IIlud = (taxRich*econ_1_w[1]+equalIncome*econ_1_w[2]+ 
                                               helpUnemp*econ_1_w[3]+obeyRulers*econ_1_w[4])/sum(econ_1_w),
                                    III = (freeElection*lib_1_w[1]+genderEquality*lib_1_w[2]+
                                             civilRights*lib_1_w[3]+importance*lib_1_w[4]+
                                             helpUnemp*lib_1_w[5])/sum(lib_1_w)) %>%
  mutate(leading = if_else(I > IIlud, if_else(I > III, "I", "III"),
                           if_else(IIlud > III, "IIlud", "III")))


bxp <- gr1_stats %>% select(I, IIlud, III) %>% gather(key="Koncepcja", value="Wartość") %>%
  ggboxplot(x = "Koncepcja", y = "Wartość",
            color = "Koncepcja", palette = c("blue", "orange", "darkgreen")) +
  theme(legend.position = "none", text = element_text(family = "serif"))

levels <- c("I", "IIlud", "III")
gr1_stats$leading <- factor(gr1_stats$leading, levels)

bar <- gr1_stats %>% select(leading) %>% 
  ggplot(aes(as.factor(leading))) + 
  geom_bar(aes(y = ..count../sum(..count..), fill = "leading"),
           fill=c(I = "blue", IIlud = "orange", III = "darkgreen")) + 
  geom_text(aes(y = (..count..)/sum(..count..), 
                label = scales::percent(round(..count../sum(..count..), 2), accuracy = 1L)),
            stat = "count", vjust=-0.5, size=4, family = 'serif') +
  scale_y_continuous(labels = percent) +
  scale_x_discrete("Dominująca koncepcja", drop=F) +
  labs(y = "Procent grupy") + expand_limits(y = c(0, 1)) +
  theme(text = element_text(family = "serif"))


yplot <- gr1_stats %>% select(I, IIlud, III) %>% gather(key="Koncepcja", value="Wartość") %>%
  ggdensity("Wartość", fill = "Koncepcja", palette = c(I = "blue", IIlud = "orange", III = "darkgreen")) +
  rotate() + clean_theme()

ggarrange(bxp, yplot, bar, NULL,
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(2, 1),
          common.legend = TRUE)

options(knitr.kable.NA = '')
mi_tab_west <- data.frame(level = c("Konfiguralna", "Metryczna", "Skalarna"),
                          AIC = c(249265, 249434, 250656),
                          BIC = c(250325, 250255, 251266),
                          Chi = c("1714,8", "1955,7", "3242,4"),
                          Chi_d = c(NA, "240,81", "1286,74"),
                          df = c(165, 201, 233), p = c(NA,"< 2,2e-16", "< 2.2e-16"))

cap = "Wyniki analizy równoważności pomiarowej dla grupy oznaczonej na zielono. Źródło: opracowanie własne."

kable(mi_tab_west, format = "latex", col.names = c("Poziom równoważności", "AIC", "BIC", "$\\chi^2$", "Przyrost $\\chi^2$", "Stopnie swobody", "p-value"), caption = cap) %>%
  kable_styling(latex_options = c("scale_down"))

# GRUPA 2
group2_countries <- countries[tree == 2]
group2_data <- dem_data %>% filter(country %in% group2_countries)

group2_cor <- cor(group2_data[,2:11], method="spearman")

colnames(group2_cor) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")
rownames(group2_cor) <- quest.short

par(family = 'serif')
corrplot(group2_cor, method="circle", order='hclust', tl.col = "black", tl.cex = 0.7)

# Struktura modelu dla pierwszej grupy
group2_model <- '
Ilib =~ obeyRulers + religiousLaw + armyTakesOver; 
II =~ taxRich + equalIncome + helpUnemp;
III =~ freeElection + genderEquality + civilRights + importance + helpUnemp'

group2_fit <- cfa(group2_model, data = group2_data, estimator = "MLM")
```
```{r diagram-west, fig.align = 'center', fig.cap = "Factor analysis model for the group marked with blue. Source of data: WVS Wave 6."}

quest.min <- c(obeyRulers = "X8", religiousLaw = "X2", armyTakesOver = "X5", 
               equalIncome = "X7", taxRich = "X1", helpUnemp = "X4", freeElection = "X3", 
               genderEquality = "X9", civilRights = "X6", importance = "X10", "Ilib", "II", "III")

par(family = 'serif')
semPaths(object = group2_fit,
         whatLabels = "std",           
         edge.label.cex = 1, nCharNodes = 0,         
         layout = "circle2",  sizeMan = 7, sizeMan2 = 7, shapeMan = "square",
         sizeLat = 7, sizeLat2 = 7, shapeLat = "circle",
         sizeInt = 5, sizeInt2 = 5,    
         what = "std", edge.color = "black",
         nodeLabels = quest.min, optimizeLatRes = T)

coeff <- standardizedSolution(group2_fit)

# wagi
fnd_2_w <- coeff$est.std[1:3]
lib_2_w <- coeff$est.std[7:11]
econ_2_w <- coeff$est.std[4:6]

gr2_stats <- group2_data %>% mutate(Ilib = (obeyRulers*fnd_2_w[1]+religiousLaw*fnd_2_w[2]+ 
                                              armyTakesOver*fnd_2_w[3])/sum(fnd_2_w),
                                    II = (taxRich*econ_2_w[1]+equalIncome*econ_2_w[2]+ 
                                            helpUnemp*econ_2_w[3])/sum(econ_2_w),
                                    III = (freeElection*lib_2_w[1]+genderEquality*lib_2_w[2]+
                                             civilRights*lib_2_w[3]+importance*lib_2_w[4]+
                                             helpUnemp*lib_2_w[5])/sum(lib_2_w)) %>%
  mutate(leading = if_else(Ilib > II, if_else(Ilib > III, "Ilib", "III"),
                           if_else(II > III, "II", "III")))


bxp <- gr2_stats %>% select(Ilib, II, III) %>% gather(key="Koncepcja", value="Wartość") %>%
  ggboxplot(x = "Koncepcja", y = "Wartość",
            color = "Koncepcja", palette = c("blue", "orange", "darkgreen")) +
  theme(legend.position = "none", text = element_text(family = "serif"))

levels <- c("Ilib", "II", "III")
gr2_stats$leading <- factor(gr2_stats$leading, levels)

bar <- gr2_stats %>% select(leading) %>% 
  ggplot(aes(as.factor(leading))) + 
  geom_bar(aes(y = ..count../sum(..count..), fill = "leading"),
           fill=c("blue", "orange", "darkgreen")) + 
  geom_text(aes(y = (..count..)/sum(..count..), 
                label = scales::percent(round(..count../sum(..count..), 2), accuracy = 1L)),
            stat = "count", vjust=-0.5, size=4, family = 'serif') +
  scale_y_continuous(labels = percent) +
  scale_x_discrete("Dominująca koncepcja", drop=F) +
  labs(y = "Procent grupy") + expand_limits(y = c(0, 1)) +
  theme(text = element_text(family = "serif"))


yplot <- gr2_stats %>% select(Ilib, II, III) %>% gather(key="Koncepcja", value="Wartość") %>%
  ggdensity("Wartość", fill = "Koncepcja", palette = c(Ilib = "blue", II = "orange", III = "darkgreen")) +
  rotate() + clean_theme()

ggarrange(bxp, yplot, bar, NULL,
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(2, 1),
          common.legend = TRUE)

options(knitr.kable.NA = '')
mi_tab_west <- data.frame(level = c("Konfiguralna", "Metryczna", "Skalarna"),
                          AIC = c(180493, 180611, 181894),
                          BIC = c(181145, 181161, 182354),
                          Chi = c("458,66", "608,95", "1919,63"),
                          Chi_d = c(NA, "150,29", "1310,68"),
                          df = c(93, 109, 123), p = c(NA,"< 2,2e-16", "< 2.2e-16"))

cap = "Wyniki analizy równoważności pomiarowej dla grupy demokracji liberalnych. Źródło: opracowanie własne."

kable(mi_tab_west, format = "latex", col.names = c("Poziom równoważności", "AIC", "BIC", "$\\chi^2$", "Przyrost $\\chi^2$", "Stopnie swobody", "p-value"), caption = cap) %>%
  kable_styling(latex_options = c("scale_down"))

group3_countries <- countries[tree == 3]
group3_data <- dem_data %>% filter(country %in% group1_countries)

group3_cor <- cor(group1_data[,2:11], method="spearman")

colnames(group3_cor) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")
rownames(group3_cor) <- quest.short

par(family = 'serif')
corrplot(group3_cor, method="circle", order='hclust', tl.col = "black", tl.cex = 0.7)

group3_model <- '
I =~ obeyRulers + religiousLaw + armyTakesOver; 
IV =~ taxRich + equalIncome + helpUnemp +
                     freeElection + genderEquality + civilRights + importance + obeyRulers'

group3_fit <- cfa(group3_model, data = group3_data, estimator = "MLM")

quest.min <- c(obeyRulers = "X8", religiousLaw = "X2", armyTakesOver = "X5", 
               equalIncome = "X7", taxRich = "X1", helpUnemp = "X4", freeElection = "X3", 
               genderEquality = "X9", civilRights = "X6", importance = "X10", 
               "I", "IV")

par(family = 'serif')
semPaths(object = group3_fit,
         whatLabels = "std",           
         edge.label.cex = 1, nCharNodes = 0,         
         layout = "circle2",  sizeMan = 7, sizeMan2 = 7, shapeMan = "square",
         sizeLat = 7, sizeLat2 = 7, shapeLat = "circle",
         sizeInt = 5, sizeInt2 = 5,    
         what = "std", edge.color = "black",
         nodeLabels = quest.min, optimizeLatRes = T)

coeff <- standardizedSolution(group3_fit)

# wagi
fnd_3_w <- coeff$est.std[1:3]
lib_econ_3_w <- coeff$est.std[4:11]

gr3_stats <- group3_data %>% mutate(I = (obeyRulers*fnd_3_w[1]+religiousLaw*fnd_3_w[2]+ 
                                           armyTakesOver*fnd_3_w[3])/sum(fnd_3_w),
                                    IV = (taxRich*lib_econ_3_w[1]+equalIncome*lib_econ_3_w[2]+ 
                                            helpUnemp*lib_econ_3_w[3]+freeElection*lib_econ_3_w[4]+
                                            genderEquality*lib_econ_3_w[5]+civilRights*lib_econ_3_w[6]+
                                            importance*lib_econ_3_w[7]+obeyRulers*lib_econ_3_w[8])/sum(lib_econ_3_w)) %>%
  mutate(leading = if_else( (I >= IV), "I","IV"))


bxp <- gr3_stats %>% select(I, IV) %>% gather(key="Koncepcja", value="Wartość") %>%
  ggboxplot(x = "Koncepcja", y = "Wartość",
            color = "Koncepcja", palette = c("blue", "darkgreen")) +
  theme(legend.position = "none", text = element_text(family = "serif"))

levels <- c("I", "IV")
gr3_stats$leading <- factor(gr3_stats$leading, levels)

bar <- gr3_stats %>% select(leading) %>% 
  ggplot(aes(as.factor(leading))) + 
  geom_bar(aes(y = ..count../sum(..count..), fill = "leading"),
           fill=c(I = "blue", IV = "darkgreen")) + 
  geom_text(aes(y = (..count..)/sum(..count..), 
                label = scales::percent(round(..count../sum(..count..), 2), accuracy = 1L)),
            stat = "count", vjust=-0.5, size=4, family = 'serif') +
  scale_y_continuous(labels = percent) +
  scale_x_discrete("Dominująca koncepcja", drop=F) +
  labs(y = "Procent grupy") + expand_limits(y = c(0, 1)) +
  theme(text = element_text(family = "serif"))

yplot <- gr3_stats %>% select(I, IV) %>% gather(key="Koncepcja", value="Wartość") %>%
  ggdensity("Wartość", fill = "Koncepcja", palette = c(I = "blue",  IV = "darkgreen")) +
  rotate() + clean_theme()

ggarrange(bxp, yplot, bar, NULL,
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(2, 1),
          common.legend = TRUE)

options(knitr.kable.NA = '')
mi_tab_3 <- data.frame(level = c("Konfiguralna", "Metryczna", "Skalarna"),
                       AIC = c(361023, 361393, 363453),
                       BIC = c(362370, 362423, 364203 ),
                       Chi = c("2498,0", "2957,1", "5097,1"),
                       Chi_d = c(NA, "459,1", "2140,0"),
                       df = c(198, 243, 283 ), p = c(NA,"< 2,2e-16", "< 2.2e-16"))

cap = "Wyniki analizy równoważności pomiarowej dla grupy oznaczonej na czerwono. Źródło: opracowanie własne."

kable(mi_tab_3, format = "latex", col.names = c("Poziom równoważności", "AIC", "BIC", "$\\chi^2$", "Przyrost $\\chi^2$", "Stopnie swobody", "p-value"), caption = cap) %>%
  kable_styling(latex_options = c("scale_down"))





