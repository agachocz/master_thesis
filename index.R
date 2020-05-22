install.packages("ggpubr")
library(ggpubr)
install.packages("ggsci")
library(ggsci)

# obliczenie wskaźników demokracji w różnych koncepcjach dla modeli

# grupa ogólna

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


all_stats_summary <- all_stats %>% summarise(fnd_mean = mean(fnd), 
                      econ_mean = mean(econ),
                      lib_mean = mean(lib),
                      fnd_count = sum(is_fnd),
                      econ_count = sum(is_econ),
                      lib_count = sum(is_lib))


bxp <- all_stats %>% select(I, II, III) %>% gather(key="Koncepcja", value="Wartość") %>%
        ggboxplot(x = "Koncepcja", y = "Wartość",
                 color = "Koncepcja", palette = c("blue", "orange", "darkgreen")) +
        theme(legend.position = "none")

levels <- c("fundamentalistic", "economic", "liberal")
all_stats$leading <- factor(all_stats$leading, levels)

bar <- all_stats %>% select(leading) %>% 
              ggplot() + geom_bar(aes(leading, fill = "leading"), 
              fill=c("blue", "orange", "darkgreen")) + 
              scale_x_discrete("Dominująca koncepcja", drop=F) +
              scale_y_discrete("Liczba osób")
        

yplot <- all_stats %>% select(I, II, III) %>% gather(key="Koncepcja", value="Wartość") %>%
          ggdensity("Wartość", fill = "Koncepcja", palette = c("blue", "orange", "darkgreen")) +
          rotate() + clean_theme()

ggarrange(bxp, yplot, bar, NULL,
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(2, 1),
          common.legend = TRUE)


# demokracje liberalne (grupa 2)

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
  theme(legend.position = "none")

levels <- c("Ilib", "II", "III")
gr2_stats$leading <- factor(gr2_stats$leading, levels)

bar <- gr2_stats %>% select(leading) %>% 
  ggplot(aes(leading, fill = "leading")) + geom_bar(
  fill=c(Ilib = "blue", II = "orange", III = "darkgreen")) + 
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, size=4) +
  scale_x_discrete("Dominująca koncepcja", drop=F) +
  labs(y = "Liczba osób") + expand_limits(y = c(1, 4500))


yplot <- gr2_stats %>% select(Ilib, II, III) %>% gather(key="Koncepcja", value="Wartość") %>%
  ggdensity("Wartość", fill = "Koncepcja", palette = c(Ilib = "blue", II = "orange", III = "darkgreen")) +
  rotate() + clean_theme()

ggarrange(bxp, yplot, bar, NULL,
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(2, 0.7),
          common.legend = TRUE)



# grupa 1

coeff <- standardizedSolution(group1_fit)

# wagi
fnd_1_w <- coeff$est.std[1:3]
lib_1_w <- coeff$est.std[8:12]
econ_1_w <- coeff$est.std[4:7]

gr1_stats <- group1_data %>% mutate(fnd = (obeyRulers*fnd_1_w[1]+religiousLaw*fnd_1_w[2]+ 
                                             armyTakesOver*fnd_1_w[3])/sum(fnd_1_w),
                                    econ = (taxRich*econ_1_w[1]+equalIncome*econ_1_w[2]+ 
                                              helpUnemp*econ_1_w[3]+obeyRulers*econ_1_w[4])/sum(econ_1_w),
                                    lib = (freeElection*lib_1_w[1]+genderEquality*lib_1_w[2]+
                                             civilRights*lib_1_w[3]+importance*lib_1_w[4]+
                                             helpUnemp*lib_1_w[5])/sum(lib_1_w)) %>%
  mutate(leading = if_else(fnd > econ, if_else(fnd > lib, "fundamentalistic", "liberal"),
                           if_else(econ > lib, "economic", "liberal")))

gr1_stats_summary <- gr1_stats %>% summarise(fnd_mean = mean(fnd), 
                                             econ_mean = mean(econ),
                                             lib_mean = mean(lib))


bxp <- gr1_stats %>% select(fnd, econ, lib) %>% gather(key="concept", value="value") %>%
  ggboxplot(x = "concept", y = "value",
            color = "concept", palette = c("blue", "yellow", "grey"))

levels <- c("fundamentalistic", "economic", "liberal")
gr1_stats$leading <- factor(gr1_stats$leading, levels)

bar <- gr1_stats %>% select(leading) %>% 
  ggplot() + geom_bar(aes(leading, fill = "leading"), fill=c("blue", "yellow", "grey")) + 
  scale_x_discrete(drop=F)


yplot <- gr1_stats %>% select(fnd, econ, lib) %>% gather(key="concept", value="value") %>%
  ggdensity("value", fill = "concept", palette = c("yellow", "blue", "grey")) +
  rotate() + clean_theme()

ggarrange(bxp, yplot, bar, NULL,
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(2, 0.7),
          common.legend = TRUE)


# grupa 3

coeff <- standardizedSolution(group3_fit)

# wagi
fnd_3_w <- coeff$est.std[1:3]
lib_econ_3_w <- coeff$est.std[4:11]


gr3_stats <- group3_data %>% mutate(fnd = (obeyRulers*fnd_3_w[1]+religiousLaw*fnd_3_w[2]+ 
                                    armyTakesOver*fnd_3_w[3])/sum(fnd_3_w),
                                    lib_econ = (taxRich*lib_econ_3_w[1]+equalIncome*lib_econ_3_w[2]+ 
                                    helpUnemp*lib_econ_3_w[3]+freeElection*lib_econ_3_w[4]+
                                    genderEquality*lib_econ_3_w[5]+civilRights*lib_econ_3_w[6]+
                                    importance*lib_econ_3_w[7]+obeyRulers*lib_econ_3_w[8])/sum(lib_econ_3_w)) %>%
  mutate(leading = if_else( (fnd >= lib_econ), "fundamentalistic","economic-liberal"))

gr3_stats_summary <- gr3_stats %>% summarise(fnd_mean = mean(fnd), 
                                             lib_econ_mean = mean(lib_econ))

bxp <- gr3_stats %>% select(fnd, lib_econ) %>% gather(key="concept", value="value") %>%
  ggboxplot(x = "concept", y = "value",
            color = "concept", palette = c("blue","grey"))

levels <- c("fundamentalistic", "economic-liberal")
gr3_stats$leading <- factor(gr3_stats$leading, levels)

bar <- gr3_stats %>% select(leading) %>% 
  ggplot() + geom_bar(aes(leading, fill = "leading"), fill=c("blue","grey")) + 
  scale_x_discrete(drop=F)


yplot <- gr3_stats %>% select(fnd, lib_econ) %>% gather(key="concept", value="value") %>%
  ggdensity("value", fill = "concept", palette = c("blue", "grey")) +
  rotate() + clean_theme()

ggarrange(bxp, yplot, bar, NULL,
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(2, 0.7),
          common.legend = TRUE)

