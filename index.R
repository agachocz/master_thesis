install.packages("ggpubr")
library(ggpubr)
install.packages("ggsci")
library(ggsci)

# obliczenie wskaźników demokracji w różnych koncepcjach dla modeli

# grupa ogólna

coeff <- summary_fit$PE

# wagi
fnd_all_w <- coeff$std.all[1:4]
lib_all_w <- coeff$std.all[8:12]
econ_all_w <- coeff$std.all[5:7]

all_stats <- dem_data %>% mutate(fnd = (obeyRulers*fnd_all_w[1]+religiousLaw*fnd_all_w[2]+ 
                    armyTakesOver*fnd_all_w[3]+equalIncome*fnd_all_w[4])/sum(fnd_all_w),
                    econ = (taxRich*econ_all_w[1]+equalIncome*econ_all_w[2]+ 
                    helpUnemp*econ_all_w[3])/sum(econ_all_w),
                    lib = (freeElection*lib_all_w[1]+genderEquality*lib_all_w[2]+
                    civilRights*lib_all_w[3]+importance*lib_all_w[4]+
                      helpUnemp*lib_all_w[5])/sum(lib_all_w)) %>%
            mutate(leading = if_else(fnd > econ, if_else(fnd > lib, "fundamentalistic", "liberal"),
                             if_else(econ > lib, "economic", "liberal")))


all_stats_summary <- all_stats %>% summarise(fnd_mean = mean(fnd), 
                      econ_mean = mean(econ),
                      lib_mean = mean(lib),
                      fnd_count = sum(is_fnd),
                      econ_count = sum(is_econ),
                      lib_count = sum(is_lib))


bxp <- all_stats %>% select(fnd, econ, lib) %>% gather(key="concept", value="value") %>%
        ggboxplot(x = "concept", y = "value",
                 color = "concept", palette = c("blue", "yellow", "grey"))

levels <- c("fundamentalistic", "economic", "liberal")
all_stats$leading <- factor(all_stats$leading, levels)

bar <- all_stats %>% select(leading) %>% 
              ggplot() + geom_bar(aes(leading, fill = "leading"), fill=c("blue", "yellow", "grey")) + 
              scale_x_discrete(drop=F)
        

yplot <- all_stats %>% select(fnd, econ, lib) %>% gather(key="concept", value="value") %>%
          ggdensity("value", fill = "concept", palette = c("yellow", "blue", "grey")) +
          rotate() + clean_theme()

ggarrange(bxp, yplot, bar, NULL,
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(2, 0.7),
          common.legend = TRUE)


# demokracje liberalne (grupa 2)

coeff <- group2_summary$PE

# wagi
fnd_2_w <- coeff$std.all[1:3]
lib_2_w <- coeff$std.all[7:11]
econ_2_w <- coeff$std.all[4:6]

gr2_stats <- group2_data %>% mutate(fnd = (obeyRulers*fnd_2_w[1]+religiousLaw*fnd_2_w[2]+ 
                             armyTakesOver*fnd_2_w[3])/sum(fnd_2_w),
                             econ = (taxRich*econ_2_w[1]+equalIncome*econ_2_w[2]+ 
                                           helpUnemp*econ_2_w[3])/sum(econ_2_w),
                                 lib = (freeElection*lib_2_w[1]+genderEquality*lib_2_w[2]+
                                          civilRights*lib_2_w[3]+importance*lib_2_w[4]+
                                          helpUnemp*lib_2_w[5])/sum(lib_2_w)) %>%
  mutate(leading = if_else(fnd > econ, if_else(fnd > lib, "fundamentalistic", "liberal"),
                           if_else(econ > lib, "economic", "liberal")))


bxp <- gr2_stats %>% select(fnd, econ, lib) %>% gather(key="concept", value="value") %>%
  ggboxplot(x = "concept", y = "value",
            color = "concept", palette = c("blue", "yellow", "grey"))

levels <- c("fundamentalistic", "economic", "liberal")
gr2_stats$leading <- factor(gr2_stats$leading, levels)

bar <- gr2_stats %>% select(leading) %>% 
  ggplot() + geom_bar(aes(leading, fill = "leading"), fill= c("blue", "yellow", "grey")) + 
  scale_x_discrete(drop=F)


yplot <- gr2_stats %>% select(fnd, econ, lib) %>% gather(key="concept", value="value") %>%
  ggdensity("value", fill = "concept", palette = c("yellow", "blue", "grey")) +
  rotate() + clean_theme()

ggarrange(bxp, yplot, bar, NULL,
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(2, 0.7),
          common.legend = TRUE)



# grupa 1

coeff <- group1_summary$PE

# wagi
fnd_1_w <- coeff$std.all[1:3]
lib_1_w <- coeff$std.all[8:12]
econ_1_w <- coeff$std.all[4:7]

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

coeff <- group3_summary$PE

# wagi
fnd_3_w <- coeff$std.all[1:3]
lib_econ_3_w <- coeff$std.all[4:11]


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

