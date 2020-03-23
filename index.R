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
            mutate(is_fnd = if_else( (fnd > econ) && (fnd > lib), 1, 0),
                   is_econ = if_else( (econ > fnd) && (econ > lib), 1, 0),
                   is_lib = if_else( (lib > econ) && (lib > fnd), 1, 0)) %>%
            summarise(fnd_mean = mean(fnd), 
                      econ_mean = mean(econ),
                      lib_mean = mean(lib),
                      fnd_count = sum(is_fnd),
                      econ_count = sum(is_econ),
                      lib_count = sum(is_lib))
