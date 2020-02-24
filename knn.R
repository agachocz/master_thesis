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

