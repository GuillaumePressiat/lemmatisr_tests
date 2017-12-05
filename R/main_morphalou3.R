library(tidyverse)


# Mise en forme adéquate du fichier csv Morphalou3.1 pour des traitements de type tidytext (dplyr, etc.)
morphalou <- readr::read_csv2('sources/Morphalou3.1/Morphalou3.1_CSV.csv',
                              skip = 15)

saveRDS(morphalou, 'sources/Morphalou3.1/morphalou3.1.Rds')

library(tidyr)
morphalou_ <- morphalou %>% 
  fill(GRAPHIE, ID, `CATÉGORIE`,
       LOCUTION, GENRE,
       `PHONÉTIQUE`) %>% 
  rename(forme_lemmatise = GRAPHIE, forme_flechie = GRAPHIE_1) %>% 
  mutate(dictionnaire = tolower(`CATÉGORIE`))

readr::write_csv(morphalou_, 'results/tidy_morphalou3.1.csv.gz')
# readr::write_rds(morphalou_, 'results/tidy_morphalou3.1.Rds')

