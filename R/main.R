library(tidyverse)

# 
# 
# 
# morphalou <- xml2::read_xml('~/Github/lemmatisR/sources/Morphalou2/Morphalou-2.0.xml')
# 
# # u <- xml2::as_list(morphalou)
# 
# # morphalou <- XML::xmlParse('~/Github/lemmatisR/sources/Morphalou2/Morphalou-2.0.xml')
# 
# 
# # test
# lemmes <- xml2::xml_find_all(morphalou, './/lexicalEntry')
# 
# # récupérer les identifiants des lemmes
# lexical_entry <- xml_attr(lemmes, attr = "id")
# 
# library(dplyr, warn.conflicts = F)
# # récupérer les formes lémmatisées
# lemmatise <- xml_children(lemmes) %>% 
#   xml2::xml_find_all(., './/lemmatizedForm')
# 
# 
# # récupérer les formes fléchies
# flechies <- xml_children(lemmes) %>% 
#   xml2::xml_find_all(., './/inflectedForm')


# test_ <- xml2::xml_find_all(morphalou, './/lexicalEntry') %>% 
#   as_list()
# 
# saveRDS(test_, '~/Github/lemmatisR/sources/Morphalou2/morphalou.Rds')

test_ <-  readRDS('~/Github/lemmatisR/sources//Morphalou2/morphalou.Rds')

library(purrr)
test_[[1000]]$formSet$lemmatizedForm

i = 7
lemma <- function(i, append = T){
  temp <- unlist(test_[[i]]) %>% 
    as_tibble()
  temp$variable <- names(unlist(test_[[i]]))
  
  temp <- temp %>% 
    filter(variable %in% c('formSet.inflectedForm.orthography',
                           'originatingEntry',
                           'formSet.lemmatizedForm.orthography'))
  
           # ,
           # 'formSet.inflectedForm.grammaticalNumber',
           # 'formSet.inflectedForm.grammaticalGender')

  forme_lemmatise <- temp %>% 
    filter(variable == 'formSet.lemmatizedForm.orthography') %>% 
    pull(value)
  forme_flechie <- temp %>% 
    filter(variable == 'formSet.inflectedForm.orthography') %>% 
    pull(value)
  dictionnaire <- temp %>% 
    filter(variable == 'originatingEntry') %>% 
    pull(value)

  if (length(forme_flechie) == 0 ){
    forme_flechie = ""; 
    lemmes_presents = FALSE
    }
  else{
    lemmes_presents = TRUE
    }
  if (i %% 100 == 0){cat(i, ", ", sep = "")}
  #distinct(tibble(forme_lemmatise, forme_flechie))
  
  cc <- tibble(forme_lemmatise, 
                 forme_flechie,
                 dictionnaire,
               lemmes_presents, 
               id_forme_lemmatise = i) %>% 
    distinct()
  readr::write_csv(cc, 'results/tidy_morphalou.csv', append = T)
#  cc
}

lemma(7)

# file.remove('results/tidy_morphalou.csv')
readr::write_lines('forme_lemmatise,forme_flechie,dictionnaire,lemmes_presents,id_forme_lemmatise', 'results/tidy_morphalou.csv')
correspondances <- 1:length(test_) %>%
  pbapply::pblapply(lemma)

# readr::write_csv(correspondances, 'results/tidy_morphalou.csv')

correspondances <- 1:2000 %>%
  pbapply::pblapply(lemma) %>% bind_rows()




# 
# # recupérer l'orthogra
# # orthography <- xml2::xml_find_all(lemmes, './/orthography')
# 
# 
# xml_child(orthography)
# 
# library(plyr)
# 
# ldply(xml_attrs(orthography))
# 
# v <- xml2::as_list(lemmes)


u <- readRDS( '~/Downloads/Morphalou2/morphalou.Rds')
jsonlite::write_json(u[300:600],'~/Downloads/Morphalou2/morphalou.json', pretty = TRUE, flatten = T)
v <- jsonlite::fromJSON('~/Downloads/Morphalou2/morphalou.json', simplifyDataFrame = T)

