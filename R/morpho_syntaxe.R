

library(tidyverse)
library(tidytext)
# Le tour du monde en quatre-vingts jours
# book <- gutenberg_download(3456)

# récupérer le livre en latin1
book <- tibble(text = readr::read_lines('http://www.gutenberg.org/files/3456/3456-8.txt', 
                                        locale = readr::locale(encoding = "latin1")))

# du début à la FIN du livre
# book[632:9888,] %>% 
#   mutate(text = stringi::stri_trans_general(text,"Latin-ASCII"))-> book_

book[632:9888,] -> book_

par_phrases <- book_ %>% 
  summarise(texte = paste0(text, collapse = " ")) %>% 
  mutate(texte = stringr::str_replace_all(texte, '\'', ' ')) %>% 
  unnest_tokens(phrases, texte, token = "sentences") %>% 
  mutate(phrase_number = row_number())

par_phrase_mot <- par_phrases %>% 
  unnest_tokens(word, phrases,token = "words") %>% 
  #mutate(word = stringr::str_extract(word, '[a-z0-9]+'))
  mutate(word = stringr::str_replace_all(word, '_', ''))

# lecture du fichier produit à partir du csv Morphalou 3.1
test <- readr::read_csv('results/tidy_morphalou3.1.csv.gz')

# Si le mot présente des formes fléchies, forme fléchie, sinon on place l'unique forme
# lemmatisée dans la forme fléchie
test <- test %>% 
  mutate(forme_flechie = case_when(
    is.na(forme_flechie) ~ forme_lemmatise,
    !is.na(forme_flechie) ~ forme_flechie
))

# nombre de caractères de la forme lemmatisée, on enlève les accents que l'on laissera sur la forme lemmatisée
test2 <- test %>% 
  mutate(nc = nchar(forme_lemmatise)) # %>% 
  # mutate(forme_flechie = stringi::stri_trans_general(forme_flechie,"Latin-ASCII"))

par_phrase_mot %>% 
  mutate(nc0 = nchar(word),
         rn = row_number()) %>% 
  left_join(test2, 
            by = c('word' = 'forme_flechie')) %>% 
  # Distance entre le mot orginal et la forme lemmatise
  mutate(diff = stringdist::stringsim(word, forme_lemmatise)) %>% 
  arrange(rn, word, desc(diff)) %>% 
  distinct(rn, .keep_all = T)  -> words_lemmes


