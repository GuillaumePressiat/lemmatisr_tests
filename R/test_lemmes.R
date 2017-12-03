library(dplyr)

# test du fichier produit sur un exemple : 
sejours_opt <- readxl::read_excel('~/Desktop/sejopt2016.xlsx')

a <- tempfile()
readr::write_tsv(sejours_opt, a)

readr::read_tsv(a) %>% 
  rename(cle_rsa = NUM_RSA,
         nohop = hop,
         commentaire = COMMENTAIRES,
         validation = `VALIDATION DIM`) -> sejours_opt


library(tidytext)
sejours_opt <- sejours_opt %>% select(nohop, cle_rsa, commentaire, validation) %>%
  mutate(
    commentaire_ = stringr::str_replace_all(commentaire, "\\+|\\.|\\'|'", " "),
    commentaire_ = stringr::str_replace_all(commentaire_, "\\-", ""),
    commentaire_ = stringi::stri_trans_general(commentaire_,"Latin-ASCII"))

stop_words <- c('de', 'du', 'avec', 'des', 'en', 'kg', 'moins', 
                'par', 'et', 'un', 'une', 'le', 'la', 'cc', 'pour',
                'sur', 'sous', 'au', 'a', 'd', 'l', 'aux', 'au')

# on découpe le texte mot par mot
sejours_opt %>% 
  unnest_tokens(word1, commentaire_, token = "ngrams", n = 1)  %>% 
  # on enlève les "stop words"
  filter(! word1 %in% stop_words) %>% 
  # toutes les unités / les actes ccam, les codes cim et les chiffres
  filter(! stringr::str_detect(word1, '^[a-zA-Z]{1}\\b')) %>% 
  filter(! stringr::str_detect(word1, '^[a-zA-Z]{4}[0-9]{3}\\b')) %>% 
  filter(! stringr::str_detect(word1, '^[a-zA-Z]{1}[0-9]{2}.*\\b')) %>% 
  filter(! stringr::str_detect(word1, '^[0-9]+.*\\b')) %>% 
  filter(! stringr::str_detect(word1, '.*(cc|kg)\\b')) %>% 
  filter(! stringr::str_detect(word1, '.*(fa|ac)\\b')) -> test1

# lecture du fichier produit à partir du xml Morphalou
test <- readr::read_csv('~/Github/lemmatisR/results/tidy_morphalou.csv')
# Si le mot présente des formes fléchies, forme fléchies, sinon on place l'unique forme
# lemmatisée dans la forme fléchie
test <- test %>% 
  mutate(forme_flechie = case_when(
    is.na(forme_flechie) ~ forme_lemmatise,
    !is.na(forme_flechie) ~ forme_flechie
))

# nombre de caractères de la forme lemmatisé, on enlève les accents que l'on laissera sur la forme lemmatisée
test2 <- test %>% 
  mutate(nc = nchar(forme_lemmatise)) %>% 
  mutate(forme_flechie = stringi::stri_trans_general(forme_flechie,"Latin-ASCII"))

# Première jointure test
# avec les verbes
test1 %>% 
  mutate(nc0 = nchar(word1),
         rn = row_number()) %>% 
  left_join(test2, 
            by = c('word1' = 'forme_flechie')) %>% 
  # Distance entre le mot orginal et la forme lemmatise
  mutate(diff = stringdist::stringsim(word1, forme_lemmatise)) %>% 
  arrange(rn, word1, desc(diff)) %>% 
  distinct(rn, .keep_all = T)  -> test3

# Deuxième jointure test
# sans les verbes
test1 %>% 
  mutate(nc0 = nchar(word1),
         rn = row_number()) %>% 
  left_join(test2 %>% filter(!grepl('verb', dictionnaire)), 
            by = c('word1' = 'forme_flechie')) %>% 
  # Distance entre le mot orginal et la forme lemmatise
  mutate(diff = stringdist::stringsim(word1, forme_lemmatise)) %>% 
  arrange(rn, word1, desc(diff)) %>% 
  distinct(rn, .keep_all = T)  -> test3

test3 %>% 
  filter(is.na(forme_lemmatise)) %>% 
  count(word1, sort = T) %>% View


test3 %>% 
  filter(! word1 %in% stop_words) %>% 
  mutate(mot = case_when(
    is.na(forme_lemmatise) ~ word1, 
    ! is.na(forme_lemmatise) ~ forme_lemmatise
  ))  -> resu

resu %>% 
  count(mot) %>% 
  mutate(p = n / sum(n)) -> unigram_probs

unigram_probs %>%
  # filter(n > 5) %>%
  (function(df){
    wordcloud::wordcloud(words = df$mot, freq = df$p, min.freq = 0.0033, rot.per = 0)})

count(resu %>% filter(! is.na(forme_lemmatise)), mot, word1, forme_lemmatise, sort = T) -> verifs




balzac <- tibble(texte = readr::read_lines(
  # La maison du chat qui pelotte
  # 'http://www.gutenberg.org/cache/epub/24217/pg24217.txt',
  # Eugénie Grandet
  # 'http://www.gutenberg.org/cache/epub/11049/pg11049.txt'
  # tome 1
  'https://www.gutenberg.org/files/41211/41211-8.txt'
))

balzac <- balzac %>% 
  pull(texte) %>% 
  paste0(collapse = " ") %>% 
  stringr::str_split('START OF THIS PROJECT GUTENBERG') %>% 
  purrr::flatten_chr() %>% 
  .[2] %>% 
  stringr::str_split('End of the Project Gutenberg') %>% 
  purrr::flatten_chr() %>% 
  .[1] %>% 
  tibble(texte = .)

library(tidytext)

balzac <- balzac %>% 
  mutate(
    texte = stringr::str_replace_all(texte, "\\+|\\.|\\'|'", " "),
    texte = stringr::str_replace_all(texte, "\\-", "")) %>% 
  unnest_tokens(word, texte) 
  

balzac <- balzac %>% 
  ungroup() %>% 
  mutate(word = stringi::stri_trans_general(word,"Latin-ASCII"))

# Première jointure test
# avec les verbes
balzac %>% 
  mutate(nc0 = nchar(word),
         rn = row_number()) %>% 
  left_join(test2, 
            by = c('word' = 'forme_flechie')) %>% 
  # Distance entre le mot orginal et la forme lemmatise
  mutate(diff = stringdist::stringsim(word, forme_lemmatise)) %>% 
  arrange(rn, word, desc(diff)) %>% 
  distinct(rn, .keep_all = T)  -> test3


test3 %>% 
  filter(! word %in% tm::stopwords('french')) %>% 
  mutate(mot = case_when(
    is.na(forme_lemmatise) ~ word, 
    ! is.na(forme_lemmatise) ~ forme_lemmatise
  ))  -> resu

resu %>% 
  count(mot, sort = T) %>% 
  mutate(p = n / sum(n)) %>% 
  (function(df){
    wordcloud::wordcloud(words = df$mot, freq = df$p, min.freq = 0.002, rot.per = 0)})

resu %>% 
  ungroup() %>% 
  summarise(texte = paste0(mot, collapse = " ")) %>% 
  unnest_tokens(bigram, texte, token = "ngrams",  n = 2) -> test4

test4 %>% 
  tidyr::separate(bigram, c("word1", "word2"), sep = " ") -> two

bigram_counts <- count(two, word1, word2, sort = T)

bigram_counts %>% head(20)

library(igraph)
g <- bigram_counts %>% 
  filter(n > 10) %>% 
  graph_from_data_frame()

library(ggraph)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(g, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = T,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() + 
  ggtitle("test") -> p
p
