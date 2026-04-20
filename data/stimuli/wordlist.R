library(tidyverse)
words <- read_csv("isochrony_wlist.csv", col_names = F)
words_to_sentences <- lapply(words, function(x) {paste0("You write ", x, " one more time.")})
sentences <- as_tibble(words_to_sentences)
sentences <- sentences |>
  mutate(random = runif(95, 0, 1))
sentences |>
  arrange(random) |>
  select(X1) -> sentence_list
write_csv(sentence_list, "sentences_3A_5.csv")
