library(tidyverse)
load(file='mrc_db.rda')
mrc_db <- mrc_db |>
  mutate(word = str_to_lower(word))

#newdic <- read_tsv("../newdic.txt", col_names = F)
#newdic
#colnames(newdic) <- c("word", "stress_pattern", "syll_count", "orthography", "freq", "POS")

#mrc_db[11:20,14:29]
#colnames(mrc_db)
#str(mrc_db)
#mrc_db[1:100,]$stress
#unique(mrc_db$stress)

newdic |>
  filter(stress_pattern == "_'_") |>
  slice(str_which(word, "^.?.?.?Yt"))

mrc_db |>
  filter(stress == "020" | stress == "120" | stress == "210") |>
  slice(str_which(phon, "^.?.?.?aI/p")) |>
  select(14:26)
mrc_db |>
  filter(stress == "200" | stress == "201" | stress == "120") |>
  slice(str_which(phon, "^.?.?.?eI/p")) |>
  select(14:26)

mrc_db |>
  filter(stress == "20" | stress == "1120") |>
  slice(str_which(word, "quash")) |>
  select(14:26) |>
  print(n=20)

vowels2 <- c("A", "&", "e", "I", "i", "@", "3", "u", "U", "V", "O")
vowels <- c("A", "&", "e", "I", "i", "@", "3", "u", "U", "V", "O", "0")
consonants <- c("p", "b", "t", "d", "k", "g")

for (i in 1:length(vowels)) {
  l[[i]] <- list()
}

for (i in 1:length(vowels)) {
  for (j in 1:length(consonants)) {
    mrc_db |>
      filter(stress == "1020") |>
      slice(str_which(phon, paste0("^[^/]+", vowels[i], "/", consonants[j]))) |>
      select(c(23, 24, 26)) -> l[[i]][[j]]
  }
}

#######################################################################
l1020_10 <- list()
for (i in 1:length(vowels)) {
  l1020_10[[i]] <- list()
}
names(l1020_10) <- vowels


for (i in 1:length(vowels)) {
  for (j in 1:length(consonants)) {
    mrc_db |>
      filter(stress == "1020" | stress == "102") |>
      slice(str_which(phon, paste0("^[^/]*", vowels[i], "/", consonants[j], "I"))) |>
      select(c(23, 24, 26)) -> l1020_10[[i]][[j]]
  }
  names(l1020_10[[i]]) <- consonants
}
l1020_10

#######################################################################
l0200_02 <- list()
for (i in 1:length(vowels)) {
  l0200_02[[i]] <- list()
}
names(l0200_02) <- vowels

for (i in 1:length(vowels)) {
  for (j in 1:length(consonants)) {
    mrc_db |>
      filter(stress == "0200") |>
      slice(str_which(phon, paste0("^([a-z]|[A-Z]|@|3|&)*", vowels[i], "/", consonants[j], "r?&"))) |>
      select(c(23, 24, 26)) -> l0200_02[[i]][[j]]
  }
  names(l0200_02[[i]]) <- consonants
}
l0200_02

#######################################################################
l020_20 <- list()
for (i in 1:length(vowels)) {
  l020_20[[i]] <- list()
}
names(l020_20) <- vowels

for (i in 1:length(vowels)) {
  for (j in 1:length(consonants)) {
    mrc_db |>
      filter(stress == "020") |>
      slice(str_which(phon, paste0("^([a-z]|[A-Z]|@|3|&)+/[qwrtzpsdfghjklyxcvbm]*&", "/", consonants[j], vowels[i]))) |>
      select(c(23, 24, 26)) -> l020_20[[i]][[j]]
  }
  names(l020_20[[i]]) <- consonants
}
l020_20

#######################################################################
l020_02 <- list()
for (i in 1:length(vowels)) {
  l020_02[[i]] <- list()
}
names(l020_02) <- vowels

for (i in 1:length(vowels)) {
  for (j in 1:length(consonants)) {
    mrc_db |>
      filter(stress == "020") |>
      slice(str_which(phon, paste0("^([a-z]|[A-Z]|@|3|&)*", vowels[i], "/", consonants[j], "I"))) |>
      select(c(23, 24, 26)) -> l020_02[[i]][[j]]
  }
  names(l020_02[[i]]) <- consonants
}
l020_02

#######################################################################
l200_20 <- list()
for (i in 1:length(vowels)) {
  l200_20[[i]] <- list()
}
names(l200_20) <- vowels

for (i in 1:length(vowels)) {
  for (j in 1:length(consonants)) {
    mrc_db |>
      filter(stress == "200" | stress == "201") |>
      slice(str_which(phon, paste0("^[qwrtzpsdfghjklyxcvbnm]*", vowels[i], "/", consonants[j], "r?[iI]"))) |>
      select(c(23, 24, 26)) -> l200_20[[i]][[j]]
  }
  names(l200_20[[i]]) <- consonants
}
l200_20

#######################################################################
l200_00 <- list()
for (i in 1:length(vowels)) {
  l200_00[[i]] <- list()
}
names(l200_00) <- vowels

for (i in 1:length(vowels)) {
  for (j in 1:length(consonants)) {
    mrc_db |>
      filter(stress == "200") |>
      slice(str_which(phon, paste0("^([a-z]|[A-Z]|@|3|&)+/([a-z]|[A-Z]|@|3|&)*", vowels[i], "/", consonants[j], "r@[^U]*"))) |>
      select(c(23, 24, 26)) -> l200_00[[i]][[j]]
  }
  names(l200_00[[i]]) <- consonants
}
l200_00

#######################################################################
l20_20 <- list()
for (i in 1:length(vowels)) {
  l20_20[[i]] <- list()
}
names(l20_20) <- vowels

for (i in 1:length(vowels)) {
  for (j in 1:length(consonants)) {
    mrc_db |>
      filter(stress == "20" | stress == "201") |>
      slice(str_which(phon, paste0("^([a-z]|[A-Z]|@|3|&)*", vowels[i], "/", consonants[j], "r"))) |>
      select(c(23, 24, 26)) -> l20_20[[i]][[j]]
  }
  names(l20_20[[i]]) <- consonants
}
l20_20

#######################################################################
l02_02 <- list()
for (i in 1:length(vowels)) {
  l02_02[[i]] <- list()
}
names(l02_02) <- vowels

for (i in 1:length(vowels)) {
  for (j in 1:length(consonants)) {
    mrc_db |>
      filter(stress == "02") |>
      slice(str_which(phon, paste0("^([a-z]|[A-Z]|@|3|&)*", vowels[i], "/", consonants[j], "r?&"))) |>
      select(c(23, 24, 26)) -> l02_02[[i]][[j]]
  }
  names(l02_02[[i]]) <- consonants
}
l02_02

mrc_db |>
  slice(str_which(phon, "0")) |>
  select(23:26)

l1020_10
l1020_02
l020_20
l020_02
l200_200
l200_00
l20_20
l02_02


wl <- read_csv("C:/Users/pplssupport/OneDrive - University of Edinburgh/dissertation/isochrony_code/stimuli/isochrony_wlist_stress.csv", col_names = F)
colnames(wl) <- c("word","stress","V1","V2","x")
consonants <- c("p","b","t","d","k","g")
C2 <- c(rep(consonants,14),c("g","p","d","g","p","p","k","b","p","g","b"))
R <- c(rep(rep(c(0,1),each = 6),7),c(0,0,0,0,1,2,3,0,0,0,0))
C1 <- rep(0,95)
C3 <- rep("",95)
write_csv(cbind(wl[1:4],C2,R,C1,C3),"C:/Users/pplssupport/OneDrive - University of Edinburgh/dissertation/isochrony_code/stimuli/isochrony_wlist_vowels.csv")
wl <- read_csv("C:/Users/pplssupport/OneDrive - University of Edinburgh/dissertation/isochrony_code/stimuli/isochrony_wlist_vowels.csv")
wl <- wl |>
  mutate(closed_syl2 = rep(0,95),
         syl_count = rep(NA,95))
write_csv(wl,"C:/Users/pplssupport/OneDrive - University of Edinburgh/dissertation/isochrony_code/stimuli/isochrony_wlist_vowels.csv")
