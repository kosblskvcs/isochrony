library(tidyverse)
library(rticulate)
library(rcartocolor)

# First, read all Fid1 exports into a single tibble
folder_path <- "C:/Users/DELL/OneDrive - University of Edinburgh/dissertation/isochrony_data/export_isochrony_dlc"
## participants were exported at multiple times into different documents
number <- c(paste0("0",1:9),"10","11","12","13-16","17","18-22","23")
file_path <- paste0(folder_path, "/dlc-data-", number[1], ".tsv")

## create tibble for p1 by reading his data
dlc_data <- read_aaa(file_path)
## append every other participant to the existing tibble
for (i in 2:length(number)) {
  file_path <- paste0(folder_path, "/dlc-data-", number[i], ".tsv")
  dlc_data <- add_row(dlc_data, read_aaa(file_path))
}
unique(dlc_data$`Client family name`)
str(dlc_data)
dlc_data[["Client family name"]] <- str_replace_all(dlc_data[["Client family name"]], "test", "en01")


## Twice-aligned
number_second <- c("13","15","17","22","23")
file_path_second <- paste0(folder_path, "/dlc-data-", paste0(number_second, collapse = "-"), ".tsv")
dlc_data_second <- read_aaa(file_path_second)

second_13 <- "03/06/2025 12:30:21"
second_15 <- "10/06/2025 15:03:35"
second_17 <- "11/06/2025 10:32:05"
second_22 <- "18/06/2025 13:45:51"
second_23 <- "18/06/2025 15:56:45"
realign <- tibble(client = paste0("en",number_second), start_realign_time = c(second_13, second_15, second_17, second_22, second_23), start_realign_row = rep(NA,5), end_realign = rep(NA,5))

for (i in 1:length(number_second)) {
  realign[[i,3]] <- str_which(dlc_data_second[["Date Time of recording"]],realign[[i,2]])[1]
  dlc_data_second |>
    filter(`Client family name`== realign[[i,1]]) |>
    nrow() -> realign[[i,"end_realign"]]
}

realign <- realign |>
  mutate(
    end_realign_row = cumsum(end_realign),
    start_realign = start_realign_row - lag(end_realign_row))
realign[[1,6]] <- realign[[1,3]]
realign <- realign |>
  relocate(end_realign, .after = last_col())
realign

realign[[5,"displ_shift"]] <- unique(dlc_data |> filter(`Client family name` == realign[[5,1]]) |> select(displ_id))[[1]][1]
realign[[5,"displ_shift_back"]] <- unique(dlc_data_second |> filter(`Client family name` == realign[[5,1]]) |> select(displ_id))[[1]][1]
realign[[5,"frame_shift"]] <- unique(dlc_data |> filter(`Client family name` == realign[[5,1]]) |> select(frame_id))[[1]][1]
realign[[5,"frame_shift_back"]] <- unique(dlc_data_second |> filter(`Client family name` == realign[[5,1]]) |> select(frame_id))[[1]][1]
dlc_data_second_2 <- dlc_data_second |> filter(`Client family name` == realign[[1,"client"]])
for (i in 2:length(number_second)) {
  dlc_data_second |>
    filter(`Client family name`== realign[[i,"client"]]) |>
    mutate(
      displ_id = displ_id + realign[[i,"displ_shift"]] - realign[[i,"displ_shift_back"]],
      frame_id = frame_id + realign[[i,"frame_shift"]] - realign[[i,"frame_shift_back"]]) -> a
  dlc_data_second_2 <- add_row(dlc_data_second_2, a)
}

for (i in 1:length(number_second)) {
  dlc_data <- rows_update(dlc_data,
                          dlc_data_second_2 |>
                            filter(`Client family name` == realign[[i,"client"]]) |>
                            slice(realign[[i,"start_realign"]]:realign[[i,"end_realign"]]),
                          by = c("frame_id","displ_id","Client family name"))
}

## Some more manipulation
dlc_data |> 
  # Let's rename some columns.
  rename(
    client = `Client family name`,
    date_time = `Date Time of recording`,
    time_rec = `Time of sample in recording`,
    time_annot = `Time of sample in annot`
  ) |>
  mutate(
    knot = knot + 1,
    word = str_sub(Prompt,11,-16)
  ) -> dlc_data
write_rds(dlc_data, paste0(folder_path,"/dlc_data.rds"))
