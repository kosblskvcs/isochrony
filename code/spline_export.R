library(tidyverse)
library(rticulate)
library(rcartocolor)

folder_path <- "C:/Users/pplssupport/OneDrive - University of Edinburgh/dissertation/isochrony_data/exported_isochrony"
#folder_path <- "C:/Users/DELL/OneDrive - University of Edinburgh/dissertation/isochrony_data/exported_isochrony"
file_path <- paste0(folder_path, "/dlc-data-09-02.tsv")
# dlc <- read_aaa(file_path, format = "wide")
# dlc_09 <- dlc
# dlc <- read_aaa(file_path)
# dlc |> 
#   ggplot(aes(X, Y)) +
#   geom_point(alpha = 0.1, size = 0.5) +
#   coord_fixed()
# dlc |> 
#   filter(spline == "DLC_Tongue") |> 
#   ggplot(aes(X, Y, group = frame_id)) +
#   geom_path(alpha = 0.1, linewidth = 0.1) +
#   coord_fixed()
# dlc |> 
#   filter(
#     `Date Time of recording` == "27/05/2025 11:15:29",
#     spline == "DLC_Tongue"
#   ) |> 
#   ggplot(aes(`Time of sample in annot`, Y, group = displ_id)) +
#   geom_path() +
#   facet_grid(rows = vars(knot), scales = "free_y")
# 
# unique(dlc$`Date Time of recording`)


en09_dlc_path <- file_path
en09_dlc <- read_aaa(en09_dlc_path) |> 
  # Let's rename some columns.
  rename(
    date_time = `Date Time of recording`,
    time_rec = `Time of sample in recording`,
    time_annot = `Time of sample in annot`
  ) |>
  mutate(
    knot = knot + 1,
    word = str_sub(Prompt,11,-16)
  )
vowel_start_end <- read_csv("C:/Users/pplssupport/OneDrive - University of Edinburgh/dissertation/isochrony_data/export_isochrony_09/vowel_start_end.csv")
word_segments <- read_csv("C:/Users/pplssupport/OneDrive - University of Edinburgh/dissertation/isochrony_code/stimuli/isochrony_wlist_vowels.csv")

en09_tongue <- en09_dlc |> 
  filter(spline == "DLC_Tongue")

## sampling rate
en09_dlc |>
  group_by(displ_id) |>
  mutate(dt = time_annot - lag(time_annot)) |>
  summarise(mean_dt = mean(dt, na.rm = TRUE)) |>
  summarise(mean_fs = round(mean(1/mean_dt)))

en09_tongue |>
#  filter(V1 == "a") |>
  ggplot(aes(X, Y, colour = as.factor(knot))) +
  geom_point(alpha = 0.1)

# en09_tongue |> 
#   filter(knot == "4") |> 
#   ggplot(aes(time_annot, Y, group = date_time)) +
#   annotate("rect",
#            ymin = -Inf, ymax = Inf, xmin = 0.3, xmax = 0.5,
#            alpha = 0.25, fill = "purple") +
#   geom_line()

# sg_plot <- en09_tongue |> 
#   filter(displ_id == 9) |> 
#   ggplot(aes(time_annot, Y)) +
#   geom_line(linetype = "dashed")
# 
# for (wl in c(5, 7, 9, 11, 15)) {
#   sg_plot <- sg_plot +
#     geom_line(
#       aes(
#         y = filter_signal(Y, order = 2, window_length = {{ wl }}, apply = 2),
#         colour = as.factor({{ wl }})
#       ), linewidth = 1.5, alpha = 0.7
#     )
# }
# 
# sg_plot + scale_colour_carto_d(palette = "Safe", name = "Window\nlength")

en09_tongue <- en09_tongue |> 
  mutate(
    Y_sm = filter_signal(Y, order = 2, window_length = 7, apply = 2),
    .by = displ_id
  )

en09_tongue |> select(time_annot, Y, Y_sm)

en09_tongue |> 
  filter(knot == "5") |> 
  ggplot(aes(time_annot, Y, group = date_time)) +
  geom_line(alpha = 0.05) +
  geom_line(aes(y = Y_sm), alpha = 0.05, colour = "purple")

set.seed(13579)
kdi <- sample(unique(en09_tongue$displ_id), 9)

en09_tongue |> 
  filter(displ_id %in% kdi) |> 
  ggplot(aes(time_annot, Y)) +
  geom_line() +
  geom_point(aes(y = Y_sm), colour = "red") +
  facet_wrap(vars(displ_id), scales = "free")

# ### Butterworth
# 
# en09_tongue <- en09_tongue |> 
#   mutate(
#     Y_smb = filter_signal(Y, filter = "butter", cutoff_freq = 10, sampling_freq = 67),
#     .by = displ_id
#   )
# 
# en09_tongue |> select(time_annot, Y, Y_smb)
# en09_tongue |> 
#   filter(knot == "4") |> 
#   ggplot(aes(time_annot, Y, group = date_time)) +
#   geom_line(alpha = 0.25) +
#   geom_line(aes(y = Y_smb), colour = "orange")
# 
# #### Compare Butterworth and Savitzky--Golay
# en09_tongue |> 
#   filter(knot == "4") |> 
#   ggplot(aes(time_annot, Y, group = date_time)) +
#   geom_point(aes(y = Y_sm), pch = 15, size = 2, colour = "purple") +
#   geom_point(aes(y = Y_smb), pch = 18, size = 1, colour = "white") +
#   facet_wrap(vars(displ_id))

## Resampling

en09_tongue_up <- en09_tongue |> 
  reframe(
    resample_signal(Y_sm, time_annot, by = 3),
    .by = displ_id
  )

### Nothing exceptional here, just doing some wrangling to get the info back
en09_tongue_up <- en09_tongue_up |> 
  left_join(y = en09_tongue |> select(Prompt, spline, knot, displ_id,) |> distinct()) |> 
  rename(Y_sm_up = signal_int)

en09_tongue_up
en09_tongue_up |> 
  filter(knot == 5) |> 
  ggplot(aes(time_int, Y_sm_up, group = displ_id)) +
  geom_point(alpha = 0.2, color = "blue") +
  geom_line(alpha = 0.2, color = "blue")

en09_tongue_up <- en09_tongue_up |> 
  mutate(
    Y_sm_up_sm = filter_signal(Y_sm_up, order = 2, window_length = 7, apply = 2),
    .by = displ_id
  )

en09_tongue_up |> 
  filter(knot == 5, displ_id == 9) |> 
  ggplot(aes(time_int, Y_sm_up_sm, group = displ_id)) +
  geom_point(alpha = 0.25) +
  geom_line()

# Kinematics

en09_segment_interval <- left_join(vowel_start_end, en09_dlc, by = join_by(date_time == date_time)) |>
  filter(spline == "DLC_Tongue")
en09_segment_interval_between_peaks <- en09_segment_interval |>
  filter(time_annot > start_of_vowel_within_annot & time_annot < end_of_vowel_within_annot)

en09_up <- en09_dlc |> 
  filter(spline == "DLC_Tongue") |> 
  mutate(
    Y_sm = filter_signal(Y, order = 2, window_length = 7, apply = 2),
    .by = c(displ_id)
  ) |> 
  reframe(
    resample_signal(Y_sm, time_annot, by = 3), .by = c(displ_id, knot)
  ) |> 
  mutate(
    Y_sm_up = filter_signal(signal_int, window_length = 11, apply = 2),
    .by = c(displ_id, knot)
  ) |>
  left_join(y = en09_dlc[c(Prompt,)], by = join_by(displ_id, knot))
en09_up <- en09_up |>
  mutate(knot = knot + 1)

en09_up |>
  filter(displ_id > 1024 & displ_id < 1036) |>
  ggplot(aes(time_int, Y_sm_up)) +
  geom_point(alpha = 0.2) +
  facet_grid(rows = vars(knot), scales = "free_y")

en09_up <- en09_up |> 
  mutate(
    Y_sm_up_vel = get_velocity(Y_sm_up)
  )

start_09_39 <- as.numeric(vowel_start_end[7,6])
end_09_39 <- as.numeric(vowel_start_end[7,7])
en09_dlc |> 
  filter(displ_id == 39) |>
  select(Prompt)
en09_up |> 
  filter(displ_id == 39) |> 
  ggplot(aes(time_int, Y_sm_up_vel)) +
  geom_point() +
  geom_line(aes(y = abs(Y_sm_up_vel)), colour = "red") +
  annotate("rect", xmin = start_09_39, xmax = end_09_39, ymin = -Inf, ymax = Inf, alpha = 0.3)

# Filter signal to include a reduced time window
en09_up_win <- en09_up |>
  filter(displ_id == 39, time_int > start_09_39, time_int < end_09_39)

# Get landmarks
en09_up_win_land <- get_landmarks(en09_up_win$Y_sm_up_vel, en09_up_win$time_int, start_09_39, end_09_39)
en09_up_win_land_long <- en09_up_win_land |> 
  pivot_longer(everything(), names_to = "land")

en09_up_win_land_long

en09_up_win |> 
  ggplot(aes(time_int, abs(get_velocity(Y_sm_up)))) +
  geom_point(colour = "red") +
  geom_vline(data = en09_up_win_land_long |> filter(str_detect(land, "_ons|_off")), aes(xintercept = value)) +
  labs(y = "Absolute velocity")

en09_up_win |> 
  ggplot(aes(time_int, Y_sm_up)) +
  geom_point() +
  geom_vline(data = en09_up_win_land_long |> filter(str_detect(land, "_ons|_off")), aes(xintercept = value)) +
  labs(y = "Y displacement")

en09_up_2 <- left_join(en09_up, en09_segment_interval_between_peaks, by = join_by(displ_id == displ_id & time_int == time_annot))
en09_up_lands <- en09_tongue_up |> 
    filter(knot == 5, time_int > start_of_vowel_within_annot_1, time_int < end_of_vowel_within_annot_1) |> 
    reframe(
      get_landmarks(Y_sm_up_vel, time_int, 0.35, 0.45),
      .by = displ_id
    )
  
filter(any(PAN=="YES") & any(PAN=="NO"), .by=ID)
reframe(
    filter(),
    .by = displ_id
  ) |>

en09_up_lands

en09_up_lands_long <- en09_up_lands |> 
  pivot_longer(-displ_id, names_to = "land")

en09_up |> 
  filter(knot == 8) |> 
  ggplot(aes(time_int, Y_sm_up)) +
  geom_point(alpha = 0.2) +
  geom_vline(data = en09_up_lands_long |> filter(str_detect(land, "_ons|_off")), aes(xintercept = value)) +
  facet_wrap(vars(displ_id))





