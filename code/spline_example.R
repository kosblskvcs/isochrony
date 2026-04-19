library(tidyverse)
library(rticulate)
library(rcartocolor)
# system.file() is needed here because the example files reside in the package.
# You can just include the file path directly in read_aaa, like 
# read_aaa("~/Desktop/splines.tsv", columns)
file_path <- system.file("extdata", "it01-dlc.tsv", package = "rticulate")

dlc <- read_aaa(file_path, format = "wide")
dlc
dlc <- read_aaa(file_path)
dlc
dlc |> 
  ggplot(aes(X, Y)) +
  geom_point(alpha = 0.1, size = 0.5) +
  coord_fixed()
dlc |> 
  filter(spline == "DLC_Tongue") |> 
  ggplot(aes(X, Y, group = frame_id)) +
  geom_path(alpha = 0.1, linewidth = 0.1) +
  coord_fixed()
dlc |> 
  filter(
    `Date Time of recording` == "29/11/2016 15:10:52",
    spline == "DLC_Tongue"
  ) |> 
  ggplot(aes(`Time of sample in annot`, Y, group = displ_id)) +
  geom_path() +
  facet_grid(rows = vars(knot), scales = "free_y")

# signal smoothing

it01_dlc_path <- system.file("extdata/it01-dlc.tsv", package = "rticulate")
it01_dlc <- read_aaa(it01_dlc_path) |> 
  # Let's rename some columns.
  rename(
    date_time = `Date Time of recording`,
    time_rec = `Time of sample in recording`,
    time_annot = `Time of sample in annot`
  )
it01_tongue <- it01_dlc |> 
  filter(spline == "DLC_Tongue")

## smapling rate
it01_dlc |>
  group_by(displ_id) |>
  mutate(dt = time_annot - lag(time_annot)) |>
  summarise(mean_dt = mean(dt, na.rm = TRUE)) |>
  summarise(mean_fs = round(mean(1/mean_dt)))

it01_tongue |> 
  filter(knot == "8") |> 
  ggplot(aes(time_annot, Y, group = date_time)) +
  annotate("rect",
           ymin = -Inf, ymax = Inf, xmin = 0.3, xmax = 0.5,
           alpha = 0.25, fill = "purple") +
  geom_line()

sg_plot <- it01_tongue |> 
  filter(displ_id == 29) |> 
  ggplot(aes(time_annot, Y)) +
  geom_line(linetype = "dashed")

for (wl in c(5, 7, 9, 11, 15)) {
  sg_plot <- sg_plot +
    geom_line(
      aes(
        y = filter_signal(Y, order = 2, window_length = {{ wl }}, apply = 2),
        colour = as.factor({{ wl }})
      ), linewidth = 1.5, alpha = 0.7
    )
}

sg_plot + scale_colour_carto_d(palette = "Safe", name = "Window\nlength")

it01_tongue <- it01_tongue |> 
  mutate(
    Y_sm = filter_signal(Y, order = 2, window_length = 7, apply = 2),
    .by = displ_id
  )

it01_tongue |> select(time_annot, Y, Y_sm)

it01_tongue |> 
  filter(knot == "8") |> 
  ggplot(aes(time_annot, Y, group = date_time)) +
  geom_line(alpha = 0.25) +
  geom_line(aes(y = Y_sm), colour = "purple")

set.seed(13579)
kdi <- sample(unique(it01_tongue$displ_id), 9)

it01_tongue |> 
  filter(displ_id %in% kdi) |> 
  ggplot(aes(time_annot, Y)) +
  geom_line() +
  geom_point(aes(y = Y_sm), colour = "red") +
  facet_wrap(vars(displ_id), scales = "free")

### Butterworth

it01_tongue <- it01_tongue |> 
  mutate(
    Y_smb = filter_signal(Y, filter = "butter", cutoff_freq = 10, sampling_freq = 67),
    .by = displ_id
  )

it01_tongue |> select(time_annot, Y, Y_smb)
it01_tongue |> 
  filter(knot == "8") |> 
  ggplot(aes(time_annot, Y, group = date_time)) +
  geom_line(alpha = 0.25) +
  geom_line(aes(y = Y_smb), colour = "orange")

#### Compare Butterworth and Savitzky--Golay
it01_tongue |> 
  filter(knot == "8") |> 
  ggplot(aes(time_annot, Y, group = date_time)) +
  geom_point(aes(y = Y_sm), pch = 15, size = 2, colour = "purple") +
  geom_point(aes(y = Y_smb), pch = 18, size = 1, colour = "white") +
  facet_wrap(vars(displ_id))

## Resampling

it01_tongue_up <- it01_tongue |> 
  reframe(
    resample_signal(Y_sm, time_annot, by = 3),
    .by = displ_id
  )

### Nothing exceptional here, just doing some wrangling to get the info back
it01_tongue_up <- it01_tongue_up |> 
  left_join(y = it01_tongue |> select(Prompt, spline, knot, displ_id) |> distinct()) |> 
  rename(Y_sm_up = signal_int)

it01_tongue_up
it01_tongue_up |> 
  filter(knot == 8) |> 
  ggplot(aes(time_int, Y_sm_up, group = displ_id)) +
  geom_point(alpha = 0.25) +
  geom_line()

it01_tongue_up <- it01_tongue_up |> 
  mutate(
    Y_sm_up_sm = filter_signal(Y_sm_up, order = 2, window_length = 7, apply = 2),
    .by = displ_id
  )

it01_tongue_up |> 
  filter(knot == 8) |> 
  ggplot(aes(time_int, Y_sm_up_sm, group = displ_id)) +
  geom_point(alpha = 0.25) +
  geom_line()

# Kinematics

it01_up <- it01_dlc |> 
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
  )

it01_up |> 
  filter(knot == 8) |> 
  ggplot(aes(time_int, Y_sm_up)) +
  geom_point(alpha = 0.2) +
  facet_wrap(vars(displ_id))

it01_up <- it01_up |> 
  mutate(
    Y_sm_up_vel = get_velocity(Y_sm_up)
  )

it01_up |> 
  filter(displ_id == 14) |> 
  ggplot(aes(time_int, Y_sm_up_vel)) +
  geom_point() +
  geom_line(aes(y = abs(Y_sm_up_vel)), colour = "red") +
  annotate("rect", xmin = 0.3, xmax = 0.55, ymin = -Inf, ymax = Inf, alpha = 0.3)

# Filter signal to include a reduced time window
it01_up_win <- it01_up |>
  filter(displ_id == 14, time_int > 0.3, time_int < 0.55)

# Get landmarks
it01_up_win_land <- get_landmarks(it01_up_win$Y_sm_up_vel, it01_up_win$time_int, 0.4, 0.5)
it01_up_win_land_long <- it01_up_win_land |> 
  pivot_longer(everything(), names_to = "land")

it01_up_win_land_long

it01_up_win |> 
  ggplot(aes(time_int, abs(get_velocity(Y_sm_up)))) +
  geom_point(colour = "red") +
  geom_vline(data = it01_up_win_land_long |> filter(str_detect(land, "_ons|_off")), aes(xintercept = value)) +
  labs(y = "Absolute velocity")

it01_up_win |> 
  ggplot(aes(time_int, Y_sm_up)) +
  geom_point() +
  geom_vline(data = it01_up_win_land_long |> filter(str_detect(land, "_ons|_off")), aes(xintercept = value)) +
  labs(y = "Y displacement")

it01_up_lands <- it01_up |> 
  filter(knot == 8, time_int > 0.3, time_int < 0.55) |> 
  reframe(
    get_landmarks(Y_sm_up_vel, time_int, 0.35, 0.45),
    .by = displ_id
  )

it01_up_lands

it01_up_lands_long <- it01_up_lands |> 
  pivot_longer(-displ_id, names_to = "land")

it01_up |> 
  filter(knot == 8) |> 
  ggplot(aes(time_int, Y_sm_up)) +
  geom_point(alpha = 0.2) +
  geom_vline(data = it01_up_lands_long |> filter(str_detect(land, "_ons|_off")), aes(xintercept = value)) +
  facet_wrap(vars(displ_id))
