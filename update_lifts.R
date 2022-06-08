# Script to update training weights
library(DBI)
library(dplyr)
library(here)
library(tidyr)
here::i_am("README.md")

# constants
increment <- 5  # weight increment to which to round training maxes
preferred_method <- "epley"

# Rounding function
round_to_n <- function(x, n = increment) {
  if (is.double(n)) {
    round(n * round(x / n), 1)
  } else {
    n * round(x / n)
  }
}

# 1rm calculator functions
epley <- function(w, r) {
  w * (1 + (r / 30))
}
brzycki <- function(w, r) {
  w * (36 / (37 - r))
}
lombardi <- function(w, r) {
  w * r^0.10
}
mayhew <- function(w, r) {
  (100 * w) / (52.2 + 41.9 * exp(1)^(-0.055 * r))
}
oconner <- function(w, r) {
  w * (1 + r / 40)
}
wathen <- function(w, r) {
  (100 * w) / (48.8+ 53.8 * exp(1)^(-0.075 * r))
}

# Get Data
inp <- dbConnect(RSQLite::SQLite(), "gymroutines_2022-06-06.db")
out <- dbConnect(RSQLite::SQLite(), paste0("new_routines_", Sys.Date(), ".db"))
RSQLite::sqliteCopyDatabase(inp, out)
dbDisconnect(inp)

tbls <- lapply(setNames(nm = dbListTables(out)), dbReadTable, conn = out)
workouts <- tbls$routine_table |>
  filter(hidden == 0) |>
  left_join(tbls$workout_table) |>
  left_join(tbls$workout_set_group_table) |>
  left_join(tbls$exercise_table |> rename(exercise = name)) |>
  left_join(tbls$workout_set_table, by = c("id" = "groupId")) |>
  mutate(
    endTime = endTime |>
      as.character() |>
      substr(start = 1, stop = 10) |>
      as.numeric() |>
      as.POSIXct(origin = "1970-01-01") |>
      as.Date()
  )

# Get per-workout maximums
xrms <- workouts |>
  group_by(exercise) |>
  summarize(weight = max(weight)) |>
  left_join(
    workouts |>
      select(
        exercise,
        weight,
        reps
      )
  )

# Calculate 1rms
est_1rms <- xrms |>
  mutate(
    epley = epley(weight, reps) |> round(),
    brzycki = brzycki(weight, reps) |> round(),
    lombardi = lombardi(weight, reps) |> round(),
    mayhew = mayhew(weight, reps) |> round(),
    oconner = oconner(weight, reps) |> round(),
    wathen = wathen(weight, reps) |> round(),
  ) |>
  pivot_longer(
    c(
      epley,
      brzycki,
      lombardi,
      mayhew,
      oconner,
      wathen,
    ),
    names_to = "method",
    values_to = "estimate"
  ) |>
  filter(method == preferred_method) |>
  mutate(
    training_max = round_to_n(.90 * estimate, increment)
  )

# Start building out a new routine set table
pcts <- list(
  `5` = list(
    s1 = .65,
    s2 = .75,
    s3 = .85
  ),
  `3` = list(
    s1 = .70,
    s2 = .80,
    s3 = .90
  ),
  `1` = list(
    s1 = .75,
    s2 = .85,
    s3 = .95
  )
)
new_routines <- tibble()
loop <- 1
for (week in c(5, 3, 1)) {
  routine <- est_1rms |>
    mutate(
      name = paste(exercise, week),
      reps = week,
      weight = training_max
    )
  routine <- bind_rows(
    routine |> mutate(weight = round_to_n(pcts[[loop]]$s1 * weight, increment)),
    routine |> mutate(weight = round_to_n(pcts[[loop]]$s2 * weight, increment)),
    routine |> mutate(weight = round_to_n(pcts[[loop]]$s3 * weight, increment))
  )
  new_routines <- new_routines |> bind_rows(routine)
  loop <- loop + 1
}
new_routines <- new_routines |>
  arrange(desc(name), exercise, weight) |>
  mutate(row_num = row_number())

# Get old routine_set_table info
old_routines <- tbls$routine_table |>
  filter(hidden == 0) |>
  left_join(tbls$routine_set_group_table) |>
  left_join(tbls$routine_set_table, by = c("id" = "groupId"), keep = T) |>
  left_join(tbls$exercise_table |> rename(exercise = name)) |>
  arrange(desc(name), exercise, weight) |>
  mutate(row_num = row_number())

# Build new routine_set_table
routine_set_table <- new_routines |>
  select(weight, row_num) |>
  left_join(old_routines |> select(-weight)) |>
  select(groupId, reps, routineSetId, weight, time, distance) |>
  arrange(routineSetId)

# Upload new table into output db
dbSendQuery(
  out,
  "DROP TABLE IF EXISTS routine_set_table;"
)
dbSendQuery(
  out,
  "
  CREATE TABLE routine_set_table(
    routineSetId INTEGER NOT NULL PRIMARY KEY,
    groupId INTEGER,
    reps INTEGER,
    weight DECIMAL,
    time INTEGER,
    distance, INTEGER
  );
  "
)
dbWriteTable(out, "routine_set_table", routine_set_table, append = T)
dbDisconnect(out)

