## code to prepare `DATASET` dataset goes here

library(tidyr)
library(dplyr)
mRS <- data.frame(
  group = sample(c(0,1), 200, T),
  gender = sample(c("M", "F"), 200, T),
  pre =  sample(c(0,1,2,3,4,5,6), 200, T, c(0.48, 0.18, 0.15, 0.1,  0.08, 0.04, 0.02)),
  post = sample(c(0,1,2,3,4,5,6), 200, T, c(0.50, 0.19, 0.13, 0.07, 0.07, 0.03, 0.01))
) |>
  pivot_longer(-c(group, gender),
               values_to = "mRS",
               names_to = "intv") |>
  arrange(rev(intv), group) |>
  mutate(across(c(gender, intv), factor)) |>
  mutate(across(c(group, mRS), as.integer))

usethis::use_data(mRS, overwrite = TRUE)
