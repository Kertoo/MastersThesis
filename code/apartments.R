library(tidyverse)
library(VGAM)

otodom_cleaned <- readr::read_csv("raw_data/otodom.csv")
olx_cleaned    <- readr::read_csv("raw_data/olx_cleaned.csv")

df1 <- otodom_cleaned |>
  filter(
    FlatDetails.RoomsNum == 1 &
    str_detect(str_to_lower(Description), "kawale"),
    #between(as.Date(ActivationDate), as.Date("2020-01-01"), as.Date("2020-09-30"))
  ) |>
  group_by(ID) |>
  arrange(desc(point), .by_group = TRUE) |>
  filter(row_number() == 1) |>
  ungroup() |>
  filter(
    between(as.Date(ModificationDate), as.Date("2020-04-01"), as.Date("2020-09-30"))
  ) |>
  mutate(otodom = TRUE)

df2 <- olx_cleaned |>
  filter(
    !str_detect(str_to_lower(description), "odstępne"),
    !str_detect(str_to_lower(description), "krótko"),
    between(params.m, 14.9, 50.1),
    between(as.Date(last_refresh), as.Date("2020-04-01"), as.Date("2020-09-30"))
  ) |>
  mutate(olx = TRUE)

df_join <- full_join(df1, df2, join_by(ID == external_id))
df_join$olx[is.na(df_join$olx)] <- FALSE
df_join$otodom[is.na(df_join$otodom)] <- FALSE

addmargins(table(df_join$olx,df_join$otodom))

