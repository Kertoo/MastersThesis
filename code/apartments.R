library(tidyverse)
library(VGAM)
library(modelsummary)

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
  mutate(otodom = TRUE)

df2 <- olx_cleaned |>
  filter(
    !str_detect(str_to_lower(description), "odstępne"),
    !str_detect(str_to_lower(description), "krótko"),
    between(params.m, 14.9, 50.1),
    between(as.Date(last_refresh), as.Date("2020-04-01"), as.Date("2020-09-30")),
    params.rooms == "one"
  ) |>
  mutate(olx = TRUE)

df_join <- full_join(df1, df2, join_by(ID == external_id))
df_join$olx[is.na(df_join$olx)] <- FALSE
df_join$otodom[is.na(df_join$otodom)] <- FALSE

df <- tibble(
  olx    = df_join$olx,
  otodom = df_join$otodom,
  offer  = ifelse(
    is.na(df_join$Individual),
    df_join$advertiser_type,
    ifelse(df_join$Individual, "private", "business")
  ),
  square_m = ifelse(
    is.na(df_join$Area),
    df_join$params.m,
    df_join$Area
  ),
  furnished = ifelse(
    is.na(df_join$params.furniture),
    df_join$FlatDetails.Furnished,
    ifelse(df_join$params.furniture == "yes", TRUE, FALSE)
  )
)

df |>
  group_by(furnished, offer) |>
  count(olx, otodom)

# Naive estimator
c(
  ( 37 +   68) * ( 37 + 410) /  37,
  ( 14 +   55) * ( 21 +  14) /  14,
  ( 74 +  291) * ( 74 + 214) /  74,
  (166 + 1357) * (166 + 133) / 166
) |> sum()

rm(list = c("df_join", "df1", "df2", "olx_cleaned", "otodom_cleaned"))

# M_t models ####
mm_t_0 <- vglm(
  formula = cbind(olx, otodom) ~ 1,
  family = posbernoulli.t(parallel.t = TRUE ~ 0),
  data = df
)

mm_t_o <- vglm(
  formula = cbind(olx, otodom) ~ offer,
  family = posbernoulli.t(parallel.t = TRUE ~ 0),
  data = df
)

mm_t_f <- vglm(
  formula = cbind(olx, otodom) ~ furnished,
  family = posbernoulli.t(parallel.t = TRUE ~ 0),
  data = df
)

mm_t_sq <- vglm(
  formula = cbind(olx, otodom) ~ square_m,
  family = posbernoulli.t(parallel.t = TRUE ~ 0),
  data = df
)

mm_t_o_f <- vglm(
  formula = cbind(olx, otodom) ~ offer + furnished,
  family = posbernoulli.t(parallel.t = TRUE ~ 0),
  data = df
)

mm_t_o_sq <- vglm(
  formula = cbind(olx, otodom) ~ offer + square_m,
  family = posbernoulli.t(parallel.t = TRUE ~ 0),
  data = df
)

mm_t_f_sq <- vglm(
  formula = cbind(olx, otodom) ~ furnished + square_m,
  family = posbernoulli.t(parallel.t = TRUE ~ 0),
  data = df
)

mm_t_o_f_sq <- vglm(
  formula = cbind(olx, otodom) ~ offer + furnished + square_m,
  family = posbernoulli.t(parallel.t = TRUE ~ 0),
  data = df
)


# # M_b models ####
mm_b_0 <- vglm(
  formula = cbind(olx, otodom) ~ 1,
  family = posbernoulli.b(drop.b = TRUE ~ 0),
  data = df
)

mm_b_o <- vglm(
  formula = cbind(olx, otodom) ~ offer,
  family = posbernoulli.b(),
  data = df
)

mm_b_f <- vglm(
  formula = cbind(olx, otodom) ~ furnished,
  family = posbernoulli.b(),
  data = df
)

mm_b_sq <- vglm(
  formula = cbind(olx, otodom) ~ square_m,
  family = posbernoulli.b(drop.b = TRUE ~ 0),
  data = df
)

mm_b_o_f <- vglm(
  formula = cbind(olx, otodom) ~ offer + furnished,
  family = posbernoulli.b(drop.b = TRUE ~ 0),
  data = df
)

mm_b_o_sq <- vglm(
  formula = cbind(olx, otodom) ~ offer + square_m,
  family = posbernoulli.b(drop.b = FALSE ~ square_m),
  data = df
)

mm_b_f_sq <- vglm(
  formula = cbind(olx, otodom) ~ furnished + square_m,
  family = posbernoulli.b(drop.b = FALSE ~ square_m),
  data = df
)

mm_b_o_f_sq <- vglm(
  formula = cbind(olx, otodom) ~ offer + furnished + square_m,
  family = posbernoulli.b(drop.b = TRUE ~ 0),
  data = df
)

# # M_tb models ####
mm_tb_0 <- vglm(
  formula = cbind(olx, otodom) ~ 1,
  family = posbernoulli.tb(
    imethod = 2, ridge.constant = .05, ridge.power = -1
  ),
  data = df,
  control = vglm.control(
    maxit = 2000,
    epsilon = 1e-12,
    trace = TRUE
  )
)

mm_tb_o <- vglm(
  formula = cbind(olx, otodom) ~ offer,
  family = posbernoulli.tb(
    drop.b = TRUE ~ 0,
    parallel.t = TRUE ~ 0,
    imethod = 2, ridge.constant = .05, ridge.power = -1
  ),
  data = df,
  control = vglm.control(
    maxit = 2000,
    epsilon = 1e-12,
    trace = TRUE
  )
)

mm_tb_f <- vglm(
  formula = cbind(olx, otodom) ~ furnished,
  family = posbernoulli.tb(
    drop.b = TRUE ~ 0,
    parallel.t = TRUE ~ 0,
    imethod = 2, ridge.constant = .05, ridge.power = -1
  ),
  data = df,
  control = vglm.control(
    maxit = 2000,
    epsilon = 1e-12,
    trace = TRUE
  )
)

mm_tb_sq <- vglm(
  formula = cbind(olx, otodom) ~ square_m,
  family = posbernoulli.tb(
    drop.b = TRUE ~ 0,
    parallel.t = TRUE ~ 0,
    imethod = 2, ridge.constant = .05, ridge.power = -1
  ),
  data = df,
  control = vglm.control(
    maxit = 2000,
    epsilon = 1e-12,
    trace = TRUE
  )
)

mm_tb_o_f <- vglm(
  formula = cbind(olx, otodom) ~ offer + furnished,
  family = posbernoulli.tb(
    drop.b = TRUE ~ 0,
    parallel.t = TRUE ~ 0,
    parallel.b = TRUE ~ 0,
    imethod = 2, ridge.constant = .05, ridge.power = -1
  ),
  data = df,
  control = vglm.control(
    maxit = 2000,
    epsilon = 1e-12,
    trace = TRUE
  )
)

mm_tb_o_sq <- vglm(
  formula = cbind(olx, otodom) ~ offer + square_m,
  family = posbernoulli.tb(
    drop.b = TRUE ~ 0,
    parallel.t = TRUE ~ 0,
    imethod = 2, ridge.constant = .05, ridge.power = -1
  ),
  data = df,
  control = vglm.control(
    maxit = 2000,
    epsilon = 1e-12,
    trace = TRUE
  )
)

mm_tb_f_sq <- vglm(
  formula = cbind(olx, otodom) ~ furnished + square_m,
  family = posbernoulli.tb(
    drop.b = TRUE ~ 0,
    parallel.t = TRUE ~ 0,
    imethod = 2, ridge.constant = .05, ridge.power = -1
  ),
  data = df,
  control = vglm.control(
    maxit = 2000,
    epsilon = 1e-12,
    trace = TRUE
  )
)

mm_tb_o_f_sq <- vglm(
  formula = cbind(olx, otodom) ~ offer + furnished + square_m,
  family = posbernoulli.tb(
    drop.b = TRUE ~ furnished,
    parallel.t = TRUE ~ 0,
    imethod = 2, ridge.constant = .05, ridge.power = -1
  ),
  data = df,
  control = vglm.control(
    maxit = 2000,
    epsilon = 1e-12,
    trace = TRUE
  )
)

# # Validation ####
time_models <- list(
  mm_t_0, mm_t_o, mm_t_f, mm_t_sq, mm_t_o_f, 
  mm_t_o_sq, mm_t_f_sq, mm_t_o_f_sq
)
behav_models <- list(
  mm_b_0, mm_b_o, mm_b_f, mm_b_sq, mm_b_o_f, 
  mm_b_o_sq, mm_b_f_sq, mm_b_o_f_sq
)
time_behav_models <- list(
  mm_tb_0, mm_tb_o, mm_tb_f, mm_tb_sq, mm_tb_o_f, 
  mm_tb_o_sq, mm_tb_f_sq, mm_tb_o_f_sq
)

actual_table <- matrix(
  nrow = 4, ncol = 4,
  dimnames = list(
    c("00", "10", "01", "11"),
    c("private & furnished", "business & furnished",
      "private & not furnished", "private & not furnished")
  ),
  c(NA, sum((df$olx & !df$otodom)[df$offer == "private" &  df$furnished]),
    sum((   !df$olx &  df$otodom)[df$offer == "private" &  df$furnished]),
    sum((    df$olx &  df$otodom)[df$offer == "private" &  df$furnished]),
    NA, sum((df$olx & !df$otodom)[df$offer != "private" &  df$furnished]),
    sum((   !df$olx &  df$otodom)[df$offer != "private" &  df$furnished]),
    sum((    df$olx &  df$otodom)[df$offer != "private" &  df$furnished]),
    NA, sum((df$olx & !df$otodom)[df$offer == "private" & !df$furnished]),
    sum((   !df$olx &  df$otodom)[df$offer == "private" & !df$furnished]),
    sum((    df$olx &  df$otodom)[df$offer == "private" & !df$furnished]),
    NA, sum((df$olx & !df$otodom)[df$offer != "private" & !df$furnished]),
    sum((   !df$olx &  df$otodom)[df$offer != "private" & !df$furnished]),
    sum((    df$olx &  df$otodom)[df$offer != "private" & !df$furnished]))
)

get_fitted_t <- function(xx) {
  zz <- predict(xx)
  # inv logit link
  zz <- 1 / (1 + exp(-zz))
  
  norm_const <- 1 - (1 - zz[, 1]) * (1 - zz[, 2])
  
  matrix(
    nrow = 4, ncol = 4,
    data = c(
      # col 1
      sum((norm_const ^ -1)[df$offer == "private" &  df$furnished]) - 
        sum(df$offer == "private" &  df$furnished),
      sum((zz[, 1] * (1 - zz[, 2]) / norm_const)[df$offer == "private" &  df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 2] / norm_const)[df$offer == "private" &  df$furnished]),
      sum((zz[, 1] * zz[, 2]       / norm_const)[df$offer == "private" &  df$furnished]),
      # col 2
      sum((norm_const ^ -1)[df$offer != "private" &  df$furnished]) - 
        sum(df$offer != "private" &  df$furnished),
      sum((zz[, 1] * (1 - zz[, 2]) / norm_const)[df$offer != "private" &  df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 2] / norm_const)[df$offer != "private" &  df$furnished]),
      sum((zz[, 1] * zz[, 2]       / norm_const)[df$offer != "private" &  df$furnished]),
      # col 3
      sum((norm_const ^ -1)[df$offer == "private" & !df$furnished]) - 
        sum(df$offer == "private" & !df$furnished),
      sum((zz[, 1] * (1 - zz[, 2]) / norm_const)[df$offer == "private" & !df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 2] / norm_const)[df$offer == "private" & !df$furnished]),
      sum((zz[, 1] * zz[, 2]       / norm_const)[df$offer == "private" & !df$furnished]),
      # col 4
      sum((norm_const ^ -1)[df$offer != "private" & !df$furnished]) - 
        sum(df$offer != "private" & !df$furnished),
      sum((zz[, 1] * (1 - zz[, 2]) / norm_const)[df$offer != "private" & !df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 2] / norm_const)[df$offer != "private" & !df$furnished]),
      sum((zz[, 1] * zz[, 2]       / norm_const)[df$offer != "private" & !df$furnished])
    ),
    dimnames = list(
      c("00", "10", "01", "11"),
      c("private & furnished", "business & furnished",
        "private & not furnished", "private & not furnished")
    )
  )
}

get_fitted_b <- function(xx) {
  zz <- predict(xx)
  # inv logit link
  zz <- 1 / (1 + exp(-zz))
  norm_const <- 1 - (1 - zz[, 1]) ^ 2
  
  matrix(
    nrow = 4, ncol = 4,
    data = c(
      # col 1
      sum((norm_const ^ -1)[df$offer == "private" &  df$furnished]) - 
        sum(df$offer == "private" &  df$furnished),
      sum((zz[, 1] * (1 - zz[, 2]) / norm_const)[df$offer == "private" &  df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 1] / norm_const)[df$offer == "private" &  df$furnished]),
      sum((zz[, 1] * zz[, 2]       / norm_const)[df$offer == "private" &  df$furnished]),
      # col 2
      sum((norm_const ^ -1)[df$offer != "private" &  df$furnished]) - 
        sum(df$offer != "private" &  df$furnished),
      sum((zz[, 1] * (1 - zz[, 2]) / norm_const)[df$offer != "private" &  df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 1] / norm_const)[df$offer != "private" &  df$furnished]),
      sum((zz[, 1] * zz[, 2]       / norm_const)[df$offer != "private" &  df$furnished]),
      # col 3
      sum((norm_const ^ -1)[df$offer == "private" & !df$furnished]) - 
        sum(df$offer == "private" & !df$furnished),
      sum((zz[, 1] * (1 - zz[, 2]) / norm_const)[df$offer == "private" & !df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 1] / norm_const)[df$offer == "private" & !df$furnished]),
      sum((zz[, 1] * zz[, 2]       / norm_const)[df$offer == "private" & !df$furnished]),
      # col 4
      sum((norm_const ^ -1)[df$offer != "private" & !df$furnished]) - 
        sum(df$offer != "private" & !df$furnished),
      sum((zz[, 1] * (1 - zz[, 2]) / norm_const)[df$offer != "private" & !df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 1] / norm_const)[df$offer != "private" & !df$furnished]),
      sum((zz[, 1] * zz[, 2]       / norm_const)[df$offer != "private" & !df$furnished])
    ),
    dimnames = list(
      c("00", "10", "01", "11"),
      c("private & furnished", "business & furnished",
        "private & not furnished", "private & not furnished")
    )
  )
}

get_fitted_tb <- function(xx) {
  zz <- predict(xx)
  # inv logit link
  zz <- 1 / (1 + exp(-zz))
  norm_const <- 1 - (1 - zz[, 1]) * (1 - zz[,2])
  
  matrix(
    nrow = 4, ncol = 4,
    data = c(
      # col 1
      sum((norm_const ^ -1)[df$offer == "private" &  df$furnished]) - 
        sum(df$offer == "private" &  df$furnished),
      sum((zz[, 1] * (1 - zz[, 3]) / norm_const)[df$offer == "private" &  df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 2] / norm_const)[df$offer == "private" &  df$furnished]),
      sum((zz[, 1] * zz[, 3]       / norm_const)[df$offer == "private" &  df$furnished]),
      # col 2
      sum((norm_const ^ -1)[df$offer != "private" &  df$furnished]) - 
        sum(df$offer != "private" &  df$furnished),
      sum((zz[, 1] * (1 - zz[, 3]) / norm_const)[df$offer != "private" &  df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 2] / norm_const)[df$offer != "private" &  df$furnished]),
      sum((zz[, 1] * zz[, 3]       / norm_const)[df$offer != "private" &  df$furnished]),
      # col 3
      sum((norm_const ^ -1)[df$offer == "private" & !df$furnished]) - 
        sum(df$offer == "private" & !df$furnished),
      sum((zz[, 1] * (1 - zz[, 3]) / norm_const)[df$offer == "private" & !df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 2] / norm_const)[df$offer == "private" & !df$furnished]),
      sum((zz[, 1] * zz[, 3]       / norm_const)[df$offer == "private" & !df$furnished]),
      # col 4
      sum((norm_const ^ -1)[df$offer != "private" & !df$furnished]) - 
        sum(df$offer != "private" & !df$furnished),
      sum((zz[, 1] * (1 - zz[, 3]) / norm_const)[df$offer != "private" & !df$furnished]),
      sum(((1 - zz[, 1]) * zz[, 2] / norm_const)[df$offer != "private" & !df$furnished]),
      sum((zz[, 1] * zz[, 3]       / norm_const)[df$offer != "private" & !df$furnished])
    ),
    dimnames = list(
      c("00", "10", "01", "11"),
      c("private & furnished", "business & furnished",
        "private & not furnished", "private & not furnished")
    )
  )
}

gstat_t <- sapply(time_models, function(xx) {
  sum(2 * actual_table * log(actual_table / get_fitted_t(xx)), na.rm = TRUE)
})

gstat_b <- sapply(behav_models, function(xx) {
  sum(2 * actual_table * log(actual_table / get_fitted_b(xx)), na.rm = TRUE)
})

gstat_tb <- sapply(time_behav_models, function(xx) {
  sum(2 * actual_table * log(actual_table / get_fitted_tb(xx)), na.rm = TRUE)
})

names(time_models)       <- paste0("M_t_" , 1:length(time_models))
names(behav_models)      <- paste0("M_b_" , 1:length(time_models))
names(time_behav_models) <- paste0("M_tb_", 1:length(time_models))

modelsummary(
  time_models,
  add_rows = t(tibble(
    "hatN" = sapply(time_models,
      \(x) x@extra$N.hat
    ),
    "se_hatN" = sapply(time_models,
      \(x) x@extra$SE.N.hat
    ),
    "gstat" = gstat_t
  )) |> as_tibble(rownames = "name")
)

modelsummary(
  behav_models,
  add_rows = tibble(
    "hatN" = sapply(behav_models,
      \(x) x@extra$N.hat
    ),
    "se_hatN" = sapply(behav_models,
      \(x) x@extra$SE.N.hat
    ),
    "gstat" = gstat_b
  ) |> t() |> as_tibble(rownames = "name")
)

modelsummary(
  time_behav_models,
  add_rows = tibble(
    "hatN" = sapply(time_behav_models,
      \(x) x@extra$N.hat
    ),
    "se_hatN" = sapply(time_behav_models,
      \(x) x@extra$SE.N.hat
    ),
    "gstat" = gstat_tb
  ) |> t() |> as_tibble(rownames = "name"),
  output = "latex_tabular"
)

df |>
  mutate(
    furnished = ifelse(
      furnished, "Umeblowane", "Nie umeblowane"
    ),
    oferta    = ifelse(
      offer == "private", "Prywatna", "Biznesowa"
    ),
    olx       = ifelse(
      olx, "Na olx", "Poza olx"
    ),
    otodom    = ifelse(
      otodom, "Na otodom", "Poza otodom"
    )
  ) |>
  ggplot(aes(x = square_m)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2.5) + 
  geom_density() +
  facet_grid(furnished + olx ~ otodom + oferta,
             labeller = labeller(
               furnished = label_value, olx = label_value,
               oferta = label_both, otodom = label_value
             )) +
  xlab("Metry kwadratowe") +
  ylab("Gęstość")

get_fitted_tb(time_behav_models[[5]])

dfb <- dfbeta(time_behav_models[[5]])
summary(dfb)

# confidence interval:
# normal
# 90%
pmax(
  nrow(df),
  time_behav_models[[5]]@extra$N.hat + 
    c(-1, 1) * qnorm(1 - .1 / 2) * time_behav_models[[5]]@extra$SE.N.hat
)
# 95%
pmax(
  nrow(df),
  time_behav_models[[5]]@extra$N.hat + 
    c(-1, 1) * qnorm(1 - .05 / 2) * time_behav_models[[5]]@extra$SE.N.hat
)

# log-normal
# 90%
G <- exp(
  qnorm(1 - .1 / 2) * sqrt(log(
    1 + time_behav_models[[5]]@extra$SE.N.hat ^ 2 / 
      (time_behav_models[[5]]@extra$N.hat - nrow(df)) ^ 2
  ))
)

nrow(df) + c(
  (time_behav_models[[5]]@extra$N.hat - nrow(df)) / G,
  (time_behav_models[[5]]@extra$N.hat - nrow(df)) * G
)
# 95%
G <- exp(
  qnorm(1 - .05 / 2) * sqrt(log(
    1 + time_behav_models[[5]]@extra$SE.N.hat ^ 2 / 
      (time_behav_models[[5]]@extra$N.hat - nrow(df)) ^ 2
  ))
)

nrow(df) + c(
  (time_behav_models[[5]]@extra$N.hat - nrow(df)) / G,
  (time_behav_models[[5]]@extra$N.hat - nrow(df)) * G
)
