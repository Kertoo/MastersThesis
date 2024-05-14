library(VGAM)

p1  <- c(.5, .6)
p2  <- c(.2, .3)
p_x <- .5
nsims <- 10000
N <- 10000

res <- matrix(
  nrow = nsims, ncol = 3
)

for (k in 1:nsims) {
  x1 <- 1 + rbinom(n = N, size = 1, prob = p_x)
  y <- data.frame(
    y1 = rbinom(n = N, size = 1, prob = p1[x1]),
    y2 = rbinom(n = N, size = 1, prob = p2[x1]),
    x1 = x1
  )
  Y <- y[y$y1 == 1 | y$y2 == 1,]
  
  mm <- vglm(
    cbind(y1, y2) ~ x1,
    data = Y,
    family = posbernoulli.t(
      parallel.t = TRUE ~ 0
    ),
    control = vglm.control(
      noWarning = TRUE
    ),
    constraints = list(
      "(Intercept)" = diag(2),
      "x1" = diag(2)
    )
  )
  
  tab <- ftable(y)
  
  df <- Y |>
    group_by(y1, y2, x1) |>
    count()
  
  mm2 <- glm(
    formula = n ~ (y1 + y2) * x1,
    family  = poisson(),
    data = df
  )
  
  predict(mm2, newdata = data.frame(
    y1 = 0, y2 = 0, x1 = unique(df$x1)
  ))
  
  res[k,] <- c(
    mm@extra$N.hat,
    sum(sapply(1:ncol(tab), function (x) {
        (tab[, x][2] + tab[, x][4]) * (tab[, x][3] + tab[, x][4]) / tab[, x][4]
      }
    )),
    sum(exp(predict(mm2, newdata = data.frame(y1 = 0, y2 = 0, x1 = c(1, 2))))) + sum(df$n)
  )
}

boxplot(
  res[, 1] - res[, 2],
  res[, 1] - res[, 3],
  res[, 2] - res[, 3]
)

# sim 2

nsims <- 10000
N <- 10000

res <- matrix(
  nrow = nsims, ncol = 3
)

for (k in 1:nsims) {
  x1 <- rnorm(N)
  y <- data.frame(
    y1 = rbinom(n = N, size = 1, prob = plogis(1 - .2 * x1)),
    y2 = rbinom(n = N, size = 1, prob = plogis(-1 + .8 * x1)),
    x1 = x1
  )
  Y <- y[y$y1 == 1 | y$y2 == 1,]
  
  mm <- vglm(
    cbind(y1, y2) ~ x1,
    data = Y,
    family = posbernoulli.t(
      parallel.t = TRUE ~ 0
    ),
    control = vglm.control(
      noWarning = TRUE
    ),
    constraints = list(
      "(Intercept)" = diag(2),
      "x1" = diag(2)
    )
  )
  
  y$x1 <- cut(y$x1, breaks = quantile(Y$x1, seq(0, 1, .1)), 
              include.lowest = TRUE)
  tab <- ftable(y)
  
  df <- Y |>
    mutate(x1 = cut(x1, breaks = quantile(Y$x1, seq(0, 1, .1)), 
                    include.lowest = TRUE)) |>
    group_by(y1, y2, x1) |>
    count()
  
  mm2 <- glm(
    formula = n ~ y1 + y2 + x1,
    family  = poisson(),
    data = df
  )
  
  res[k,] <- c(
    mm@extra$N.hat,
    sum(sapply(1:ncol(tab), function (x) {
      (tab[, x][2] + tab[, x][4]) * (tab[, x][3] + tab[, x][4]) / tab[, x][4]
    }
    )),
    sum(exp(predict(mm2, newdata = data.frame(y1 = 0, y2 = 0, x1 = unique(df$x1))))) + sum(df$n)
  )
}

data.frame(
  result = c(res[, 1] - res[, 2], res[, 1] - res[, 2], res[, 2] - res[, 3]),
  comparison = rep(c(
    "OL-FL",
    "OL-ll",
    "FL-ll"
  ), each = nsims)
) |>
  ggplot(aes(x = comparison, y = result)) +
  geom_violin(alpha = 0.8, draw_quantiles = 1:9 / 10, scale = "width") +
  geom_jitter(alpha = .05, height = 0)

data.frame(
  result = c(res[, 1] - N, res[, 2] - N, res[, 3] - N),
  comparison = rep(c(
    "Observed likelihood",
    "Full likelihood",
    "log linear model"
  ), each = nsims)
) |>
  ggplot(aes(x = comparison, y = result)) +
  geom_violin(alpha = 0.8, draw_quantiles = 1:9 / 10, scale = "width") +
  geom_jitter(alpha = .05, height = 0)

data.frame(
  result = c(res[, 1] - N, res[, 2] - N, res[, 3] - N),
  comparison = rep(c(
    "Observed likelihood",
    "Full likelihood",
    "log linear model"
  ), each = nsims)
) |>
  group_by(comparison) |>
  summarise(
    bias = mean(result),
    mse  = mean(result ^ 2),
    mae  = mean(abs(result))
  )
