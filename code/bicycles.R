library(tidyverse)
library(VGAMextra)

bicycle <- readr::read_csv(
  paste0(
    "https://gist.githubusercontent.com/sachinsdate/",
    "c17931a3f000492c1c42cf78bf4ce9fe/raw",
    "/7a5131d3f02575668b3c7e8c146b6a285acd2cd7/nyc_bb_bicyclist_counts.csv"
  )
)
bicycle$DateProper <- bicycle$Date %>% dmy
bicycle$Weekend <- bicycle$DateProper %>% wday() %in% c(6, 7)
summary(bicycle)

sapply(1:(NROW(bicycle)-1), 
       FUN = function(x) bicycle$DateProper[x + 1] - bicycle$DateProper[x])

model <- vglm(formula = BB_COUNT ~ HIGH_T + LOW_T + PRECIP,
              data = bicycle,
              family = poissonTSff(Order = c(7, 7)),
              control = vglm.control(trace = TRUE, 
                                     epsilon = .Machine$double.eps))

matplot(x = bicycle$DateProper, y = bicycle$BB_COUNT, 
        "l", col = "red")
matplot(x = bicycle$DateProper, y = fitted(model)[, 1], 
        "l", add = TRUE, col = "green")

cond <- c(TRUE, TRUE)

order <- c(0, 0)

## already checked it you need all HIGH_T, LOW_T, PRECIP variables

while (any(cond)) {
  model <- vglm(formula = BB_COUNT ~ HIGH_T + LOW_T + PRECIP,
                data = bicycle,
                family = VGLM.INGARCHff(Order = order,
                                        dist.type = "negbinomial",
                                        link = "loglink"),
                control = vglm.control(trace = FALSE))
  
  model1 <- vglm(formula = BB_COUNT ~ HIGH_T + LOW_T + PRECIP,
                data = bicycle,
                family = VGLM.INGARCHff(Order = order + c(1, 0),
                                        dist.type = "negbinomial",
                                        link = "loglink"),
                control = vglm.control(trace = FALSE))
  
  model2 <- vglm(formula = BB_COUNT ~ HIGH_T + LOW_T + PRECIP,
                 data = bicycle,
                 family = VGLM.INGARCHff(Order = order + c(0, 1),
                                         dist.type = "negbinomial",
                                         link = "loglink"),
                 control = vglm.control(trace = FALSE))
  
  model3 <- vglm(formula = BB_COUNT ~ HIGH_T + LOW_T + PRECIP + Weekend,
                 data = bicycle,
                 family = VGLM.INGARCHff(Order = order + c(1, 1),
                                         dist.type = "negbinomial",
                                         link = "loglink"),
                 control = vglm.control(trace = FALSE))
  if (any((abc <- sapply(list(model1, model2, model3), FUN = AIC)) < AIC(model))) {
    model <- list(model1, model2, model3)[which.min(abc)]
    order <- order + switch (which.min(abc),
      c(1, 0),
      c(0, 1),
      c(1, 1)
    )
  } else {
    cond <- FALSE
  }
}

qqnorm(resid(model, "pearson")[, 1])
qqline(resid(model, "pearson")[, 1])

qqnorm(resid(model, "pearson")[, 2])
qqline(resid(model, "pearson")[, 2])

summary(model)

model_no_time <- vglm(formula = BB_COUNT ~ HIGH_T + LOW_T + PRECIP,
                      data = bicycle,
                      family = negbinomial(zero = NULL))

fitted(model)

matplot(
  x    = bicycle$DateProper, 
  y    = bicycle$BB_COUNT, 
  type = "b", 
  col  = "red",
  pch  = 19,
  lty  = 1,
  ylab = "Liczba rowerzystów",
  xlab = "Dzień",
  main = paste0("Dzienne liczba rowerzystów na moście manhatańskim\n",
                "w okresie od ", 
                range(bicycle$DateProper)[1], 
                " do ", 
                range(bicycle$DateProper)[2]),
  axes = FALSE,
  cex  = .6
)
axis(2, las = 2, at = seq(from = 0, 
                          to = 5000, 
                          by = 500))
axis(1, 
     labels = FALSE,
     at = seq.Date(from = range(bicycle$DateProper)[1], 
                   to   = range(bicycle$DateProper)[2] + days(4), 
                   by   = 1),
     lwd = .6)
axis(1, 
     labels = FALSE,
     at = seq.Date(from = range(bicycle$DateProper)[1], 
                   to   = range(bicycle$DateProper)[2] + days(7), 
                   by   = 7),
     lwd = 1.5)

text(
  x = seq.Date(from = range(bicycle$DateProper)[1], 
               to   = range(bicycle$DateProper)[2] + days(7), 
               by   = 7), 
  y = par("usr", no.readonly = TRUE)[3] - 150, 
  adj = 1,
  seq.Date(from = range(bicycle$DateProper)[1], 
           to   = range(bicycle$DateProper)[2] + days(7), 
           by   = 7),
  srt = 30, cex = 0.7, xpd = TRUE
)

matplot(x = bicycle$DateProper, 
        y = fitted(model)[, 1], 
        "b", add = TRUE, 
        col = "green",
        pch = 19,
        lty = 2,
        cex = .6)
legend("topleft",
       legend = c("Ilości dopasowane", "Ilości zaobserwowane"), 
       col = c("green", "red"), pch = 19,
       lty = c(2, 1),
       box.lwd = .7,
       cex = .6)

