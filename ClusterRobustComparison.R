library(VGAM)
library(sandwich)
library(lmtest)
library(tidyverse)
library(doParallel)
library(foreach)

set.seed(123)
tries <- 1000
#res <- matrix(ncol = 3, nrow = tries)
ress <- matrix(nrow = 0, ncol = 11) |> as.data.frame()

for (design in c("D1", "D2", "D3", "D4")) {
  for (N in seq(25, 300, by = 25)) {
    print(cbind(design, N))
    
    cl <- makeCluster(detectCores() - 1)
    registerDoParallel(cl)
    
    res <- foreach(
      k = 1:tries, .combine = rbind,
      .packages = c("VGAM", "sandwich", "lmtest")
    ) %dopar% {
      x <- sin(runif(n = N))
      x1 <- rbinom(n = N, prob = .5, size = 1)
      effect <- switch (
        design,
        "D1" = 1 + ifelse(x1, 1, 0),
        "D2" = 1 + ifelse(x1, 1, 0), 
        "D3" = 1,# x / x1 effect
        "D4" = 1
      )
      het <- switch (
        design,
        "D1" = 0,
        "D2" = rnorm(N, mean = 1, sd = 1), # Additional heteroscedasticity
        "D3" = 0,
        "D4" = rnorm(N, mean = 1, sd = 1)
      )
      y <- x * effect - 1 + rnorm(n = N, sd = exp(.5 + x1 + het))
      df <- data.frame(x = x, y = y, x1 = x1)
      
      m1 <- lm(y ~ x / x1, data = df)
      m2 <- vglm(y ~ x * x1, data = df, family = uninormal(zero = NULL),
                 constraints = list(
                   "(Intercept)" = diag(2),
                   "x" = rbind(1, 0),
                   "x1" = rbind(0, 1),
                   "x:x1" = rbind(1, 0)
                 ))
      
      sm_lm <- summary(m1)
      sm_vlm <- summary(m2)
      sm_hc0 <- coeftest(m1, vcovHC(m1, type = "HC0"))
      sm_hc1 <- coeftest(m1, vcovHC(m1, type = "HC1"))
      sm_hc2 <- coeftest(m1, vcovHC(m1, type = "HC2"))
      sm_hc3 <- coeftest(m1, vcovHC(m1, type = "HC3"))
      sm_hc4 <- coeftest(m1, vcovHC(m1, type = "HC4"))
      sm_hc4m <- coeftest(m1, vcovHC(m1, type = "HC4m"))
      sm_hc5 <- coeftest(m1, vcovHC(m1, type = "HC5"))
      
      cbind(
        sm_lm$coefficients[3, c(1, 2, 4)][3],
        sm_vlm@coef3[5, c(1, 2, 4)][3],
        sm_hc0[3, c(1, 2, 4)][3],
        sm_hc1[3, c(1, 2, 4)][3],
        sm_hc2[3, c(1, 2, 4)][3],
        sm_hc3[3, c(1, 2, 4)][3],
        sm_hc4[3, c(1, 2, 4)][3],
        sm_hc4m[3, c(1, 2, 4)][3],
        sm_hc5[3, c(1, 2, 4)][3]
      )
    }
    stopCluster(cl)
    
    ress <- rbind(ress, cbind(res |> as.data.frame(), N, design))
  }
}

ress <- as.data.frame(ress)
colnames(ress) <- c("lm", "vlm", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5", "N", "design")

ress |>
  filter(design %in% c("D1", "D2")) |>
  pivot_longer(!(N | design)) |>
  transform(xx = value < .05) |>
  group_by(name, N, design) |>
  summarise(power = mean(xx)) |>
  transform(N = paste0("N = ", N) |> 
    ordered(levels = c("N = 25", "N = 50", "N = 75", 
                       "N = 100", "N = 125", "N = 150", 
                       "N = 175", "N = 200", "N = 225", 
                       "N = 250", "N = 275", "N = 300"))) |>
  mutate(lower = mapply(function(x) binom.test(x = tries * x, n = tries, p = .05)$conf.int[1],
                        power),
         upper = mapply(function(x) binom.test(x = tries * x, n = tries, p = .05)$conf.int[2],
                        power)) |>
  ggplot(aes(y = power, x = name, shape = design, color = design)) +
  geom_point(position = position_dodge(width = .3)) +
  facet_wrap(~ N, scales = "free_x") +
  ylab("Empiryczna moc testu") + 
  xlab("Model będący podstawą testu")  +
  theme(legend.position = "top", 
        legend.text  = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5))) +
  scale_color_manual(name = "Układ", 
                     labels = c("Tylko warunkowa heteroskedastyczność", 
                                "Indywidualna i warunkowa heteroskedastyczność"), 
                     values=c("dodgerblue4", "firebrick4")) +
  scale_shape_manual(name = "Układ", 
                     labels = c("Tylko warunkowa heteroskedastyczność", 
                                "Indywidualna i warunkowa heteroskedastyczność"), 
                     values = 16:17) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = .3)) +
  scale_y_continuous(breaks = round(seq(0, .4, by = 0.04), 3))

ggsave(filename = "empirical_power_in_factorial_study_design.png",
       height = 13,
       scale = 1.32)

ress |>
  filter(design %in% c("D3", "D4")) |>
  pivot_longer(!(N | design)) |>
  transform(xx = value < .05) |>
  group_by(name, N, design) |>
  summarise("Type 1 error" = mean(xx)) |>
  transform(N = paste0("N = ", N) |> 
    ordered(levels = c("N = 25", "N = 50", "N = 75", 
                       "N = 100", "N = 125", "N = 150", 
                       "N = 175", "N = 200", "N = 225", 
                       "N = 250", "N = 275", "N = 300"))) |>
  mutate(lower = mapply(function(x) binom.test(x = tries * x, n = tries, p = .05)$conf.int[1],
                        Type.1.error),
         upper = mapply(function(x) binom.test(x = tries * x, n = tries, p = .05)$conf.int[2],
                        Type.1.error)) |>
  ggplot(aes(y = Type.1.error, x = name, shape = design, color = design)) +
  geom_point(position = position_dodge(width = .3)) +
  facet_wrap(~ N, scales = "free_x") +
  ylab("Empiryczny rozmiar testu") + 
  xlab("Model będący podstawą testu") +
  theme(legend.position = "top", 
        legend.text  = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5))) +
  scale_color_manual(name = "Układ", 
                     labels = c("Tylko warunkowa heteroskedastyczność", 
                                "Indywidualna i warunkowa heteroskedastyczność"), 
                     values=c("dodgerblue4", "firebrick4")) +
  scale_shape_manual(name = "Układ", 
                     labels = c("Tylko warunkowa heteroskedastyczność", 
                                "Indywidualna i warunkowa heteroskedastyczność"), 
                     values = 16:17) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = .3)) +
  scale_y_continuous(breaks = round(seq(0, .15, by = .0175), 3))

ggsave(filename = "empirical_size_in_factorial_study_design.png",
       height = 13,
       scale = 1.32)

write.csv(ress, file = "factorial_study_design_simulation_results.csv")
