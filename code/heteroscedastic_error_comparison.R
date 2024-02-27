library(VGAM)
library(sandwich)
library(lmtest)
library(tidyverse)
library(doSNOW)
library(progress)
# Set on your own
cores <- 7
# Set both of these to FALSE if you want to just recreate figures
# using saved simulation results
run_sim_multicore <- FALSE
run_sim_singlecore <- FALSE

# Importing custom VGAM methods defined in heteroscedasticErrorsFunctions.R
source("code/heteroscedastic_errors_functions.R")

#### Simulations ####

## multicore
if (run_sim_multicore) {
  set.seed(123)
  tries <- 10000
  
  ress <- matrix(nrow = 0, ncol = 18) |> as.data.frame()
  
  for (design in c("D1", "D2", "D3", "D4")) {
    for (N in seq(25, 300, by = 25)) {
      cat("design is ", design, " number of observations = ", N, "\n", sep = "")
      
      cl <- makeCluster(cores)
      clusterExport(cl, c("AA", "AA1", "AA2"))
      registerDoSNOW(cl)
      
      pb <- progress_bar$new(format = "[:bar] :percent [Elapsed: :elapsedfull || Remaining: :eta]",
                             total = tries)
      
      opts <- list(progress = \(n) pb$tick())
      
      res <- foreach(
        k = 1:tries, .combine = rbind,
        .packages = c("VGAM", "sandwich", "lmtest"),
        .options.snow = opts
      ) %dopar% {
        x <- sin(runif(n = N))
        x1 <- rbinom(n = N, prob = .5, size = 1)
        ef <- switch (design,
          "D1" = 1 + ifelse(x1, 1, 0),
          "D2" = 1 + ifelse(x1, 1, 0), 
          "D3" = 1,# x / x1 effect
          "D4" = 1
        )
        het <- switch (design,
          "D1" = 0,
          "D2" = rnorm(N, mean = 1, sd = 1), # Additional heteroscedasticity
          "D3" = 0,
          "D4" = rnorm(N, mean = 1, sd = 1)
        )
        y <- x * ef - 1 + rnorm(n = N, sd = exp(.5 + x1 + het))
        df <- data.frame(x = x, y = y, x1 = x1)
        
        m1 <- lm(y ~ x / x1, data = df)
        m2 <- vglm(
          y ~ x * x1, data = df, family = uninormal(zero = NULL),
          constraints = list(
            "(Intercept)" = diag(2),
            "x" = rbind(1, 0),
            "x1" = rbind(0, 1),
            "x:x1" = rbind(1, 0)
          )
        )
        
        sm_lm   <- summary(m1)
        sm_lm_hc0  <- coeftest(m1, vcovHC(m1, type =  "HC0"))
        sm_lm_hc1  <- coeftest(m1, vcovHC(m1, type =  "HC1"))
        sm_lm_hc2  <- coeftest(m1, vcovHC(m1, type =  "HC2"))
        sm_lm_hc3  <- coeftest(m1, vcovHC(m1, type =  "HC3"))
        sm_lm_hc4  <- coeftest(m1, vcovHC(m1, type =  "HC4"))
        sm_lm_hc4m <- coeftest(m1, vcovHC(m1, type = "HC4m"))
        sm_lm_hc5  <- coeftest(m1, vcovHC(m1, type =  "HC5"))
        
        
        sm_vlm  <- summary(m2)
        # Error pops out here
        sm_vlm_hc0  <- coeftest(m2, AA2(m2, type =  "HC0"))
        sm_vlm_hc1  <- coeftest(m2, AA2(m2, type =  "HC1"))
        sm_vlm_hc2  <- coeftest(m2, AA2(m2, type =  "HC2"))
        sm_vlm_hc3  <- coeftest(m2, AA2(m2, type =  "HC3"))
        sm_vlm_hc4  <- coeftest(m2, AA2(m2, type =  "HC4"))
        sm_vlm_hc4m <- coeftest(m2, AA2(m2, type = "HC4m"))
        sm_vlm_hc5  <- coeftest(m2, AA2(m2, type =  "HC5"))
        
        cbind(
          sm_lm$coefficients[3, c(1, 2, 4)][3],
          sm_lm_hc0[3, c(1, 2, 4)][3],
          sm_lm_hc1[3, c(1, 2, 4)][3],
          sm_lm_hc2[3, c(1, 2, 4)][3],
          sm_lm_hc3[3, c(1, 2, 4)][3],
          sm_lm_hc4[3, c(1, 2, 4)][3],
          sm_lm_hc4m[3, c(1, 2, 4)][3],
          sm_lm_hc5[3, c(1, 2, 4)][3],
          sm_vlm@coef3[5, c(1, 2, 4)][3],
          sm_vlm_hc0[5, c(1, 2, 4)][3],
          sm_vlm_hc1[5, c(1, 2, 4)][3],
          sm_vlm_hc2[5, c(1, 2, 4)][3],
          sm_vlm_hc3[5, c(1, 2, 4)][3],
          sm_vlm_hc4[5, c(1, 2, 4)][3],
          sm_vlm_hc4m[5, c(1, 2, 4)][3],
          sm_vlm_hc5[5, c(1, 2, 4)][3]
        )
      }
      stopCluster(cl)
      
      ress <- rbind(ress, cbind(res |> as.data.frame(), N, design))
    }
  }
  
  ress <- as.data.frame(ress)
}

## single core
if (run_sim_singlecore) {
  set.seed(123)
  tries <- 10000
  #res <- matrix(ncol = 3, nrow = tries)
  ress <- matrix(nrow = tries, ncol = 18) |> as.data.frame()
  
  for (design in c("D1", "D2", "D3", "D4")) {
    for (N in seq(25, 300, by = 25)) {
      print(cbind(design, N))
      
      for (kk in 1L:tries) {
        x <- sin(runif(n = N))
        x1 <- rbinom(n = N, prob = .5, size = 1)
        ef <- switch (design,
          "D1" = 1 + x1,
          "D2" = 1 + x1, 
          "D3" = 1,# x / x1 effect
          "D4" = 1
        )
        het <- switch (design,
          "D1" = 0,
          "D2" = rnorm(N, mean = 1, sd = 1), # Additional heteroscedasticity
          "D3" = 0,
          "D4" = rnorm(N, mean = 1, sd = 1)
        )
        y <- x * ef - 1 + rnorm(n = N, sd = exp(.5 + x1 + het))
        df <- data.frame(x = x, y = y, x1 = x1)
        
        m1 <- lm(y ~ x / x1, data = df)
        m2 <- vglm(
          y ~ x * x1, 
          data = df, 
          family = uninormal(zero = NULL),
          constraints = list(
            "(Intercept)" = diag(2),
            "x" = rbind(1, 0),
            "x1" = rbind(0, 1),
            "x:x1" = rbind(1, 0)
          )
        )
        
        sm_lm <- summary(m1)
        sm_vlm <- summary(m2)
        sm_lm_hc0  <- coeftest(m1, vcovHC(m1, type = "HC0"))
        sm_lm_hc1  <- coeftest(m1, vcovHC(m1, type = "HC1"))
        sm_lm_hc2  <- coeftest(m1, vcovHC(m1, type = "HC2"))
        sm_lm_hc3  <- coeftest(m1, vcovHC(m1, type = "HC3"))
        sm_lm_hc4  <- coeftest(m1, vcovHC(m1, type = "HC4"))
        sm_lm_hc4m <- coeftest(m1, vcovHC(m1, type = "HC4m"))
        sm_lm_hc5  <- coeftest(m1, vcovHC(m1, type = "HC5"))
        
        sm_vlm_hc0  <- coeftest(m2, vcovHC(m2, type = "HC0"))
        sm_vlm_hc1  <- coeftest(m2, vcovHC(m2, type = "HC1"))
        sm_vlm_hc2  <- coeftest(m2, vcovHC(m2, type = "HC2"))
        sm_vlm_hc3  <- coeftest(m2, vcovHC(m2, type = "HC3"))
        sm_vlm_hc4  <- coeftest(m2, vcovHC(m2, type = "HC4"))
        sm_vlm_hc4m <- coeftest(m2, vcovHC(m2, type = "HC4m"))
        sm_vlm_hc5  <- coeftest(m2, vcovHC(m2, type = "HC5"))
        
        ress[kk, 1:17] <- c(
          sm_lm$coefficients[3, c(1, 2, 4)][3],
          sm_lm_hc0[3, c(1, 2, 4)][3],
          sm_lm_hc1[3, c(1, 2, 4)][3],
          sm_lm_hc2[3, c(1, 2, 4)][3],
          sm_lm_hc3[3, c(1, 2, 4)][3],
          sm_lm_hc4[3, c(1, 2, 4)][3],
          sm_lm_hc4m[3, c(1, 2, 4)][3],
          sm_lm_hc5[3, c(1, 2, 4)][3],
          #vlm stuff
          sm_vlm@coef3[5, c(1, 2, 4)][3],
          sm_vlm_hc0[5, c(1, 2, 4)][3],
          sm_vlm_hc1[5, c(1, 2, 4)][3],
          sm_vlm_hc2[5, c(1, 2, 4)][3],
          sm_vlm_hc3[5, c(1, 2, 4)][3],
          sm_vlm_hc4[5, c(1, 2, 4)][3],
          sm_vlm_hc4m[5, c(1, 2, 4)][3],
          sm_vlm_hc5[5, c(1, 2, 4)][3],
          N
        )
        ress[kk, 18] <- c(design)
      }
    }
  }
}

if (any(c(run_sim_multicore, run_sim_singlecore))) {
  colnames(ress) <- c(
    "lm", paste0("lm-", c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")), 
    "vlm", paste0("vlm-", c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")), 
    "N", "design"
  )
  rownames(ress) <- 1:NROW(ress)
  
  write.csv(ress, file = "output_data/factorial_study_design_simulation_results.csv")
}

tries <- 10000
ress <- read.csv("output_data/factorial_study_design_simulation_results.csv", 
                 row.names = NULL)[,-1]

ress |>
  filter(design %in% c("D1", "D2")) |>
  pivot_longer(!(N | design)) |>
  transform(xx = value < .05) |>
  group_by(name, N, design) |>
  summarise(power = mean(xx)) |>
  group_by(N, design) |>
  summarise("Największa moc" = max(power),
            "Nazwa najmocniejszego testu" = name[which.max(power)])

ress |>
  filter(design %in% c("D3", "D4")) |>
  pivot_longer(!(N | design)) |>
  transform(xx = value < .05) |>
  group_by(name, N, design) |>
  summarise("type1_error" = mean(xx)) |>
  group_by(N, design) |>
  summarise("Największy poziom błędu" = max(type1_error),
            "Nazwa testu o najwyższym poziomie błędu" = name[which.max(type1_error)],
            "Najmniejszy poziom błędu" = min(type1_error),
            "Nazwa testu o najmniejszym poziomie błędu" = name[which.min(type1_error)])

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
                     labels = c("Tylko warunkowa heteroskedastyczność (D1)", 
                                "Indywidualna i warunkowa heteroskedastyczność (D2)"), 
                     values=c("dodgerblue4", "firebrick4")) +
  scale_shape_manual(name = "Układ", 
                     labels = c("Tylko warunkowa heteroskedastyczność (D1)", 
                                "Indywidualna i warunkowa heteroskedastyczność (D2)"), 
                     values = 16:17) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = .3)) +
  scale_y_continuous(breaks = round(seq(0, .4, by = 0.04), 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "figures/empirical_power_in_factorial_study_design.png",
       height = 6,
       scale = 1.75)

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
                     labels = c("Tylko warunkowa heteroskedastyczność (D3)", 
                                "Indywidualna i warunkowa heteroskedastyczność (D4)"), 
                     values=c("dodgerblue4", "firebrick4")) +
  scale_shape_manual(name = "Układ", 
                     labels = c("Tylko warunkowa heteroskedastyczność (D3)", 
                                "Indywidualna i warunkowa heteroskedastyczność (D4)"), 
                     values = 16:17) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = .3)) +
  scale_y_continuous(breaks = round(seq(0, .15, by = .01), 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "figures/empirical_size_in_factorial_study_design.png",
       height = 6,
       scale = 1.75)

