#remotes::install_github("opisthokonta/goalmodel")
#install.packages(c("VGAM", "tidyverse", "gridExtra"))
library(tidyverse)
library(VGAM)
library(gridExtra)
library(goalmodel)
library(VennDiagram)

## data cleaning ####


# get those files from https://github.com/LukaszChrostowski/Football_Results_Predictions
load("processed_data.Rdata")
load("processed_data_averages_3.Rdata")

# Data cleaning

proccessed_data <- proccessed_data[!(proccessed_data$Sezon %in% c("2012/13", "2013/14")), ]
proccessed_data_averages_3 <- proccessed_data_averages_3[!(proccessed_data_averages_3$Sezon %in% c("2012/13", "2013/14")), ]

dfAnalysis <- proccessed_data_averages_3[, ((sapply(proccessed_data_averages_3, FUN = function(x) sum(!is.na(x)) / length(x)) > .95) |> which() |> names())[-c(3, 4, 5)]]
dfAnalysis$y1 <- proccessed_data$`Gole Gospodarz`
dfAnalysis$y2 <- proccessed_data$`Gole Gość`

colnames(dfAnalysis) <- c(
  "Home", "Away",
  paste0(rep(c(
    "formPossesion", "formCritical", "formAttempts", "formFailedAttempts",
    "formCorners", "formOffsides", "formGoalkeeperSaves", "formFauls",
    "formYellow", "formRed"), 2), 
    rep(c("Home", "Away"), each = 10)), "formScoresHome", "formScoresAway", 
  "Walkower", "lowerLeagueHome", "lowerLeagueAway", "formLosesHome", 
  "formDrawsHome", "formLosesAway", "formDrawsAway", "fromScoreLostHome", 
  "fromScoreLostAway", "scoreHome", "scoreAway"
)

dfAnalysis$formDrawsAway <- dfAnalysis$formDrawsAway |> as.numeric()
dfAnalysis$formDrawsHome <- dfAnalysis$formDrawsHome |> as.numeric()

dfAnalysis$formLosesAway <- dfAnalysis$formLosesAway |> as.numeric()
dfAnalysis$formLosesHome <- dfAnalysis$formLosesHome |> as.numeric()

dfAnalysis <- dfAnalysis[dfAnalysis$Walkower == 0, colnames(dfAnalysis) != "Walkower"]
dfAnalysis <- dfAnalysis[!(is.na(dfAnalysis) |> rowSums() > 0), ]

write.csv(dfAnalysis, "footballAnalysis.csv")

## analysis ####

dfAnalysis <- readr::read_csv("footballAnalysis.csv")[, -1]

plot <- dfAnalysis |> 
  mutate(scoreHome = factor(scoreHome, levels = 0:7),
         scoreAway = factor(scoreAway, levels = 0:7)) |>
  count(scoreHome, scoreAway, .drop = FALSE, name = "wt") |>
  ggplot(aes(y = scoreHome, x = scoreAway)) +
  geom_tile(aes(fill = wt), colour = "black") +
  scale_fill_continuous(low = "white", high = "red") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid = element_line(color = "white"),
        legend.key.size = unit(1, "cm")) +
  xlab("Wyniki gości") +
  ylab("Wyniki gospodarzy") +
  geom_text(aes(label = wt), color="black") +
  labs(fill = "Częsość występowania")

ggsave(filename = "football_częstości.png", plot)

set.seed(123)
chisq.test(x = dfAnalysis$scoreHome,
           y = dfAnalysis$scoreAway, 
           simulate.p.value = TRUE, B = 10^5)

#### indep bivariate poisson ####
model_biv_poisson_simple <- vglm(
  cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueHome - lowerLeagueAway, 
  family = poissonff(),
  data = dfAnalysis
)

constraintsList1 <- list(
  "(Intercept)"             = diag(2),
  "formPossesionHome"       = matrix(c(1, 0), ncol = 1),
  "formCriticalHome"        = matrix(c(1, 0), ncol = 1),
  "formAttemptsHome"        = matrix(c(1, 0), ncol = 1),
  "formFailedAttemptsHome"  = matrix(c(1, 0), ncol = 1),
  "formCornersHome"         = matrix(c(1, 0), ncol = 1),
  "formOffsidesHome"        = matrix(c(1, 0), ncol = 1),
  "formGoalkeeperSavesHome" = matrix(c(1, 0), ncol = 1),
  "formFaulsHome"           = matrix(c(1, 0), ncol = 1),
  "formYellowHome"          = matrix(c(1, 0), ncol = 1),
  "formRedHome"             = matrix(c(1, 0), ncol = 1),
  "formPossesionAway"       = matrix(c(0, 1), ncol = 1),
  "formCriticalAway"        = matrix(c(0, 1), ncol = 1),
  "formAttemptsAway"        = matrix(c(0, 1), ncol = 1),
  "formFailedAttemptsAway"  = matrix(c(0, 1), ncol = 1),
  "formCornersAway"         = matrix(c(0, 1), ncol = 1),
  "formOffsidesAway"        = matrix(c(0, 1), ncol = 1),
  "formGoalkeeperSavesAway" = matrix(c(0, 1), ncol = 1),
  "formFaulsAway"           = matrix(c(0, 1), ncol = 1),
  "formYellowAway"          = matrix(c(0, 1), ncol = 1),
  "formRedAway"             = matrix(c(0, 1), ncol = 1),
  "formLosesHome"           = matrix(c(1, 0), ncol = 1),
  "formDrawsHome"           = matrix(c(1, 0), ncol = 1),
  "formLosesAway"           = matrix(c(0, 1), ncol = 1),
  "formDrawsAway"           = matrix(c(0, 1), ncol = 1),
  "fromScoreLostHome"       = matrix(c(1, 0), ncol = 1),
  "fromScoreLostAway"       = matrix(c(0, 1), ncol = 1),
  "formScoresHome"          = matrix(c(1, 0), ncol = 1),
  "formScoresAway"          = matrix(c(0, 1), ncol = 1)
)

constraintsList2<- list(
  "(Intercept)"             = diag(2),
  "formPossesionHome"       = matrix(c(1, -1), ncol = 1),
  "formCriticalHome"        = matrix(c(1, -1), ncol = 1),
  "formAttemptsHome"        = matrix(c(1, -1), ncol = 1),
  "formFailedAttemptsHome"  = matrix(c(1, -1), ncol = 1),
  "formCornersHome"         = matrix(c(1, -1), ncol = 1),
  "formOffsidesHome"        = matrix(c(1, -1), ncol = 1),
  "formGoalkeeperSavesHome" = matrix(c(1, -1), ncol = 1),
  "formFaulsHome"           = matrix(c(1, -1), ncol = 1),
  "formYellowHome"          = matrix(c(1, -1), ncol = 1),
  "formRedHome"             = matrix(c(1, -1), ncol = 1),
  "formPossesionAway"       = matrix(c(-1, 1), ncol = 1),
  "formCriticalAway"        = matrix(c(-1, 1), ncol = 1),
  "formAttemptsAway"        = matrix(c(-1, 1), ncol = 1),
  "formFailedAttemptsAway"  = matrix(c(-1, 1), ncol = 1),
  "formCornersAway"         = matrix(c(-1, 1), ncol = 1),
  "formOffsidesAway"        = matrix(c(-1, 1), ncol = 1),
  "formGoalkeeperSavesAway" = matrix(c(-1, 1), ncol = 1),
  "formFaulsAway"           = matrix(c(-1, 1), ncol = 1),
  "formYellowAway"          = matrix(c(-1, 1), ncol = 1),
  "formRedAway"             = matrix(c(-1, 1), ncol = 1),
  "formLosesHome"           = matrix(c(1, -1), ncol = 1),
  "formDrawsHome"           = matrix(c(1, -1), ncol = 1),
  "formLosesAway"           = matrix(c(-1, 1), ncol = 1),
  "formDrawsAway"           = matrix(c(-1, 1), ncol = 1),
  "fromScoreLostHome"       = matrix(c(1, -1), ncol = 1),
  "fromScoreLostAway"       = matrix(c(-1, 1), ncol = 1),
  "formScoresHome"          = matrix(c(1, -1), ncol = 1),
  "formScoresAway"          = matrix(c(-1, 1), ncol = 1)
)

model_biv_poisson_constrained <- vglm(
  cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueHome - lowerLeagueAway, 
  family = poissonff(),
  data = dfAnalysis,
  constraints = constraintsList1
)

model_biv_poisson_constrained_alternative <- vglm(
  cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueHome - lowerLeagueAway, 
  family = poissonff(),
  data = dfAnalysis,
  constraints = constraintsList2
)

coef(model_biv_poisson_simple, matrix.out = TRUE)
coef(model_biv_poisson_constrained, matrix.out = TRUE)
coef(model_biv_poisson_constrained_alternative, matrix.out = TRUE)
# lrtest(model_biv_poisson_constrained,
#        model_biv_poisson_constrained_alternative)
# 
# lrtest(model_biv_poisson_simple, 
#        model_biv_poisson_constrained_alternative)
# 
# lrtest(model_biv_poisson_simple, 
#        model_biv_poisson_constrained)

cor_mat <- model_biv_poisson_simple |> 
  model.matrixvlm(type = "lm") |>
  cor()
cor_mat <- cor_mat[-1, -1]

dimnames(cor_mat) <- list(
  str_remove_all(rownames(cor_mat), 
                 paste(c("^form", "^from", "ome$", "way$"),
                       collapse = "|")),
  str_remove_all(rownames(cor_mat), 
                 paste(c("^form", "^from", "ome$", "way$"), 
                       collapse = "|"))
)

corrplot::corrplot(cor_mat, method = "number", tl.cex = .65, number.cex = .5)

cor_mat1 <- model_biv_poisson_simple |> vcov() |> cov2cor()
dimnames(cor_mat1) <- list(
  str_remove_all(rownames(cor_mat1), 
                 paste(c("^form", "^from", "ome", "ome", "way", "way"),
                       collapse = "|")),
  str_remove_all(rownames(cor_mat1), 
                 paste(c("^form", "^from", "ome", "ome", "way", "way"), 
                       collapse = "|"))
)

corrplot::corrplot(cor_mat1, tl.cex = .65, number.cex = .5)

# model_biv_poisson_sel_1 <- model_biv_poisson_sel
# model_biv_poisson_sel <- vglm(
#   cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueHome - 
#     lowerLeagueAway,
#   family = poissonff(),
#   data = dfAnalysis
# )
# AIC(model_biv_poisson_sel)
# AIC(model_biv_poisson_sel_1)
# AA <- anova(model_biv_poisson_sel)
# rownames(AA)[AA$`Pr(>Chi)` |> which.max()]

model_biv_poisson_sel <- vglm(
  cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueHome - 
    lowerLeagueAway - formCornersAway - formDrawsHome - formAttemptsHome -
    formRedHome - formCriticalHome - formOffsidesHome - formRedAway -
    formLosesHome - formFailedAttemptsAway - formFailedAttemptsHome -
    formYellowAway - formFaulsAway - formYellowHome - formDrawsAway -
    formLosesAway - formFaulsHome - formOffsidesAway - formGoalkeeperSavesAway,
  family = poissonff(),
  data = dfAnalysis
)

lrtest(model_biv_poisson_simple, model_biv_poisson_sel)

#### Dixon coles model ####

mydixoncolesff <- function() {
  new("vglmff",
      blurb = c("Dixon coles distribution\n",
                "Links:    ",
                "log(lambda1)", ", ", "log(lambda2)", ", ", "rhobit(rho)"),
      deviance = function(mu, y, w, residuals = FALSE, eta, extra = NULL, summation = TRUE) {
        #TODO:: This may not be needed
        # devy <- -log(y) - 1
        # devmu <- -log(mu) -y/mu
        # devi <- 2 * (devy - devmu)
        #
        # if (residuals) {
        #   sign(y - mu) * sqrt(abs(devi) * c(w))
        # } else {
        #   dev.elts <- c(w) * devi
        #   if (summation) sum(dev.elts) else dev.elts
        # }
        NULL
      },
      loglikelihood = function(mu, y, w, residuals = FALSE, eta, extra = NULL, summation = TRUE) {
        lambda1 <- eta2theta(
          eta[, 1, drop = FALSE], "loglink",
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0,
               short = TRUE, tag = FALSE)
        )
        lambda2 <- eta2theta(
          eta[, 2, drop = FALSE], "loglink",
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0,
               short = TRUE, tag = FALSE)
        )
        rho <- eta2theta(
          eta[, 3, drop = FALSE], "rhobitlink",
          list(theta = NULL, inverse = FALSE, deriv = 0,
               short = TRUE, tag = FALSE)
        )
        if (residuals) {
          stop("Not implemented")
        } else {
          tau <- ifelse(
            y[, 1] > 1 | y[, 2] > 1, 1,
            ifelse(
              y[, 1] == 1,
              ifelse(
                y[, 2] == 1, 1 - rho,
                1 + lambda2 * rho
              ),
              ifelse(
                y[, 2] == 1, 1 + lambda1 * rho,
                1 - lambda1 * lambda2 * rho
              )
            )
          )

          ll <- c(w) * (log(tau) +
                        dpois(x = y[, 1], lambda = lambda1, log = TRUE) +
                        dpois(x = y[, 2], lambda = lambda2, log = TRUE))

          if (summation) sum(ll) else ll
        }
        },
      initialize = expression({
        if (NCOL(y) != 2) stop("This model only works on bivariate arguments")

        if (criterion == "deviance") criterion <- "loglikelihood"
        extra$type.fitted <- "mean"
        extra$colnames.y <- colnames(y)

        extra$M1 <- M <- M1 <- 6
        extra$ncoly <- ncoly <- ncol(y)

        predictors.names <- c("log(lambda1)", "log(lambda2)", "rhobit(rho)")

        #change this
        # etastart <- cbind(log(pmax(1, y[,1])), log(pmax(1, y[,2])), 0, 0, 0,
        # rhobitlink(cor(y[,1], y[,2])))

        etastart <- cbind(
          log(pmax(1, y[, 1])),
          log(pmax(1, y[, 2])),
          rhobitlink(cor(y[, 1], y[, 2]))
        )

        etastart
      }),
      linkinv = function(eta, extra = NULL) {
        type.fitted <- if (length(extra$type.fitted))
          extra$type.fitted
        else {
          type.fitted <- "mean"
          warning("cannot find 'type.fitted'. ", "Returning the 'mean'.")
          "mean"
        }
        type.fitted <- match.arg(type.fitted, c("mean", "home_adv", "attack", "defence"))[1]
        lambda1 <- eta2theta(
          eta[, 1, drop = FALSE], "loglink",
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0,
               short = TRUE, tag = FALSE)
        )
        lambda2 <- eta2theta(
          eta[, 2, drop = FALSE], "loglink",
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0,
               short = TRUE, tag = FALSE)
        )
        rho <- eta2theta(
          eta[, 3, drop = FALSE], "rhobitlink",
          list(theta = NULL, inverse = FALSE, deriv = 0,
               short = TRUE, tag = FALSE)
        )

        mu <- cbind(lambda1, lambda2)
        colnames(mu) <- c("Home", "Away")

        gamma <- as.matrix(lambda1 / lambda2)
        colnames(gamma) <- "Home-advantage"

        ans <- switch(type.fitted,
          mean     = mu,
          home_adv = gamma,
          attack   = mu[, 1, drop = FALSE],
          defence  = mu[, 2, drop = FALSE]
        )

        ans
      },
      last = expression({
        misc$link <- rep(c("loglink", "rhobit"), c(2, 1))
      }),
      linkfun = function() NULL,
      vfamily = "Dixon_coles",
      deriv = expression({
        # values
        lambda1 <- eta2theta(
          eta[, 1, drop = FALSE], "loglink",
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0,
               short = TRUE, tag = FALSE)
        )
        lambda2 <- eta2theta(
          eta[, 2, drop = FALSE], "loglink",
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0,
               short = TRUE, tag = FALSE)
        )
        rho <- eta2theta(
          eta[, 3, drop = FALSE], "rhobitlink",
          list(theta = NULL, inverse = FALSE, deriv = 0,
               short = TRUE, tag = FALSE)
        )
        # derivatives of inv links
        dlambda1.deta <- dtheta.deta(lambda1, "loglink")
        dlambda2.deta <- dtheta.deta(lambda2, "loglink")
        drho.deta     <- dtheta.deta(rho,    "rhobitlink")

        tau <- ifelse(
          y[, 1] > 1 | y[, 2] > 1, 1,
          ifelse(
            y[, 1] == 1,
            ifelse(
              y[,2] == 1, 1 - rho,
              1 + lambda2 * rho
            ),
            ifelse(
              y[,2] == 1, 1 + lambda1 * rho,
              1 - lambda1 * lambda2 * rho
            )
          )
        )

        dl.dlambda1 <- y[,1] / lambda1 - 1 +
          ifelse(y[, 1] == 0 & y[, 2] == 0,
                 -rho * lambda2,
                 ifelse(y[, 1] == 0 & y[, 2] == 1,
                        rho, 0)) / tau

        dl.dlambda2 <- y[,2] / lambda2 - 1 +
          ifelse(y[, 1] == 0 & y[, 2] == 0,
                 -rho * lambda1,
                 ifelse(y[, 1] == 1 & y[, 2] == 0,
                        rho, 0)) / tau

        dl.drho <-
          ifelse(y[, 1] == 0 & y[, 2] == 0,
                 -lambda1 * lambda2,
                 ifelse(y[, 1] == 0 & y[, 2] == 1,
                        lambda1,
                        ifelse(y[, 1] == 1 & y[, 2] == 0,
                               lambda2,
                               ifelse(y[, 1] == 1 & y[, 2] == 1, -1, 0)))) / tau

        # first derivative w.r. to eta's
        c(w) * cbind(
          dl.dlambda1 * dlambda1.deta,
          dl.dlambda2 * dlambda2.deta,
          dl.drho    * drho.deta
        )
      }),
      weight = expression({
        wz <- matrix(0, n, 6)
        const1 <- exp(-(lambda1 + lambda2))

        #Ed2l.dlambda1.dlambda1
        wz[, iam(1, 1, 3)] <- -1 / lambda1 - const1 * (
          lambda2 * (rho ^ 2) / (1 + lambda1 * rho) + 
          (lambda2 * rho) ^ 2 / (1 - lambda1 * lambda2 * rho)
        )
        wz[, iam(1, 1, 3)] <- wz[, iam(1, 1, 3)] * dlambda1.deta ^ 2
        
        #Ed2l.dlambda1.dlambda2
        wz[, iam(1, 2, 3)] <- - const1 * (
          rho + (rho ^ 2) * lambda1 * lambda2 / 
          (1 - lambda1 * lambda2 * rho)
        )
        wz[, iam(1, 2, 3)] <- wz[, iam(1, 2, 3)] * dlambda1.deta * dlambda2.deta
        
        #Ed2l.dlambda1.drho
        wz[, iam(1, 3, 3)] <- - const1 * (
          rho * lambda1 * lambda2 / (1 + lambda1 * rho) +
          rho * lambda1 * lambda2 ^ 2 / (1 - lambda1 * lambda2 * rho)
        )
        wz[, iam(1, 3, 3)] <- wz[, iam(1, 3, 3)] * dlambda1.deta * drho.deta
        
        #Ed2l.dlambda2.dlambda2
        wz[, iam(2, 2, 3)] <- -1 / lambda2 - const1 * (
          lambda1 * rho ^ 2 / (1 + lambda2 * rho) +
          (lambda1 * rho) ^ 2 / (1 - lambda1 * lambda2 * rho)
        )
        wz[, iam(2, 2, 3)] <- wz[, iam(2, 2, 3)] * dlambda2.deta ^ 2
        
        #Ed2l.dlambda2.drho
        wz[, iam(2, 3, 3)] <- - const1 * (
          lambda1 ^ 2 * lambda2 * rho / (1 - lambda1 * lambda2 * rho) +
          lambda1 * lambda2 * rho / (1 + lambda2 * rho)
        )
        wz[, iam(2, 3, 3)] <- wz[, iam(2, 3, 3)] * dlambda2.deta * drho.deta
        
        
        #Ed2l.drho.drho
        wz[, iam(3, 3, 3)] <- - const1 * (
          lambda1 * lambda2 / (1 - lambda1 * lambda2 * rho) +
          lambda1 * lambda2 / (1 + lambda1 * rho) +
          lambda1 * lambda2 / (1 + lambda2 * rho) +
          lambda1 * lambda2 / (1 - rho)
        )
        wz[, iam(3, 3, 3)] <- wz[, iam(3, 3, 3)] * drho.deta ^ 2

        wz <- -wz
        # xx <- matrix(
        #   nrow = 3, ncol = 3,
        #   data = c(
        #     wz[1, iam(1, 1, 3)], wz[1, iam(1, 2, 3)], wz[1, iam(1, 3, 3)],
        #     wz[1, iam(2, 1, 3)], wz[1, iam(2, 2, 3)], wz[1, iam(2, 3, 3)],
        #     wz[1, iam(3, 1, 3)], wz[1, iam(3, 2, 3)], wz[1, iam(3, 3, 3)]
        #   )
        # )
        # print(xx)
        # print(det(xx[1:2, 1:2]))
        # print(det(xx[1:3, 1:3]))
        # print(rho[1,])
        # stop("abc")
        c(w) * wz
      })
  )
}

constraints_list_dixon_coles <- list(
  "(Intercept)" = diag(3),
  "formPossesionHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formCriticalHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formAttemptsHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formFailedAttemptsHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formCornersHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formOffsidesHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formGoalkeeperSavesHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formFaulsHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formYellowHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formRedHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formPossesionAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formCriticalAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formAttemptsAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formFailedAttemptsAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formCornersAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formOffsidesAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formGoalkeeperSavesAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formFaulsAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formYellowAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formRedAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formScoresHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formScoresAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formLosesHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formDrawsHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formLosesAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "formDrawsAway" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "fromScoreLostHome" = cbind(c(1, 0, 0), c(0, 1, 0)),
  "fromScoreLostAway" = cbind(c(1, 0, 0), c(0, 1, 0))
)

model <- vglm(
  cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueAway - lowerLeagueHome,
  family = mydixoncolesff(),
  data = dfAnalysis,
  control = vglm.control(trace = TRUE, criterion = "coefficients"),
  constraints = constraints_list_dixon_coles
)

AA <- anova(model, type = "II")

predict_res <- function(model, 
                        type = c("all", "home_win", "home_loss", "draw"),
                        data,
                        eps = 1e-6) {
  if (missing(type)) type <- "all"
  # lambdy
  if (missing(data)) {
    data <- predict(model, type = "link")
  } else {
    data <- predict(model, newdata = newdata, type = "link")
  }
  
  data[, 1:2] <- exp(data[, 1:2, drop = FALSE])
  data[,   3] <- rhobitlink(data[, 3, drop = FALSE], inverse = TRUE)
  
  prob_draw <- (besselI(2 * sqrt(data[,1] * data[,2]), nu = 0) - data[,1] * data[,2] - 1) * 
    exp(-(data[,1] + data[,2])) +
    (1 - data[,3]) * exp(-(data[,1] + data[,2])) * data[,1] * data[,2] +
    (1 - data[,1] * data[,2] * data[,3]) * exp(-(data[,1] + data[,2]))
  
  if (type == "draw") {
    return(cbind("draw" = draw))
  }
  
  prob_home_or_draw <- exp(-(data[,1] + data[,2])) * (
    (1 - data[,2] * data[,1] * data[,3]) +
    (1 + data[,2] * data[,3]) * data[,1] +
    (1 - data[,3]) * data[,1] * data[,2]
  )
  
  ll <- rep(TRUE, NROW(data))
  xx <- 2
  while (any(ll)) {
    toAdd <- data[, 1] ^ xx * (exp(-data[, 1]) / factorial(xx)) * ppois(q = xx, lambda = data[, 2])
    prob_home_or_draw <- prob_home_or_draw + toAdd
    ll <- toAdd > eps
    xx <- xx + 1
  }
  prob_home <- prob_home_or_draw - prob_draw
  prob_away <- 1 - prob_home_or_draw
  # "all", "home_win", "home_loss", "draw"
  switch (type,
    "all" = cbind(
        "home_win" = prob_home,
        "draw" = prob_draw,
        "away_win" = prob_away
      ),
    "home_win" = cbind("home_win" = prob_home),
    "home_loss" = cbind("home_loss" = prob_away)
  )
}

probs <- predict_res(model)

# model_sel_1 <- model_sel
# model_sel <- vglm(
#   cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueAway - 
#     lowerLeagueHome - formCornersAway - formDrawsHome - formAttemptsHome - 
#     formRedHome - formCriticalHome - formOffsidesHome - formRedAway - 
#     formLosesHome - formFailedAttemptsAway - formFailedAttemptsHome -
#     formYellowAway - formDrawsAway - formLosesAway - formFaulsAway -
#     formYellowHome - formFaulsHome - formOffsidesAway,
#   family = mydixoncolesff(),
#   data = dfAnalysis,
#   control = vglm.control(trace = TRUE, criterion = "coefficients"),
#   constraints = constraints_list_dixon_coles
# )
# AIC(model_sel)
# AIC(model_sel_1)
# anova(model_sel)

model_sel <- vglm(
  cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueAway - 
    lowerLeagueHome - formCornersAway - formDrawsHome - formAttemptsHome - 
    formRedHome - formCriticalHome - formOffsidesHome - formRedAway - 
    formLosesHome - formFailedAttemptsAway - formFailedAttemptsHome -
    formYellowAway - formDrawsAway - formLosesAway - formFaulsAway -
    formYellowHome - formFaulsHome - formOffsidesAway,
  family = mydixoncolesff(),
  data = dfAnalysis,
  control = vglm.control(trace = TRUE, criterion = "coefficients"),
  constraints = constraints_list_dixon_coles
)
summary(model_sel)

lrtest(model, model_sel)
lrtest(model_biv_poisson_simple, model_biv_poisson_sel)

# test with biv poisson model
test_stat <- 2 * (logLik(model_sel) - logLik(model_biv_poisson_sel))
pchisq(
  test_stat, 
  df = length(coef(model_sel)) - length(coef(model_biv_poisson_sel)),
  lower.tail = FALSE
)

probs <- predict_res(model_sel)

df <- data.frame(
  dfAnalysis[, c("Home", "Away", "scoreHome", "scoreAway")],
  probs,
  res = ifelse(
    dfAnalysis$scoreHome > dfAnalysis$scoreAway, 1,
    ifelse(dfAnalysis$scoreHome == dfAnalysis$scoreAway, 2, 3)
  ) |> factor()
)

# true dixon-coles model

true_dc_model <- goalmodel(
  goals1 = dfAnalysis$scoreHome, goals2 = dfAnalysis$scoreAway,
  team1 = dfAnalysis$Home, team2 = dfAnalysis$Away,
  dc = TRUE
)

summary(true_dc_model)

true_dc_model_covariates <- goalmodel(
  goals1 = dfAnalysis$scoreHome, goals2 = dfAnalysis$scoreAway,
  team1 = dfAnalysis$Home, team2 = dfAnalysis$Away,
  dc = TRUE,
  x1 = model.matrix(
    ~ . - 1, 
    dfAnalysis[,colnames(dfAnalysis)[grepl(pattern = "Home$", x = colnames(dfAnalysis))][-c(1, 13, 17)]]
  ),
  x2 = model.matrix(
    ~ . - 1,
    dfAnalysis[,colnames(dfAnalysis)[grepl(pattern = "Away$", x = colnames(dfAnalysis))][-c(1, 13, 17)]]
  )
)

true_dc_model_covariates_sel <- goalmodel(
  goals1 = dfAnalysis$scoreHome, goals2 = dfAnalysis$scoreAway,
  team1 = dfAnalysis$Home, team2 = dfAnalysis$Away,
  dc = TRUE,
  x1 = model.matrix(
    ~ . - 1 - formDrawsHome - formAttemptsHome - formRedHome - 
      formCriticalHome - formOffsidesHome - formLosesHome - 
      formFailedAttemptsHome - formYellowHome - formFaulsHome, 
    dfAnalysis[,colnames(dfAnalysis)[grepl(pattern = "Home$", x = colnames(dfAnalysis))][-c(1, 13, 17)]]
  ),
  x2 = model.matrix(
    ~ . - 1 - formCornersAway - formRedAway - formFailedAttemptsAway -
      formYellowAway - formDrawsAway - formLosesAway - formFaulsAway - 
      formOffsidesAway,
    dfAnalysis[,colnames(dfAnalysis)[grepl(pattern = "Away$", x = colnames(dfAnalysis))][-c(1, 13, 17)]]
  )
)

true_dc_model_covariates_sel$aic
true_dc_model$aic
true_dc_model_covariates$aic

# pchisq(2 * (true_dc_model_covariates_sel$loglikelihood - true_dc_model$loglikelihood),
#       df = 21, lower.tail = FALSE)

res <- predict_result(
  true_dc_model_covariates_sel,
  team1 = dfAnalysis$Home, team2 = dfAnalysis$Away,
  x1 = model.matrix(
    ~ . - 1 - formDrawsHome - formAttemptsHome - 
      formRedHome - formCriticalHome - formOffsidesHome - formLosesHome - 
      formFailedAttemptsHome - formYellowHome - formFaulsHome, 
    dfAnalysis[,colnames(dfAnalysis)[grepl(pattern = "Home$", x = colnames(dfAnalysis))][-c(1, 13, 17)]]
  ),
  x2 = model.matrix(
    ~ . - 1 - formFailedAttemptsAway - formYellowAway - 
      formAttemptsAway - formFaulsAway - formDrawsAway - formLosesAway - 
      formGoalkeeperSavesAway -formCornersAway - formOffsidesAway - 
      fromScoreLostAway - formScoresAway,
    dfAnalysis[,colnames(dfAnalysis)[grepl(pattern = "Away$", x = colnames(dfAnalysis))][-c(1, 13, 17)]]
  ),
  return_df = TRUE
)

res2 <- predict_result(
  true_dc_model,
  team1 = dfAnalysis$Home, team2 = dfAnalysis$Away,
  return_df = TRUE
)

res <- cbind(res, res = ifelse(
  dfAnalysis$scoreHome > dfAnalysis$scoreAway, 1,
  ifelse(dfAnalysis$scoreHome == dfAnalysis$scoreAway, 2, 3)
) |> factor())

res[, 3:5] |> apply(
  MARGIN = 1, FUN = which.max
)

(p_obs <- c(
  sum(dfAnalysis$scoreHome >  dfAnalysis$scoreAway),
  sum(dfAnalysis$scoreHome == dfAnalysis$scoreAway),
  sum(dfAnalysis$scoreHome <  dfAnalysis$scoreAway)
) / NROW(dfAnalysis))

cross_entropy_dc_true <- 
  (p_obs[1] * log(res[, 3])) * (dfAnalysis$scoreHome >  dfAnalysis$scoreAway) +
  (p_obs[2] * log(res[, 4])) * (dfAnalysis$scoreHome == dfAnalysis$scoreAway) +
  (p_obs[3] * log(res[, 5])) * (dfAnalysis$scoreHome <  dfAnalysis$scoreAway)

cross_entropy_dc_true_2 <- 
  (p_obs[1] * log(res2[, 3])) * (dfAnalysis$scoreHome >  dfAnalysis$scoreAway) +
  (p_obs[2] * log(res2[, 4])) * (dfAnalysis$scoreHome == dfAnalysis$scoreAway) +
  (p_obs[3] * log(res2[, 5])) * (dfAnalysis$scoreHome <  dfAnalysis$scoreAway)

cross_entropy_dc_vgam <- 
  (p_obs[1] * log(df[, 5])) * (dfAnalysis$scoreHome >  dfAnalysis$scoreAway) +
  (p_obs[2] * log(df[, 6])) * (dfAnalysis$scoreHome == dfAnalysis$scoreAway) +
  (p_obs[3] * log(df[, 7])) * (dfAnalysis$scoreHome <  dfAnalysis$scoreAway)

c(
  "true dc covariate selection" = -sum(cross_entropy_dc_true), 
  "true dc" = -sum(cross_entropy_dc_true_2), 
  "vgam" = -sum(cross_entropy_dc_vgam)
)

zmienne_dc_sel <- c(colnames(model.matrix(
  ~ . - 1 - formDrawsHome - formAttemptsHome - 
    formRedHome - formCriticalHome - formOffsidesHome - formLosesHome - 
    formFailedAttemptsHome - formYellowHome - formFaulsHome, 
  dfAnalysis[,colnames(dfAnalysis)[grepl(pattern = "Home$", x = colnames(dfAnalysis))][-c(1, 13, 17)]]
)), colnames(model.matrix(
  ~ . - 1 - formFailedAttemptsAway - formYellowAway - 
    formAttemptsAway - formFaulsAway - formDrawsAway - formLosesAway - 
    formGoalkeeperSavesAway -formCornersAway - formOffsidesAway - 
    fromScoreLostAway - formScoresAway,
  dfAnalysis[,colnames(dfAnalysis)[grepl(pattern = "Away$", x = colnames(dfAnalysis))][-c(1, 13, 17)]]
)))

zmienne_vgam_sel <- (Coef(model_sel, matrix = TRUE) |> rownames())[-1]
zmienne <- dfAnalysis |> colnames()
zmienne_dc_sel <- c("Home", "Away", zmienne_dc_sel)

v <- venn.diagram(
  list(
    zmienne,
    zmienne_vgam_sel,
    zmienne_dc_sel
  ),
  fill = c("white", "blue", "orange"),
  alpha = c(0.5, 0.5, 0.5), 
  cex = .5,
  filename = NULL,
  disable.logging = TRUE,
  resolution = 100,
  main = "Wszystkie zgromadzone zmienne objaśniające",
  category.names = c("", "", "")
)

grid.newpage()
grid.draw(v)

xx <- setdiff(zmienne, union(zmienne_vgam_sel, zmienne_dc_sel))
xx[5:(length(xx) + 1)] <- xx[4:length(xx)]
xx[4] <- "\n"
xx[9:(length(xx) + 1)] <- xx[8:length(xx)]
xx[8] <- "\n"
xx[13:(length(xx) + 1)] <- xx[12:length(xx)]
xx[12] <- "\n"
xx[17:(length(xx) + 1)] <- xx[16:length(xx)]
xx[16] <- "\n"
xx[21:(length(xx) + 1)] <- xx[20:length(xx)]
xx[20] <- "\n"

v[[7]]$label <- paste(c("Zmienne tylko oryginalnym\nmodelu Dixona-Coles'a:\n\n", setdiff(zmienne_dc_sel, zmienne_vgam_sel)), collapse = "\n")
v[[8]]$label  <- paste(c("Zmienne poza oboma modelami:\n\n", paste(xx, collapse = " ")), collapse = " ")
v[[10]]$label <- paste(c("Zmienne w testowym modelu,\nale nie w oryginalnym:\n\n", setdiff(zmienne_vgam_sel, zmienne_dc_sel)), collapse = "\n")
v[[9]]$label <- paste(c("Zmienne wspólne:\n\n", intersect(zmienne_vgam_sel, zmienne_dc_sel)), collapse = "\n")

grid.newpage()
grid.draw(v)
