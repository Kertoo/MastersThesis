library(tidyverse)
library(VGAM)

load("~/Desktop/MastersThesis/processed_data.Rdata")
load("~/Desktop/MastersThesis/processed_data_averages_3.Rdata")


# testing for independence
# WTF??
chisq.test(
  x = proccessed_data$`Gole Gość`, 
  y = proccessed_data$`Gole Gospodarz`, 
  simulate.p.value = TRUE#, B = 10^7
)

# Data cleaning

df <- proccessed_data
dff <- proccessed_data_averages_3

df <- df[df$Sezon != "2012/13", ]
df <- df[df$Sezon != "2013/14", ]
dff <- dff[dff$Sezon != "2012/13", ]
dff <- dff[dff$Sezon != "2013/14", ]

dfAnalysis <- dff[, ((sapply(dff, FUN = function(x) sum(!is.na(x)) / length(x)) > .95) %>% which %>% names)[-c(3, 4, 5)]]
dfAnalysis$y1 <- df$`Gole Gospodarz`
dfAnalysis$y2 <- df$`Gole Gość`
colnames(dfAnalysis) <- c("Home", "Away",
sapply(c("Home", "Away"), function(x) {
  paste0(c("formPossesion", "formCritical", "formAttempts", "formFailedAttempts",
           "formCorners", "formOffsides", "formGoalkeeperSaves", "formFauls",
           "formYellow", "formRed"), x)
}) %>% as.vector, "formScoresHome", "formScoresAway", "Walkower",
"lowerLeagueHome", "lowerLeagueAway", "formLosesHome", "formDrawsHome",
"formLosesAway", "formDrawsAway", "fromScoreLostHome", "fromScoreLostAway",
"scoreHome", "scoreAway")

dfAnalysis$formDrawsAway <- dfAnalysis$formDrawsAway %>% as.numeric()
dfAnalysis$formDrawsHome <- dfAnalysis$formDrawsHome %>% as.numeric()

dfAnalysis$formLosesAway <- dfAnalysis$formLosesAway %>% as.numeric()
dfAnalysis$formLosesHome <- dfAnalysis$formLosesHome %>% as.numeric()

dfAnalysis <- dfAnalysis[dfAnalysis$Walkower == 0, colnames(dfAnalysis) != "Walkower"]
dfAnalysis <- dfAnalysis[!(is.na(dfAnalysis) %>% rowSums() > 0), ]

chisq.test(
  x = dfAnalysis$scoreHome,
  y = dfAnalysis$scoreAway, 
  simulate.p.value = TRUE, B = 10^7
)

model1 <- vglm(
  cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueHome - lowerLeagueAway, 
  family = poissonff(),
  data = dfAnalysis,
  constraints = list(
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
    "formPossesionAway"       = matrix(-c(1, -1), ncol = 1),
    "formCriticalAway"        = matrix(-c(1, -1), ncol = 1),
    "formAttemptsAway"        = matrix(-c(1, -1), ncol = 1),
    "formFailedAttemptsAway"  = matrix(-c(1, -1), ncol = 1),
    "formCornersAway"         = matrix(-c(1, -1), ncol = 1),
    "formOffsidesAway"        = matrix(-c(1, -1), ncol = 1),
    "formGoalkeeperSavesAway" = matrix(-c(1, -1), ncol = 1),
    "formFaulsAway"           = matrix(-c(1, -1), ncol = 1),
    "formYellowAway"          = matrix(-c(1, -1), ncol = 1),
    "formRedAway"             = matrix(-c(1, -1), ncol = 1),
    "formLosesHome"           = matrix(c(1, -1), ncol = 1),
    "formDrawsHome"           = matrix(c(1, -1), ncol = 1),
    "formLosesAway"           = matrix(-c(1, -1), ncol = 1),
    "formDrawsAway"           = matrix(-c(1, -1), ncol = 1),
    "fromScoreLostHome"       = matrix(c(1, -1), ncol = 1),
    "fromScoreLostAway"       = matrix(-c(1, -1), ncol = 1),
    "formScoresHome"          = matrix(c(1, -1), ncol = 1),
    "formScoresAway"          = matrix(-c(1, -1), ncol = 1)
  )
)

model2 <- vglm(
  cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueHome - lowerLeagueAway, 
  family = poissonff(),
  data = dfAnalysis,
  constraints = list(
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
    "formScoresAway"          = matrix(c(0, -1), ncol = 1)
  )
)

coef(model1, matrix.out = TRUE)

model3 <- vglm(
  cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueHome - lowerLeagueAway, 
  family = negbinomial(zero = NULL),
  data = dfAnalysis,
  constraints = list(
    "(Intercept)"             = matrix(c(1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1), ncol = 3, byrow = TRUE),
    "formPossesionHome"       = matrix(c(1, 0, -1, 0), ncol = 1),
    "formCriticalHome"        = matrix(c(1, 0, -1, 0), ncol = 1),
    "formAttemptsHome"        = matrix(c(1, 0, -1, 0), ncol = 1),
    "formFailedAttemptsHome"  = matrix(c(1, 0, -1, 0), ncol = 1),
    "formCornersHome"         = matrix(c(1, 0, -1, 0), ncol = 1),
    "formOffsidesHome"        = matrix(c(1, 0, -1, 0), ncol = 1),
    "formGoalkeeperSavesHome" = matrix(c(1, 0, -1, 0), ncol = 1),
    "formFaulsHome"           = matrix(c(1, 0, -1, 0), ncol = 1),
    "formYellowHome"          = matrix(c(1, 0, -1, 0), ncol = 1),
    "formRedHome"             = matrix(c(1, 0, -1, 0), ncol = 1),
    "formPossesionAway"       = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formCriticalAway"        = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formAttemptsAway"        = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formFailedAttemptsAway"  = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formCornersAway"         = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formOffsidesAway"        = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formGoalkeeperSavesAway" = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formFaulsAway"           = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formYellowAway"          = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formRedAway"             = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formLosesHome"           = matrix(c(1, 0, -1, 0), ncol = 1),
    "formDrawsHome"           = matrix(c(1, 0, -1, 0), ncol = 1),
    "formLosesAway"           = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formDrawsAway"           = matrix(-c(1, 0, -1, 0), ncol = 1),
    "fromScoreLostHome"       = matrix(c(1, 0, -1, 0), ncol = 1),
    "fromScoreLostAway"       = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formScoresHome"          = matrix(c(1, 0, -1, 0), ncol = 1),
    "formScoresAway"          = matrix(-c(1, 0, -1, 0), ncol = 1)
  )
)

model4 <- vglm(
  cbind(scoreHome, scoreAway) ~ . - Home - Away - lowerLeagueHome - lowerLeagueAway, 
  family = negbinomial(zero = NULL),
  data = dfAnalysis,
  constraints = list(
    "(Intercept)"             = diag(4),
    "formPossesionHome"       = matrix(c(1, 0, -1, 0), ncol = 1),
    "formCriticalHome"        = matrix(c(1, 0, -1, 0), ncol = 1),
    "formAttemptsHome"        = matrix(c(1, 0, -1, 0), ncol = 1),
    "formFailedAttemptsHome"  = matrix(c(1, 0, -1, 0), ncol = 1),
    "formCornersHome"         = matrix(c(1, 0, -1, 0), ncol = 1),
    "formOffsidesHome"        = matrix(c(1, 0, -1, 0), ncol = 1),
    "formGoalkeeperSavesHome" = matrix(c(1, 0, -1, 0), ncol = 1),
    "formFaulsHome"           = matrix(c(1, 0, -1, 0), ncol = 1),
    "formYellowHome"          = matrix(c(1, 0, -1, 0), ncol = 1),
    "formRedHome"             = matrix(c(1, 0, -1, 0), ncol = 1),
    "formPossesionAway"       = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formCriticalAway"        = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formAttemptsAway"        = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formFailedAttemptsAway"  = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formCornersAway"         = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formOffsidesAway"        = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formGoalkeeperSavesAway" = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formFaulsAway"           = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formYellowAway"          = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formRedAway"             = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formLosesHome"           = matrix(c(1, 0, -1, 0), ncol = 1),
    "formDrawsHome"           = matrix(c(1, 0, -1, 0), ncol = 1),
    "formLosesAway"           = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formDrawsAway"           = matrix(-c(1, 0, -1, 0), ncol = 1),
    "fromScoreLostHome"       = matrix(c(1, 0, -1, 0), ncol = 1),
    "fromScoreLostAway"       = matrix(-c(1, 0, -1, 0), ncol = 1),
    "formScoresHome"          = matrix(c(1, 0, -1, 0), ncol = 1),
    "formScoresAway"          = matrix(-c(1, 0, -1, 0), ncol = 1)
  ),
  control = vglm.control(trace = TRUE, maxit = 100, stepsize = .5, epsilon = .Machine$double.eps)
)