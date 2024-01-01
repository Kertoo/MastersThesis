library(tidyverse)
library(singleRcapture)

load("~/Desktop/policja_cleaned.RData")

colnames(policja) <- c("year_cap", "id", "gender", "year_born",
                       "country", "captures", "country_gr", "age")

policja$gender <- policja$gender %>% as.factor()
policja$country_gr <- ifelse(policja$country_gr == "POLSKA",
                             "POLAND",
                             ifelse(policja$country_gr == "UKRAINA",
                                    "UKRAINE",
                                    "OTHER")) %>% as.factor()

summary(policja)

policja %>%
  ggplot(aes(y = captures, col = age)) +
  ylim(as.character(1:12)) +
  geom_jitter(aes(x = age), height = .1) +
  facet_wrap(~ gender + country_gr) + 
  theme_bw()


simple_model <- estimatePopsize(
  captures ~ gender + age + country_gr,
  data = policja,
  model = "ztpoisson",
  method = "IRLS"
)


complex_model <- estimatePopsize(
  captures ~ gender * age + country_gr,
  data = policja,
  model = "oiztgeom",
  method = "IRLS",
  controlModel = controlModel(omegaFormula = ~ gender + age),
  controlMethod = controlMethod(verbose = 5, 
                                stepsize = .1, 
                                momentumFactor = 1,
                                saveIRLSlogs = TRUE)
)

summary(simple_model)
summary(complex_model)

comp <- sapply(list("simple"  = simple_model, 
                    "complex" = complex_model), 
               FUN = function(x) c(AIC(x), BIC(x)))
rownames(comp) <- c("AIC", "BIC")

comp

stratifyPopsize(complex_model, ~ gender / age)
