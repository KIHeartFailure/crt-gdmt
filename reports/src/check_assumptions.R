source(here::here("setup/setup.R"))

# load data
load(here("data/clean-data/data.RData"))

dataass <- mice::complete(imprsdata, 3)
dataass <- mice::complete(imprsdata, 6)

# For log reg models

## bbl

modvarstmp <- c(modvars, "sos_lm_bbl1_3", "sos_lm_rasiarni1_3", "sos_lm_mra1_3", "sos_lm_loop1_3")

logreg <- glm(formula(paste0("bbldiff_cat2 == 'Increase' ~ crt + ", paste(modvarstmp, collapse = " + "))),
  family = binomial(link = "logit"), data = rsdata
)

# vif
print(car::vif(logreg))

# outliers
cooks <- cooks.distance(logreg)
plot(cooks)
abline(h = 4 / nrow(dataass), lty = 2, col = "red") # add cutoff line

# rasiarni

logreg <- glm(formula(paste0("rasiarnidiff_cat2 == 'Increase' ~ crt + ", paste(modvarstmp, collapse = " + "))),
  family = binomial(link = "logit"), data = rsdata
)

# vif
print(car::vif(logreg))

# outliers
cooks <- cooks.distance(logreg)
plot(cooks)
abline(h = 4 / nrow(dataass), lty = 2, col = "red") # add cutoff line


# mra

logreg <- glm(formula(paste0("mradiff_cat2 == 'Increase' ~ crt + ", paste(modvarstmp, collapse = " + "))),
  family = binomial(link = "logit"), data = rsdata
)

# vif
print(car::vif(logreg))

# outliers
cooks <- cooks.distance(logreg)
plot(cooks)
abline(h = 4 / nrow(dataass), lty = 2, col = "red") # add cutoff line
