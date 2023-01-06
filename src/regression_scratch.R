
# Imports -----------------------------------------------------------------

library(estimatr)
library(tidyverse)
library(glmnet)
library(glmnetUtils)
library(modelsummary)
library(tidymodels)


# Functions ---------------------------------------------------------------

# This is a wrapper function for doing LASSO with defaults for cross-validation
lasso <- function(f, data, weights) {
  mask <- subset(data, select = c(attr(terms(f), "term.labels"))) |>
    complete.cases()
  masked_data <- data[mask, ]
  # NOTE: this is assigning globally, but I wasn't able to get it working
  # with normal scoping for reasons I couldn't really process.
  masked_w <<- weights[mask]

  assertthat::are_equal(dim(masked_data)[1], length(masked_w))

  mat <- model.frame(formula = f, data = masked_data, weights = masked_w)
  y <- mat[, 1]
  x <- mat[, 2:(dim(mat)[2] - 1)] |> as.matrix()
  w <- mat[, dim(mat)[2]]

  lasso_cv <- cv.glmnet(y = y, x = x, weights = w)

  lambda <- lasso_cv$lambda.min
  out <- glmnet(y = y, x = x, lambda = lambda, alpha = 1)

  return(out)
}


# Custom tidier function for modelsummary -
# when presenting LASSO results, use "---" to indicate which variables have
# had their coefficients set to zero
tidy_custom.glmnet <- function(x, ...) {
  out <- data.frame(term = names(coef(x)[, 1]),
                    estimate = ifelse(coef(x)[, 1] == 0,
                                      "---",
                                      as.character(round(coef(x)[, 1], 3))),
                    p.value = NA_real_,
                    std.error = NA_real_)
  return(out)
}

# Data loading and preprocessing ------------------------------------------

load("data/stefan_sorting_data.RData")

# rename var
anes16$educ <- anes16$education

anes20_rep <- filter(anes20, rep == 1)
anes20_dem <- filter(anes20, dem == 1)
anes20_ind <- filter(anes20, ind == 1)

anes16_rep <- filter(anes16, rep == 1)
anes16_dem <- filter(anes16, dem == 1)
anes16_ind <- filter(anes16, ind == 1)


# Table A.4 --------------------  -------------------------------------------

# NOTE: the results will differ because because of a change in weighting -
# the ANES 2020s result were all accidentally unweighted in the manuscript.

# these models all take the form DV ~ consistent set of predictors
# so define the predictors, then use those to create the formulae
predictors <- . ~ sorting_r + age + rep + ind + male + urban + smalltown +
  rural + income + educ + ideo + union + gun + married + gay + bi +
  children + RR + AUTH + religscale + bornagain + white + know_scale +
  news_fox + news_cnn + news_msnbc + facebook + twitter + instagram + reddit +
  youtube + snapchat + tiktok + sm_other

formulae <- list(
  "Dissatisfaction with Democracy" = update.formula(predictors, dem1 ~ .),
  "Checks and balances aren't important" = update.formula(predictors, dem2 ~ .),
  "Helpful if president could act alone" = update.formula(predictors, dem3 ~ .),
  "Support for political violence" = update.formula(predictors, vio_justy ~ .),
  "Skeptical of election integrity" = update.formula(predictors, fairelec ~ .),
  "Compromise is selling out" = update.formula(predictors, comp1.1 ~ .),
  "Prefer leader sticks to principles" = update.formula(predictors, comp2.1 ~ .),
  "Participation" = update.formula(predictors, MASONINDEX1 ~ .),
  "Affective polarization" = update.formula(predictors, ftdifference ~ .)
)

models <- lapply(formulae, \(f)(lm_robust(f, data=anes20, anes20$weight)))

# using modelsummary instead of stargazer. three reasons:
# 1. it's a bit more flexible and has some nicer defaults
# 2. it takes estimatr::lm_robust objects directly so you don't have to pass the
#    SEs separately
# 3. we can put LASSO and OLS results side-by-side if we wanted
coef_mapper <- c(
    "sorting_r" = "Sorting",
    "age" = "Age",
    "rep" = "Republican",
    "ind" = "Independent",
    "male" = "Male",
    "urban" = "Urban",
    "smalltown" = "Small town",
    "rural" = "Rural",
    "income" = "Income",
    "educ" = "Education",
    "ideo" = "Ideology",
    "union" = "Union Membership",
    "gun" = "Gun Ownership",
    "married" = "Marital Status",
    "gay" = "Gay",
    "bi" = "Bisexual",
    "children" = "Has Children",
    "RR" = "Racial Resentment",
    "AUTH" = "Authoritarianism",
    "religscale" = "Religiosity Scale",
    "bornagain" = "Born Again",
    "white" = "White",
    "know_scale" = "Political Knowledge",
    "panel" = "Panel respondent",
    "news_fox" = "TV-Fox",
    "news_cnn" = "TV-MSNBC",
    "news_msnbc" = "TV-CNN",
    "facebook" = "Facebook",
    "twitter" = "Twitter",
    "instagram" = "Instagram",
    "youtube" = "YouTube",
    "reddit" = "Reddit",
    "snapchat" = "Snapchat",
    "tiktok" = "TikTok",
    "sm_other" = "Other SM"
  )

modelsummary(models,
  coef_rename = coef_mapper,
  output = "results/new_version/a4.html",
  fmt = 2
  )

lasso_models <- lapply(formulae,
  \(f)(lasso(f, data=anes20, weights=anes20$weight)))

modelsummary(lasso_models,
  coef_map = coef_mapper,
  output = "results/new_version/a4_lasso.html",
  statistic = NULL,
  estimate = "estimate",
  fmt = as.character,
  return_zeros = TRUE
  )

# Table A.1 ----------------------------------------------------------------

sorting_formula <- sorting_r  ~ female + age + rep + ind + urban + smalltown +
  rural + income + educ + ideo + union + gun + gay + bi +  children + RR +
  AUTH + religscale + bornagain + white + facebook + twitter + instagram +
  reddit + youtube + snapchat + tiktok + sm_other + news_msnbc + news_cnn +
  news_fox + know_scale

# substitute out 2020-only terms
sorting_formula_16 <- update.formula(sorting_formula, ~ . -
                                       urban - smalltown - rural -
                                       facebook - twitter - instagram -
                                       reddit - snapchat - tiktok - sm_other -
                                       youtube + facebook_tw)

sorting_2020 <- lm_robust(sorting_formula,
  data = anes20,
  weights = anes20$weight)
sorting_2016 <- lm_robust(sorting_formula_16,
  data = anes16,
  weights = anes16$weight)
sorting_2020_lasso <- lasso(sorting_formula,
  data = anes20,
  weights = anes20$weight)
sorting_2016_lasso <- lasso(sorting_formula_16,
  data = anes16,
  weights = anes16$weight)

models <- list(
  "2016 Sorting (OLS)" = sorting_2016,
  "2016 Sorting (LASSO)" = sorting_2016_lasso,
  "2020 Sorting (OLS)" = sorting_2020,
  "2020 Sorting (LASSO)" = sorting_2020_lasso
)

coef_mapper <- c(
    "age" = "Age",
    "rep" = "Republican",
    "ind" = "Independent",
    "female" = "Female",
    "urban" = "Urban",
    "smalltown" = "Small town",
    "rural" = "Rural",
    "income" = "Income",
    "educ" = "Education",
    "ideo" = "Ideology",
    "gay" = "Gay",
    "bi" = "Bisexual",
    "RR" = "Racial Resentment",
    "AUTH" = "Authoritarianism",
    "religscale" = "Religiosity Scale",
    "bornagain" = "Born Again",
    "white" = "White",
    "know_scale" = "Political Knowledge",
    "news_fox" = "TV-Fox",
    "news_cnn" = "TV-MSNBC",
    "news_msnbc" = "TV-CNN",
    "facebook_tw" = "Facebook",
    "facebook" = "Facebook",
    "twitter" = "Twitter",
    "snapchat" = "Snapchat",
    "reddit" = "Reddit",
    "tiktok" = "TikTok",
    "sm_other" = "Other SM",
    "youtube" = "YouTube",
    "union" = "Union Membership",
    "gun" = "Gun Ownership",
    "children" = "Has Children"
  )


modelsummary(models,
  coef_rename = coef_mapper,
  stars = TRUE,
  output = "results/new_version/a1.html",
  fmt = 2,
  return_zeros = TRUE,
  notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)


# Tables A.2 and A.3 ---------------------------------------------------------------

sorting_formula_minus_pid <- update.formula(sorting_formula, ~ . - rep - ind)
sorting_formula_16_minus_pid <- update.formula(sorting_formula_16, ~ . - rep - ind)

sorting_2020_rep <- lm_robust(sorting_formula_minus_pid,
                              data = anes20_rep,
                              weights = anes20_rep$weight)
sorting_2020_rep_lasso <- lasso(sorting_formula_minus_pid,
                              data = anes20_rep,
                              weights = anes20_rep$weight)

sorting_2020_ind <- lm_robust(sorting_formula_minus_pid,
                              data = anes20_ind,
                              weights = anes20_ind$weight)
sorting_2020_ind_lasso <- lasso(sorting_formula_minus_pid,
                              data = anes20_ind,
                              weights = anes20_ind$weight)

sorting_2020_dem <- lm_robust(sorting_formula_minus_pid,
                              data = anes20_dem,
                              weights = anes20_dem$weight)
sorting_2020_dem_lasso <- lasso(sorting_formula_minus_pid,
                              data = anes20_dem,
                              weights = anes20_dem$weight)
models <- list(
  "Republicans (OLS)" = sorting_2020_rep,
  "Republicans (LASSO)" = sorting_2020_rep_lasso,
  "Independents (OLS)" = sorting_2020_ind,
  "Independents (LASSO)" = sorting_2020_ind_lasso,
  "Democrats (OLS)" = sorting_2020_dem,
  "Democrats (LASSO)" = sorting_2020_dem_lasso
)

modelsummary(models,
  coef_rename = coef_mapper,
  stars = TRUE,
  output = "results/new_version/a2.html",
  fmt = 2,
  return_zeros = TRUE
)

sorting_2016_rep <- lm_robust(sorting_formula_16_minus_pid,
                              data = anes16_rep,
                              weights = anes16_rep$weight)
sorting_2016_rep_lasso <- lasso(sorting_formula_16_minus_pid,
                              data = anes16_rep,
                              weights = anes16_rep$weight)

sorting_2016_ind <- lm_robust(sorting_formula_16_minus_pid,
                              data = anes16_ind,
                              weights = anes16_ind$weight)
sorting_2016_ind_lasso <- lasso(sorting_formula_16_minus_pid,
                              data = anes16_ind,
                              weights = anes16_ind$weight)

sorting_2016_dem <- lm_robust(sorting_formula_16_minus_pid,
                              data = anes16_dem,
                              weights = anes16_dem$weight)
sorting_2016_dem_lasso <- lasso(sorting_formula_16_minus_pid,
                              data = anes16_dem,
                              weights = anes16_dem$weight)
models <- list(
  "Republicans (OLS)" = sorting_2016,
  "Republicans (LASSO)" = sorting_2016_rep_lasso,
  "Independents (OLS)" = sorting_2016_ind,
  "Independents (LASSO)" = sorting_2016_ind_lasso,
  "Democrats (OLS)" = sorting_2016_dem,
  "Democrats (LASSO)" = sorting_2016_dem_lasso
)

modelsummary(models,
  coef_rename = coef_mapper,
  stars = TRUE,
  output = "results/new_version/a3.html",
  fmt = 2,
  return_zeros = TRUE,
  notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)
