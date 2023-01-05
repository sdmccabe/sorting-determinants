library(estimatr)
library(tidyverse)
library(glmnet)
library(glmnetUtils)
library(modelsummary)
library(tidymodels)

# for now, focusing on reproducing Table A.4
# first cut - reproduce the original table
# the results will differ because because of a change in weighting -
# the ANES 2020s result were all accidentally unweighted in the manuscript.
load("data/stefan_sorting_data.RData")

# these models all take the form DV ~ consistent set of predictors
# so define the predictors, then use those to create the formulae
predictors <- . ~ sorting_r + age + rep + ind + male + urban + smalltown +
  rural + income + educ + ideo + union + gun + married + gay + bi +
  children + RR + AUTH + religscale + bornagain + white + know_scale + panel +
  news_fox + news_cnn + news_msnbc + facebook + twitter + instagram + reddit +
  youtube + snapchat + tiktok + sm_other


f1 <- update.formula(predictors, dem1 ~ .)
f2 <- update.formula(predictors, dem2 ~ .)
f3 <- update.formula(predictors, dem3 ~ .)
f4 <- update.formula(predictors, vio_justy ~ .)
f5 <- update.formula(predictors, fairelec ~ .)
f6 <- update.formula(predictors, comp1.1 ~ .)
f7 <- update.formula(predictors, comp2.1 ~ .)
f8 <- update.formula(predictors, MASONINDEX1 ~ .)
f9 <- update.formula(predictors, ftdifference ~ .)

anes16$educ <- anes16$education
sorting_formula <- sorting_r  ~ age + rep + ind + male + urban + smalltown +
  rural + income + educ + ideo + gay + bi + RR + AUTH + religscale +
  bornagain + white + know_scale + news_msnbc + news_cnn + news_fox
sorting_formula_16 <- sorting_r  ~ age + rep + ind + male + income +
  educ + ideo + gay + bi + RR + AUTH + religscale + bornagain +
  white + know_scale + news_msnbc + news_cnn + news_fox

sorting_2020 <- lm_robust(sorting_formula,
  data = anes20,
  weights = anes20$weight)
sorting_2016 <- lm_robust(sorting_formula_16,
  data = anes16,
  weights = anes16$weight)
sorting_2020_alt <- lm_robust(sorting_formula,
  data = anes20busby,
  weights = anes20busby$V200010b)
sorting_2016_alt <- lm_robust(sorting_formula_16,
  data = anes16busby,
  weights = anes16busby$V160102)


m1 <- lm_robust(f1, data = anes20busby, weight = anes20busby$V200010b)
m2 <- lm_robust(f2, data = anes20busby, weight = anes20busby$V200010b)
m3 <- lm_robust(f3, data = anes20busby, weight = anes20busby$V200010b)
m4 <- lm_robust(f4, data = anes20busby, weight = anes20busby$V200010b)
m5 <- lm_robust(f5, data = anes20busby, weight = anes20busby$V200010b)
m6 <- lm_robust(f6, data = anes20busby, weight = anes20busby$V200010b)
m7 <- lm_robust(f7, data = anes20busby, weight = anes20busby$V200010b)
m8 <- lm_robust(f8, data = anes20busby, weight = anes20busby$V200010b)
m9 <- lm_robust(f9, data = anes20busby, weight = anes20busby$V200010b)




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

modelsummary(list(
  "Dissatisfaction with Democracy" = m1,
  "Checks and balances aren't important" = m2,
  "Helpful if president could act alone" = m3,
  "Support for political violence" = m4,
  "Skeptical of election integrity" = m5,
  "Compromise is selling out" = m6,
  "Prefer leader sticks to principles" = m7,
  "Participation" = m8,
  "Affective polarization" = m9
  ),
  coef_rename = coef_mapper,
  output = "results/new_version/modelsummary.html",
  fmt = 2
  )

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

l1 <- lasso(f1, data = anes20busby, weights = anes20busby$V200010b)
l2 <- lasso(f2, data = anes20busby, weights = anes20busby$V200010b)
l3 <- lasso(f3, data = anes20busby, weights = anes20busby$V200010b)
l4 <- lasso(f4, data = anes20busby, weights = anes20busby$V200010b)
l5 <- lasso(f5, data = anes20busby, weights = anes20busby$V200010b)
l6 <- lasso(f6, data = anes20busby, weights = anes20busby$V200010b)
l7 <- lasso(f7, data = anes20busby, weights = anes20busby$V200010b)
l8 <- lasso(f8, data = anes20busby, weights = anes20busby$V200010b)
l9 <- lasso(f9, data = anes20busby, weights = anes20busby$V200010b)


# glance_custom.glmnet <- function(x, ...) {
#   #dv <- as.character(formula(x)[2])
#    dv <- "DV"
#   out <- data.frame("DV" = dv)
#   return(out)
# }
# #

tidy_custom.glmnet <- function(x, ...) {
  out <- data.frame(term = names(coef(x)[, 1]),
                    estimate = ifelse(coef(x)[, 1] == 0,
                                      "---",
                                      as.character(round(coef(x)[, 1], 3))))
  return(out)
}

modelsummary(list(
  "Dissatisfaction with Democracy" = l1,
  "Checks and balances aren't important" = l2,
  "Helpful if president could act alone" = l3,
  "Support for political violence" = l4,
  "Skeptical of election integrity" = l5,
  "Compromise is selling out" = l6,
  "Prefer leader sticks to principles" = l7,
  "Participation" = l8,
  "Affective polarization" = l9
  ),
  coef_map = coef_mapper,
  output = "results/new_version/modelsummary_lasso.html",
  statistic = NULL,
  estimate = "estimate",
  fmt = as.character,
  return_zeros = TRUE
  )

coef_mapper <- c(
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
    "news_msnbc" = "TV-CNN"
  )

modelsummary(list(
  "2016 Sorting (alt)" = sorting_2016,
  "2016 Sorting" = sorting_2016_alt,
  "2020 Sorting (alt)" = sorting_2020,
  "2020 Sorting" = sorting_2020_alt
),
  coef_rename = coef_mapper,
  stars = TRUE,
  output = "results/new_version/sorting_predictors.html",
  fmt = 2
)

predictors <- . ~ sorting_r + age + rep + ind + male + urban + smalltown +
  rural + income + educ + ideo +  gay + bi +
  RR + AUTH + religscale + bornagain + white + know_scale +
  news_fox + news_cnn + news_msnbc

f1 <- update.formula(predictors, dem1 ~ .)
f2 <- update.formula(predictors, dem2 ~ .)
f3 <- update.formula(predictors, dem3 ~ .)
f4 <- update.formula(predictors, vio_justy ~ .)
f5 <- update.formula(predictors, fairelec ~ .)
f6 <- update.formula(predictors, comp1.1 ~ .)
f7 <- update.formula(predictors, comp2.1 ~ .)
f8 <- update.formula(predictors, MASONINDEX1 ~ .)
f9 <- update.formula(predictors, ftdifference ~ .)
m1 <- lm_robust(f1, data = anes20busby, weight = anes20busby$V200010b)
m2 <- lm_robust(f2, data = anes20busby, weight = anes20busby$V200010b)
m3 <- lm_robust(f3, data = anes20busby, weight = anes20busby$V200010b)
m4 <- lm_robust(f4, data = anes20busby, weight = anes20busby$V200010b)
m5 <- lm_robust(f5, data = anes20busby, weight = anes20busby$V200010b)
m6 <- lm_robust(f6, data = anes20busby, weight = anes20busby$V200010b)
m7 <- lm_robust(f7, data = anes20busby, weight = anes20busby$V200010b)
m8 <- lm_robust(f8, data = anes20busby, weight = anes20busby$V200010b)
m9 <- lm_robust(f9, data = anes20busby, weight = anes20busby$V200010b)



coef_mapper <- c(coef_mapper, "sorting_r" = "Sorting")
modelsummary(list(
  "Dissatisfaction with Democracy" = m1,
  "Checks and balances aren't important" = m2,
  "Helpful if president could act alone" = m3,
  "Support for political violence" = m4,
  "Skeptical of election integrity" = m5,
  "Compromise is selling out" = m6,
  "Prefer leader sticks to principles" = m7,
  "Participation" = m8,
  "Affective polarization" = m9
  ),
  coef_rename = coef_mapper,
  output = "results/new_version/dem_DVs_2020.html",
  fmt = 2,
  stars = TRUE
  )

f1 <- update.formula(f1, ~ . - urban - rural - smalltown)
f2 <- update.formula(f2, ~ . - urban - rural - smalltown)
f3 <- update.formula(f3, ~ . - urban - rural - smalltown)
f4 <- update.formula(f4, ~ . - urban - rural - smalltown)
f5 <- update.formula(f5, ~ . - urban - rural - smalltown)
f6 <- update.formula(f6, ~ . - urban - rural - smalltown)
f7 <- update.formula(f7, ~ . - urban - rural - smalltown)
f8 <- update.formula(f8, ~ . - urban - rural - smalltown)
f9 <- update.formula(f9, ~ . - urban - rural - smalltown)

m1 <- lm_robust(f1, data = anes16busby, weight = anes16busby$V160102)
#m2 <- lm_robust(f2, data = anes16busby, weight = anes16busby$V160102)
#m3 <- lm_robust(f3, data = anes16busby, weight = anes16busby$V160102)
m4 <- lm_robust(f4, data = anes16busby, weight = anes16busby$V160102)
m5 <- lm_robust(f5, data = anes16busby, weight = anes16busby$V160102)
m6 <- lm_robust(f6, data = anes16busby, weight = anes16busby$V160102)
m7 <- lm_robust(f7, data = anes16busby, weight = anes16busby$V160102)
m8 <- lm_robust(f8, data = anes16busby, weight = anes16busby$V160102)
m9 <- lm_robust(f9, data = anes16busby, weight = anes16busby$V160102)

modelsummary(list(
  "Dissatisfaction with Democracy" = m1,
  #"Checks and balances aren't important" = m2,
  #"Helpful if president could act alone" = m3,
  "Support for political violence" = m4,
  "Skeptical of election integrity" = m5,
  "Compromise is selling out" = m6,
  "Prefer leader sticks to principles" = m7,
  "Participation" = m8,
  "Affective polarization" = m9
  ),
  coef_rename = coef_mapper,
  output = "results/new_version/dem_DVs_2016.html",
  fmt = 2,
  stars = TRUE
  )
