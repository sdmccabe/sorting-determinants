library(stargazer)
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

setwd("/home/main/git/sdmccabe/sorting-determinants/results/new_version")
load("/home/main/git/sdmccabe/sorting-determinants/data/Sorting - 2020 ANES - updated.RData")

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

m1 <- lm_robust(f1, data = anes20, weight = V200010b)
m2 <- lm_robust(f2, data = anes20, weight = V200010b)
m3 <- lm_robust(f3, data = anes20, weight = V200010b)
m4 <- lm_robust(f4, data = anes20, weight = V200010b)
m5 <- lm_robust(f5, data = anes20, weight = V200010b)
m6 <- lm_robust(f6, data = anes20, weight = V200010b)
m7 <- lm_robust(f7, data = anes20, weight = V200010b)
m8 <- lm_robust(f8, data = anes20, weight = V200010b)
m9 <- lm_robust(f9, data = anes20, weight = V200010b)

# using modelsummary instead of stargazer. three reasons:
# 1. it's a bit more flexible and has some nicer defaults
# 2. it takes estimatr::lm_robust objects directly so you don't have to pass the
#    SEs separately
# 3. we can put LASSO and OLS results side-by-side if we wanted

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
  coef_rename = c(
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
    "bornagin" = "Born Again",
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
  ),
  output = "modelsummary.html",
  fmt = 2
  )
