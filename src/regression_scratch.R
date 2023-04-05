
# Imports -----------------------------------------------------------------

library(estimatr)
library(tidyverse)
library(glmnet)
library(glmnetUtils)
library(modelsummary)
library(tidymodels)

set.seed(1679068582)

# override modelsummary's default goodness-of-fit statistics to
# show LASSO pseudo-R2
GOF_MAP <- rbind(modelsummary::gof_map,
  data.frame(raw = c('dev.ratio'),
             clean = c("McFadden pseudo-R2"),
             fmt = c(3),
             omit = c(FALSE)))

# all variables displayed in any tables should be named here
COEFRENAMER <- c(
  "(Intercept)" = "Intercept",
  "sorting_r" = "Sorting",
  "age" = "Age",
  "rep" = "Republican",
  "ind" = "Independent",
  "female" = "Female",
  "male" = "Male",
  "urban" = "Urban",
  "smalltown" = "Small town",
  "rural" = "Rural",
  "income" = "Income",
  "educ" = "Education",
  "ideo" = "Ideology",
  "married" = "Married",
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
  "instagram" = "Instagram",
  "snapchat" = "Snapchat",
  "reddit" = "Reddit",
  "tiktok" = "TikTok",
  "sm_other" = "Other SM",
  "youtube" = "YouTube",
  "union" = "Union Membership",
  "gun" = "Gun Ownership",
  "children" = "Has Children",
  "sorting_r_change" = "Δ Sorting",
  "pid7" = "Party ID",
  "policyindex" = "Issue scale",
  "south" = "South",
  "attend_church_ever" = "Church Attendance",
  "hisp" = "Hispanic",
  "black" = "Black",
  "issue_strength" = "Issue Strength",
  "issue_constraint" = "Issue Constraint",
  "issue_strength:issue_constraint" = "Strength x Constraint",
  "pid7_str" = "Partisan ID Strength",
  "dem1" = "Dissatisfaction with democracy",
  "dem2" = "Checks and balances aren't important",
  "dem3" = "Helpful if president could act alone",
  "vio_justy" = "Support for political violence",
  "fairelec" = "Skeptical of election integrity",
  "comp1.1" = "Compromise is selling out",
  "comp2.1" = "Prefer leader sticks to principles",
  "MASONINDEX1" = "Participation",
  "ftdifference" = "Affective polarization",
  "tucker" = "Tucker Carlson",
  "hannity" = "Hannity",
  "the_five" = "The Five",
  "ingraham" = "The Ingraham Angle",
  "maccallum" = "The Story with Martha MacCallum",
  "baier" = "Special Report with Bret Baier",
  "fox_and_friends" = "Fox and Friends",
  "maddow" = "The Rachel Maddow Show",
  "lawrence" = "The Last Word with Lawrence O'Donnell",
  "morning_joe" = "Morning Joe",
  "chris_hayes" = "All In with Chris Hayes",
  "brian_williams" = "NBC Nightly News",
  "tapper" = "The Lead with Jake Tapper",
  "anderson" = "Anderson Cooper 360",
  "cuomo" = "Cuomo Prime Time",
  "erin_burnett" = "Erin Burnett OutFront",
  "kelly" = "The Kelly File",
  "van_susteren" = "On the Record with Greta van Susteren",
  "oreilly" = "The O'Reilly Factor",
  "chris_matthews" = "Hardball with Chris Matthews",
  "nancy_grace" = "Nancy Grace",
  "cnn_en_espanol" = "CNN en Espanol"
)



# Functions ---------------------------------------------------------------

# This is a wrapper function for doing LASSO with defaults for cross-validation
lasso_new <- function(f, data, weights) {
  mask <<- subset(data, select = rownames(attr(terms(f), "factors"))) |>
    complete.cases()
  masked_data <<- data[mask, ]
  # NOTE: this is assigning globally, but I wasn't able to get it working
  # with normal scoping for reasons I couldn't really process.
  masked_w <<- weights[mask]

  assertthat::are_equal(dim(masked_data)[1], length(masked_w))

  mat <<- model.frame(formula = f, data = masked_data, weights = masked_w)
  y <<- mat[, 1]
  w <<- mat[, dim(mat)[2]]
  x <<- model.matrix(object = f, data = masked_data, weights = masked_w)
  x <<- x[, 2:ncol(x)]
  #x <- mat[, 2:(dim(mat)[2] - 1)] |> as.matrix()
  #w <- mat[, dim(mat)[2]]

  lasso_new_cv <<- cv.glmnet(y = y, x = x, weights = w)

  lambda <<- lasso_new_cv$lambda.min
  out <<- glmnet(y = y, x = x, lambda = lambda, alpha = 1)

  return(out)
}


# Custom tidier function for modelsummary -
# when presenting LASSO results, use "---" to indicate which variables have
# had their coefficients set to zero automatically
tidy_custom.glmnet <- function(x, ...) {
  out <- data.frame(term = names(coef(x)[, 1]),
                    estimate = ifelse(coef(x)[, 1] == 0,
                                      "---",
                                      as.character(round(coef(x)[, 1], 3))),
                    p.value = NA_real_,
                    std.error = NA_real_)
  return(out)
}

# Show only N and dev.ratio (renamed to pseudo-R2 above) in LASSO
glance_custom.glmnet <- function(x, ...) {
  out <- data.frame(nobs = stats::nobs(x),
                    dev.ratio = x$dev.ratio)
  return(out)
}

# This function wraps all the model fitting and writes the output to a file.
# After this we just need the mapping of model specification to table names.

# using modelsummary instead of stargazer. three reasons:
# 1. it's a bit more flexible and has some nicer defaults
# 2. it takes estimatr::lm_robust objects directly so you don't have to pass the
#    SEs separately
# 3. we can put LASSO and OLS results side-by-side if we wanted

# spec should be a named list of lists. The list subelements should
# contain `data`, `formula`, and `fun`. `data` must contain sample
# weights in the `weight` column.
# NOTE: This is duplicating `data` a lot. probably a fix for that,
# but I don't care right now.
models_to_table <- function(
  spec,
  output,
  fmt = 2,
  return_zeros = TRUE,
  stars = TRUE,
  notes = NULL,
  title = NULL
  ) {

  res <- lapply(spec,
    \(x) x$fun(x$formula, data = x$data, weights = x$data$weight))

  # If the output is a latex table, we want to use some styling
  # from kableExtra
  # NOTE: the kableExtra styling is using an outdated package, `tabu`.
  # to make it play nice, you need to load an outdated version of `longtable`:
  # \usepackage{longtable}[=v4.13]
  out_format <- stringr::str_split(output, "\\.", simplify = T)[-1]
  out_type <- case_when(out_format == "tex" ~ "latex",
                        out_format == "html" ~ "html",
                        TRUE ~ NA_character_)

  k <- modelsummary(
    res,
    output = out_type,
    coef_rename = COEFRENAMER,
    stars = stars,
    fmt = fmt,
    return_zeros = return_zeros,
    notes = notes,
    title = title,
    longtable = TRUE,
    gof_map = GOF_MAP
    )
  if(out_type == "latex") {
    k <- k |>
      kableExtra::kable_styling(full_width = TRUE)
  }
  kableExtra::save_kable(k, output)
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

#  Defining Controls -----------------------------------------------------------------

# these models all take the form DV ~ consistent set of predictors
# so define the predictors, then use those to create the formulae
sorting_plus_controls <- . ~ sorting_r + age + rep + ind + male + urban + smalltown +
  rural + income + educ + ideo + union + gun + married + gay + bi +
  children + RR + AUTH + religscale + bornagain + white + know_scale +
  news_fox + news_cnn + news_msnbc + facebook + twitter + instagram + reddit +
  youtube + snapchat + tiktok + sm_other + issue_strength +issue_constraint +
  issue_strength:issue_constraint + pid7_str

sorting_plus_controls_16 <- update.formula(sorting_plus_controls, ~ . -
  urban - smalltown - rural - facebook - twitter - instagram - reddit -
  snapchat - tiktok - sm_other - youtube + facebook_tw)

# other models are going to use these controls minus sorting
default_controls <- . ~ female + age + rep + ind + urban + smalltown +
  rural + income + educ + ideo + union + gun + gay + bi +  children + RR +
  AUTH + religscale + bornagain + white + facebook + twitter + instagram +
  reddit + youtube + snapchat + tiktok + sm_other + news_msnbc + news_cnn +
  news_fox + know_scale + issue_strength + issue_constraint +
  issue_strength:issue_constraint + pid7_str

default_controls_16 <- update.formula(default_controls, ~ . -
                                        urban - smalltown - rural - facebook - twitter - instagram - reddit -
                                        snapchat - tiktok - sm_other - youtube + facebook_tw)

# others are going to study PID subsets, and therefore need to drop PID
sorting_plus_controls_minus_pid <- update.formula(sorting_plus_controls, ~ . - rep - ind - pid7_str)
sorting_plus_controls_16_minus_pid <- update.formula(sorting_plus_controls_16, ~ . - rep - ind - pid7_str)
default_controls_minus_pid <- update.formula(default_controls, ~ . - rep - ind - pid7_str)
default_controls_16_minus_pid <- update.formula(default_controls, ~ . - rep - ind - pid7_str)

sorting_formula <- update.formula(default_controls, sorting_r ~ .)
sorting_formula_16 <- update.formula(default_controls_16, sorting_r ~ .)

sorting_formula_minus_pid <- update.formula(sorting_formula, ~ . - rep - ind - pid7_str - ideo)
sorting_formula_16_minus_pid <- update.formula(sorting_formula_16, ~ . - rep - ind - pid7_str - ideo)

sorting_formula_disaggregate_media <- update.formula(sorting_formula,
  ~ . - news_fox - news_msnbc - news_cnn + hannity + tucker + the_five +
    ingraham + maccallum + baier + fox_and_friends + maddow + lawrence +
    morning_joe + chris_hayes + brian_williams + tapper + anderson +
    cuomo + erin_burnett)

sorting_formula_disaggregate_media_16 <- update.formula(sorting_formula_16,
  ~ . - news_fox - news_msnbc - news_cnn + hannity + kelly + van_susteren +
      oreilly + chris_matthews + maddow + anderson + nancy_grace +
    erin_burnett + cnn_en_espanol)

sorting_formula_disaggregate_media_minus_pid <-
  update.formula(sorting_formula_disaggregate_media, ~ . - rep - ind - pid7_str - ideo)

sorting_formula_disaggregate_media_16_minus_pid <-
  update.formula(sorting_formula_disaggregate_media_16, ~ . - rep - ind - pid7_str - ideo)

# Table A.4 -----------------------------------------------------------------

# NOTE: the results will differ because because of a change in weighting -
# the ANES 2020s result were all accidentally unweighted in the manuscript.

spec <- list(
  "Dissatisfaction with democracy" = list(
    formula = update.formula(sorting_plus_controls, dem1 ~ .),
    data = anes20,
    fun = lm_robust),
  "Checks and balances aren't important" = list(
    formula = update.formula(sorting_plus_controls, dem2 ~ .),
    data = anes20,
    fun = lm_robust),
  "Helpful if president could act alone" = list(
    formula = update.formula(sorting_plus_controls, dem3 ~ .),
    data = anes20,
    fun = lm_robust),
  "Support for political violence" = list(
    formula = update.formula(sorting_plus_controls, vio_justy ~ .),
    data = anes20,
    fun = lm_robust),
  "Skeptical of election integrity" = list(
    formula = update.formula(sorting_plus_controls, fairelec ~ .),
    data = anes20,
    fun = lm_robust),
  "Compromise is selling out" = list(
    formula = update.formula(sorting_plus_controls, comp1.1 ~ .),
    data = anes20,
    fun = lm_robust),
  "Prefer leader sticks to principles" = list(
    formula = update.formula(sorting_plus_controls, comp2.1 ~ .),
    data = anes20,
    fun = lm_robust),
  "Participation" = list(
    formula = update.formula(sorting_plus_controls, MASONINDEX1 ~ .),
    data = anes20,
    fun = lm_robust),
  "Affective polarization" = list(
    formula = update.formula(sorting_plus_controls, ftdifference ~ .),
    data = anes20,
    fun = lm_robust)
)

models_to_table(spec,
                output="results/tables/tex/a4.tex",
                title="Effects of sorting, models behind Figure 4 (2020) (OLS)")

# replace OLS with lasso_new for alternate table
spec <- lapply(spec, \(x) (list(data = x$data, formula = x$formula, fun = lasso_new)))
models_to_table(spec,
                output="results/tables/tex/a4_lasso.tex",
                title="Effects of sorting, models behind Figure 4 (2020) (LASSO)")

spec <- list(
  "Dissatisfaction with Democracy" = list(
    formula = update.formula(sorting_plus_controls_16, dem1 ~ .),
    data = anes16,
    fun = lm_robust),
  "Support for political violence" = list(
    formula = update.formula(sorting_plus_controls_16, vio_justy ~ .),
    data = anes16,
    fun = lm_robust),
  "Skeptical of election integrity" = list(
    formula = update.formula(sorting_plus_controls_16, fairelec ~ .),
    data = anes16,
    fun = lm_robust),
  "Compromise is selling out" = list(
    formula = update.formula(sorting_plus_controls_16, comp1.1 ~ .),
    data = anes16,
    fun = lm_robust),
  "Prefer leader sticks to principles" = list(
    formula = update.formula(sorting_plus_controls_16, comp2.1 ~ .),
    data = anes16,
    fun = lm_robust),
  "Participation" = list(
    formula = update.formula(sorting_plus_controls_16, MASONINDEX1 ~ .),
    data = anes16,
    fun = lm_robust),
  "Affective polarization" = list(
    formula = update.formula(sorting_plus_controls_16, ftdifference ~ .),
    data = anes16,
    fun = lm_robust)
)

models_to_table(spec,
                output="results/tables/tex/a4_16.tex",
                title="Effects of sorting, models behind Figure 4 (2016) (OLS)")

# replace OLS with lasso_new for alternate table
spec <- lapply(spec, \(x) (list(data = x$data, formula = x$formula, fun = lasso_new)))
models_to_table(spec,
                output="results/tables/tex/a4_lasso_16.tex",
                title="Effects of sorting, models behind Figure 4 (2016) (LASSO)")

# Table A.1 ----------------------------------------------------------------

spec <- list(
  "2016 Sorting (OLS)" = list(formula = sorting_formula_16,
                              data = anes16,
                              fun = lm_robust),
  "2016 Sorting (LASSO)" = list(formula = sorting_formula_16,
                              data = anes16,
                              fun = lasso_new),
  "2020 Sorting (OLS)" = list(formula = sorting_formula,
                              data = anes20,
                              fun = lm_robust),
  "2020 Sorting (LASSO)" = list(formula = sorting_formula,
                              data = anes20,
                              fun = lasso_new)
)
models_to_table(spec, output="results/tables/tex/a1.tex",
                title = "Detailed results predicting sorting, whole sample")

# Tables A.2 and A.3 ---------------------------------------------------------------

spec <- list(
  "Republicans (OLS)" = list(formula = sorting_formula_minus_pid, data=anes20_rep, fun = lm_robust),
  "Republicans (LASSO)" = list(formula = sorting_formula_minus_pid, data=anes20_rep, fun = lasso_new),
  "Independents (OLS)" = list(formula = sorting_formula_minus_pid, data=anes20_ind, fun = lm_robust),
  "Independents (LASSO)" = list(formula = sorting_formula_minus_pid, data=anes20_ind, fun = lasso_new),
  "Democrats (OLS)" = list(formula = sorting_formula_minus_pid, data=anes20_dem, fun = lm_robust),
  "Democrats (LASSO)" = list(formula = sorting_formula_minus_pid, data=anes20_dem, fun = lasso_new)
)
models_to_table(spec,
                output="results/tables/tex/a2.tex",
                title="Detailed results predicting sorting, partisan categories, 2020")

spec <- list(
  "Republicans (OLS)" = list(formula = sorting_formula_16_minus_pid, data=anes16_rep, fun = lm_robust),
  "Republicans (LASSO)" = list(formula = sorting_formula_16_minus_pid, data=anes16_rep, fun = lasso_new),
  "Independents (OLS)" = list(formula = sorting_formula_16_minus_pid, data=anes16_ind, fun = lm_robust),
  "Independents (LASSO)" = list(formula = sorting_formula_16_minus_pid, data=anes16_ind, fun = lasso_new),
  "Democrats (OLS)" = list(formula = sorting_formula_16_minus_pid, data=anes16_dem, fun = lm_robust),
  "Democrats (LASSO)" = list(formula = sorting_formula_16_minus_pid, data=anes16_dem, fun = lasso_new)
)

models_to_table(spec,
  output = "results/tables/tex/a3.tex",
  title = "Detailed results predicting sorting, partisan categories, 2016",
  notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)

# Table A.5 ---------------------------------------------------------------

satisfaction_formula <- update.formula(sorting_formula_minus_pid, demsat ~ .)
satisfaction_formula_16 <- update.formula(sorting_plus_controls_16_minus_pid, demsat ~ .)

spec <- list(
  "Republicans (OLS)" = list(formula = satisfaction_formula,
                             data = anes20_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = satisfaction_formula,
                             data = anes20_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = satisfaction_formula,
                             data = anes20_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = satisfaction_formula,
                             data = anes20_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = satisfaction_formula,
                             data = anes20_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = satisfaction_formula,
                             data = anes20_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  output = "results/tables/tex/a5a.tex",
  title = "Effects of sorting on democratic dissatisfaction, partisan categories, 2020"
)

spec <- list(
  "Republicans (OLS)" = list(formula = satisfaction_formula_16,
                             data = anes16_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = satisfaction_formula_16,
                             data = anes16_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = satisfaction_formula_16,
                             data = anes16_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = satisfaction_formula_16,
                             data = anes16_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = satisfaction_formula_16,
                             data = anes16_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = satisfaction_formula_16,
                             data = anes16_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  output = "results/tables/tex/a5b.tex",
  title = "Effects of sorting on democratic dissatisfaction, partisan categories, 2016",
  notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)

# Table A.6 ---------------------------------------------------------------

# dem2 is 2020 only
# NOTE: double-check this. there are two "checks and balances" variables
# mentioned in the manuscript
checks_formula <- update.formula(sorting_plus_controls_minus_pid, dem2 ~ .)

spec <- list(
  "Republicans (OLS)" = list(formula = checks_formula,
                             data = anes20_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = checks_formula,
                             data = anes20_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = checks_formula,
                             data = anes20_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = checks_formula,
                             data = anes20_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = checks_formula,
                             data = anes20_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = checks_formula,
                             data = anes20_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  title = "Effects of sorting on support for checks and balances, partisan categories, 2020",
  output = "results/tables/tex/a6.tex"
  )

# Table A.7 ---------------------------------------------------------------
actalone_formula <- update.formula(sorting_plus_controls_minus_pid, dem3 ~ .)

spec <- list(
  "Republicans (OLS)" = list(formula = actalone_formula,
                             data = anes20_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = actalone_formula,
                             data = anes20_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = actalone_formula,
                             data = anes20_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = actalone_formula,
                             data = anes20_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = actalone_formula,
                             data = anes20_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = actalone_formula,
                             data = anes20_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  title = "Effects of sorting on president acting alone, partisan categories, 2020",
  output = "results/tables/tex/a7.tex"
)

# Table A.8 ---------------------------------------------------------------
violence_formula <- update.formula(sorting_plus_controls_minus_pid, vio_justy ~ .)
violence_formula_16 <- update.formula(sorting_plus_controls_16_minus_pid, vio_justy ~ .)

spec <- list(
  "Republicans (OLS)" = list(formula = violence_formula,
                             data = anes20_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = violence_formula,
                             data = anes20_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = violence_formula,
                             data = anes20_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = violence_formula,
                             data = anes20_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = violence_formula,
                             data = anes20_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = violence_formula,
                             data = anes20_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  title = "Effects of sorting on support for political violence, partisan categories, 2020",
  output = "results/tables/tex/a8a.tex"
)

spec <- list(
  "Republicans (OLS)" = list(formula = violence_formula_16,
                             data = anes16_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = violence_formula_16,
                             data = anes16_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = violence_formula_16,
                             data = anes16_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = violence_formula_16,
                             data = anes16_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = violence_formula_16,
                             data = anes16_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = violence_formula_16,
                             data = anes16_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  output = "results/tables/tex/a8b.tex",
  title = "Effects of sorting on support for political violence, partisan categories, 2016",
  notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)

# Table A.9 ---------------------------------------------------------------
election_formula <- update.formula(sorting_plus_controls_minus_pid, fairelec ~ .)
election_formula_16 <- update.formula(sorting_plus_controls_16_minus_pid, fairelec ~ .)

spec <- list(
  "Republicans (OLS)" = list(formula = election_formula,
                             data = anes20_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = election_formula,
                             data = anes20_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = election_formula,
                             data = anes20_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = election_formula,
                             data = anes20_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = election_formula,
                             data = anes20_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = election_formula,
                             data = anes20_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  title = "Effects of sorting on election integrity skepticism, partisan categories, 2020",
  output = "results/tables/tex/a9a.tex"
)

spec <- list(
  "Republicans (OLS)" = list(formula = election_formula_16,
                             data = anes16_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = election_formula_16,
                             data = anes16_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = election_formula_16,
                             data = anes16_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = election_formula_16,
                             data = anes16_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = election_formula_16,
                             data = anes16_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = election_formula_16,
                             data = anes16_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  output = "results/tables/tex/a9b.tex",
  title = "Effects of sorting on election integrity skepticism, partisan categories, 2016",
  notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)

# Table A.10 ---------------------------------------------------------------
compromise_formula <- update.formula(sorting_plus_controls_minus_pid, comp1.1 ~ .)
compromise_formula_16 <- update.formula(sorting_plus_controls_16_minus_pid, comp1.1 ~ .)

spec <- list(
  "Republicans (OLS)" = list(formula = compromise_formula,
                             data = anes20_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = compromise_formula,
                             data = anes20_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = compromise_formula,
                             data = anes20_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = compromise_formula,
                             data = anes20_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = compromise_formula,
                             data = anes20_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = compromise_formula,
                             data = anes20_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  title = "Effects of sorting on belief compromise is selling out, partisan categories, 2020",
  output = "results/tables/tex/a10a.tex"
)

spec <- list(
  "Republicans (OLS)" = list(formula = compromise_formula_16,
                             data = anes16_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = compromise_formula_16,
                             data = anes16_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = compromise_formula_16,
                             data = anes16_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = compromise_formula_16,
                             data = anes16_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = compromise_formula_16,
                             data = anes16_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = compromise_formula_16,
                             data = anes16_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  output = "results/tables/tex/a10b.tex",
  title = "Effects of sorting on belief compromise is selling out, partisan categories, 2016",
  notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)

# Table A.11 ---------------------------------------------------------------
principles_formula <- update.formula(sorting_plus_controls_minus_pid, comp2.1 ~ .)
principles_formula_16 <- update.formula(sorting_plus_controls_16_minus_pid, comp2.1 ~ .)

spec <- list(
  "Republicans (OLS)" = list(formula = principles_formula,
                             data = anes20_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = principles_formula,
                             data = anes20_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = principles_formula,
                             data = anes20_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = principles_formula,
                             data = anes20_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = principles_formula,
                             data = anes20_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = principles_formula,
                             data = anes20_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  title = "Effects of sorting on preferring leader who sticks to principles, partisan categories, 2020",
  output = "results/tables/tex/a11a.tex"
)

spec <- list(
  "Republicans (OLS)" = list(formula = principles_formula_16,
                             data = anes16_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = principles_formula_16,
                             data = anes16_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = principles_formula_16,
                             data = anes16_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = principles_formula_16,
                             data = anes16_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = principles_formula_16,
                             data = anes16_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = principles_formula_16,
                             data = anes16_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  output = "results/tables/tex/a11b.tex",
  title = "Effects of sorting on preferring leader who sticks to principles, partisan categories, 2016",
  notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)

# Table A.12 ---------------------------------------------------------------
participation_formula <- update.formula(sorting_plus_controls_minus_pid, MASONINDEX1 ~ .)
participation_formula_16 <- update.formula(sorting_plus_controls_16_minus_pid, MASONINDEX1 ~ .)

spec <- list(
  "Republicans (OLS)" = list(formula = participation_formula,
                             data = anes20_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = participation_formula,
                             data = anes20_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = participation_formula,
                             data = anes20_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = participation_formula,
                             data = anes20_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = participation_formula,
                             data = anes20_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = participation_formula,
                             data = anes20_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  output = "results/tables/tex/a12a.tex",
  title = "Effects of sorting on political participation, partisan categories, 2020",
)

spec <- list(
  "Republicans (OLS)" = list(formula = participation_formula_16,
                             data = anes16_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = participation_formula_16,
                             data = anes16_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = participation_formula_16,
                             data = anes16_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = participation_formula_16,
                             data = anes16_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = participation_formula_16,
                             data = anes16_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = participation_formula_16,
                             data = anes16_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  output = "results/tables/tex/a12b.tex",
  title = "Effects of sorting on political participation, partisan categories, 2016",
  notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)

# Table A.13 ---------------------------------------------------------------
affective_formula <- update.formula(sorting_plus_controls_minus_pid, ftdifference ~ .)
affective_formula_16 <- update.formula(sorting_plus_controls_16_minus_pid, ftdifference ~ .)

spec <- list(
  "Republicans (OLS)" = list(formula = affective_formula,
                             data = anes20_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = affective_formula,
                             data = anes20_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = affective_formula,
                             data = anes20_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = affective_formula,
                             data = anes20_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = affective_formula,
                             data = anes20_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = affective_formula,
                             data = anes20_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  output = "results/tables/tex/a13a.tex",
  title = "Effects of sorting on affective polarization, partisan categories, 2020",
)

spec <- list(
  "Republicans (OLS)" = list(formula = affective_formula_16,
                             data = anes16_rep,
                             fun = lm_robust),
  "Republicans (LASSO)" = list(formula = affective_formula_16,
                             data = anes16_rep,
                             fun = lasso_new),
  "Independents (OLS)" = list(formula = affective_formula_16,
                             data = anes16_ind,
                             fun = lm_robust),
  "Independents (LASSO)" = list(formula = affective_formula_16,
                             data = anes16_ind,
                             fun = lasso_new),
  "Democrats (OLS)" = list(formula = affective_formula_16,
                             data = anes16_dem,
                             fun = lm_robust),
  "Democrats (LASSO)" = list(formula = affective_formula_16,
                             data = anes16_dem,
                             fun = lasso_new)
)

models_to_table(spec,
  output = "results/tables/tex/a13b.tex",
  title = "Effects of sorting on affective polarization, partisan categories, 2016",
  notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)


# Table A.14 ---------------------------------------------------------------
# NOTE: I can't find the original model specification; currently using the
# single-variable model VAR_2020 ~ sorting_r_change, and using panel sample weights.

panel <- panel |> filter(panel == 1) |> rename(weight = panel_weight)
change_formula <- . ~ sorting_r_change
spec <- list(
  "Dissatisfaction with Democracy" = list(
    formula = update.formula(change_formula, demsat_2020 ~ .),
    data = panel,
    fun = lm_robust),
  "Support for political violence" = list(
    formula = update.formula(change_formula, vio_justy_2020 ~ .),
    data = panel,
    fun = lm_robust),
  "Skeptical of election integrity" = list(
    formula = update.formula(change_formula, fairelec_2020 ~ .),
    data = panel,
    fun = lm_robust),
  "Compromise is selling out" = list(
    formula = update.formula(change_formula, comp1.1_2020 ~ .),
    data = panel,
    fun = lm_robust),
  "Prefer leader sticks to principles" = list(
    formula = update.formula(change_formula, comp2.1_2020 ~ .),
    data = panel,
    fun = lm_robust),
  "Participation" = list(
    formula = update.formula(change_formula, MASONINDEX1_2020 ~ .),
    data = panel,
    fun = lm_robust),
  "Affective polarization" = list(
    formula = update.formula(change_formula, ftdifference_2020 ~ .),
    data = panel,
    fun = lm_robust)
)

models_to_table(spec,
  output = "results/tables/tex/a14.tex",
  title = "Effects of changes in sorting on dependent variables"
)


# Mason A.1 ---------------------------------------------------------------

# NOTE: this differs from Mason in two major ways:
# 1. missing value coding decisions. Her Stata code is often recoding NAs to 0.
# 2. `urban` is excluded from the model. 2012 and 2016 have dwell_block_urban
#    as an administrative variable in FTF respondents only; 2020 has
#    urban/rural ID. (and there aren't a lot of FTFs in 2016). So the variable
#    causes a lot of missingness and is not comparable across years.
#    (and wasn't comparable across years in Mason - the 1952-2000 data uses
#     something closer to the 2020 item, which is why the variable drops off
#     in 2004 and 2008).

mason_formula_a1a <- FT_dem ~ pid7 + policyindex + male + south + white +
                              attend_church_ever + age
mason_formula_a1b <- update.formula(mason_formula_a1a, FT_rep ~ .)

spec = list(
  "2012 (OLS)" = list(
    formula = mason_formula_a1a,
    data = anes12 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lm_robust),
  "2012 (LASSO)" = list(
    formula = mason_formula_a1a,
    data = anes12 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lasso_new),
  "2016 (OLS)" = list(
    formula = mason_formula_a1a,
    data = anes16 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lm_robust),
  "2016 (LASSO)" = list(
    formula = mason_formula_a1a,
    data = anes16 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lasso_new),
  "2020 (OLS)" = list(
    formula = mason_formula_a1a,
    data = anes20 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lm_robust),
  "2020 (LASSO)" = list(
    formula = mason_formula_a1a,
    data = anes20 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lasso_new)
)

models_to_table(spec,
                output = "results/tables/tex/mason_a1a.tex",
                title = "Replication of Mason (2018) Table A.1a")

spec = list(
  "2012 (OLS)" = list(
    formula = mason_formula_a1b,
    data = anes12 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lm_robust),
  "2012 (LASSO)" = list(
    formula = mason_formula_a1b,
    data = anes12 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lasso_new),
  "2016 (OLS)" = list(
    formula = mason_formula_a1b,
    data = anes16 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lm_robust),
  "2016 (LASSO)" = list(
    formula = mason_formula_a1b,
    data = anes16 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lasso_new),
  "2020 (OLS)" = list(
    formula = mason_formula_a1b,
    data = anes20 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lm_robust),
  "2020 (LASSO)" = list(
    formula = mason_formula_a1b,
    data = anes20 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lasso_new)
)

models_to_table(spec,
                output = "results/tables/tex/mason_a1b.tex",
                title = "Replication of Mason (2018) Table A.1b")


# Mason A.4 ---------------------------------------------------------------

mason_formula_a4 <- ftdifference ~ pid7_str + sorting_r + issue_strength*issue_constraint +
  know_scale + educ + white + hisp + black + male + age + income + attend_church_ever

spec = list(
  "2012 (OLS)" = list(
    formula = mason_formula_a4,
    data = anes12 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lm_robust),
  "2012 (LASSO)" = list(
    formula = mason_formula_a4,
    data = anes12 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lasso_new),
  "2016 (OLS)" = list(
    formula = mason_formula_a4,
    data = anes16 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lm_robust),
  "2016 (LASSO)" = list(
    formula = mason_formula_a4,
    data = anes16 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lasso_new),
  "2020 (OLS)" = list(
    formula = mason_formula_a4,
    data = anes20 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lm_robust),
  "2020 (LASSO)" = list(
    formula = mason_formula_a4,
    data = anes20 |> mutate_if(is.numeric, \(x) x / max(x, na.rm=T)),
    fun = lasso_new)
)

models_to_table(spec,
                output = "results/tables/tex/mason_a4.tex",
                title = "Replication of Mason (2018) Table A.4")


spec <- list(
  "2016 (I)" = list(
    formula = update.formula(sorting_plus_controls_16, ftdifference ~ .),
    data = anes16,
    fun = lm_robust
  ),
  "2016 (II)" = list(
    formula = mason_formula_a4,
    data = anes16,
    fun = lm_robust
  ),
  "2020 (I)" = list(
    formula = update.formula(sorting_plus_controls, ftdifference ~ .),
    data = anes20,
    fun = lm_robust
  ),
  "2020 (II)" = list(
    formula = mason_formula_a4,
    data = anes20,
    fun = lm_robust
  )
)

models_to_table(spec,
                output = "results/tables/tex/mason_replication.tex",
                title = "Comparison to Mason (2018) affective polarization model (OLS)",
                notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.",
                             "Coefficients may differ from originating tables due to differences in pre-processing.")
                )

# replace OLS with lasso_new for alternate table
spec <- lapply(spec, \(x) (list(data = x$data, formula = x$formula, fun = lasso_new)))
models_to_table(spec,
                output="results/tables/tex/mason_replication_lasso.tex",
                title="Comparison to Mason (2018) affective polarization model (LASSO)")



# Plots -------------------------------------------------------------------

m1 <- lm_robust(sorting_formula_16, data=anes16, weights = anes16$weight) |> tidy() |> mutate(model="All")
m2 <- lm_robust(sorting_formula_16_minus_pid, data=anes16_dem, weights = anes16_dem$weight) |> tidy() |> mutate(model="Democrats")
m3 <- lm_robust(sorting_formula_16_minus_pid, data=anes16_ind, weights = anes16_ind$weight) |> tidy() |> mutate(model="Independents")
m4 <- lm_robust(sorting_formula_16_minus_pid, data=anes16_rep, weights = anes16_rep$weight) |> tidy() |> mutate(model="Republicans")


t1 <- bind_rows(m1, m2, m3, m4) |> tibble () |>
  mutate(term = as.factor(term) |> fct_relabel(~COEFRENAMER[.x]) |> fct_relevel(COEFRENAMER) |> fct_rev(),
  star = as.integer(p.value < 0.05)) |>
  mutate(hypothesis = case_when(term == "Female" ~ "Gender",
                                term == "White" ~ "Race",
                                term %in% c("TV-CNN", "TV-MSNBC", "TV-Fox") ~ "Media",
                                term %in% c("Republican", "Independent") ~ "Party",
                                TRUE ~ NA_character_))


caption <- "Plot shows estimated relationship with sorting, where 0 indicates perfectly unsorted and 1 indicates perfectly sorted.\nEstimates created with OLS regression, using robust standard errors, and are weighted with ANES weights.\nShaded coefficients are significant at 95% confidence; unshaded are not."
p1 <- ggplot(filter(t1, term!="Intercept"), aes(x=term, alpha = star, color=hypothesis)) +
  geom_point(aes(y = estimate), size=2) +
  geom_linerange(aes(ymin=conf.low, ymax=conf.high)) +
  coord_flip(ylim = c(-0.25, 0.25)) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  labs(x="Term",
       y="Coefficient",
       title="Predictors of party-ideology sorting, 2016 ANES",
       caption = caption,
       color = "Hypotheses") +
  guides(alpha="none") +
  scale_alpha_continuous(range = c(0.25, 1)) +
  scale_color_brewer(type = "qual", palette = 6, na.value="gray50") +
  facet_wrap(vars(model), nrow = 1) +
  theme(text = element_text(size=16),
        plot.caption = element_text(size=12))

ggsave("results/figures/sorting_predictors_2016.png", plot=p1, dpi=400, width=16, height=9, units="in")
ggsave("results/figures/sorting_predictors_2016.pdf", plot=p1, dpi=400, width=16, height=9, units="in")

m1 <- lm_robust(sorting_formula, data=anes20, weights = anes20$weight)  |> tidy() |> mutate(model="All")
m2 <- lm_robust(sorting_formula_minus_pid, data=anes20_dem, weights = anes20_dem$weight) |> tidy() |> mutate(model="Democrats")
m3 <- lm_robust(sorting_formula_minus_pid, data=anes20_ind, weights = anes20_ind$weight) |> tidy() |> mutate(model="Independents")
m4 <- lm_robust(sorting_formula_minus_pid, data=anes20_rep, weights = anes20_rep$weight) |> tidy() |> mutate(model="Republicans")


t2 <- bind_rows(m1, m2, m3, m4) |> tibble()|>
  mutate(term = as.factor(term) |> fct_relabel(~COEFRENAMER[.x]) |> fct_relevel(COEFRENAMER) |> fct_rev(),
  star = as.integer(p.value < 0.05)) |>
  mutate(hypothesis = case_when(term == "Female" ~ "Gender",
                                term == "White" ~ "Race",
                                term %in% c("TV-CNN", "TV-MSNBC", "TV-Fox") ~ "Media",
                                term %in% c("Republican", "Independent") ~ "Party",
                                TRUE ~ NA_character_))


p2 <- ggplot(filter(t2, term!="Intercept"), aes(x=term, color=hypothesis)) +
  geom_point(aes(y = estimate, alpha = star)) +
  geom_linerange(aes(ymin=conf.low, ymax=conf.high, alpha=star)) +
  coord_flip(ylim = c(-0.25, 0.25)) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  labs(x="Term",
       y="Coefficient",
       title="Predictors of party-ideology sorting, 2020 ANES",
       color="Hypotheses",
       caption = caption) +
  guides(alpha="none") +
  scale_alpha_continuous(range = c(0.25, 1)) +
  scale_color_brewer(type = "qual", palette = 6, na.value="gray50") +
  facet_wrap(vars(model), nrow = 1) +
  theme(text = element_text(size=16),
        plot.caption = element_text(size=12))

ggsave("results/figures/sorting_predictors_2020.png", plot = p2, dpi=400, width=16, height=9, units="in")
ggsave("results/figures/sorting_predictors_2020.pdf", plot = p2, dpi=400, width=16, height=9, units="in")


spec <- list(
  "Dissatisfaction with democracy" = list(
    formula = update.formula(sorting_plus_controls, dem1 ~ .),
    data = anes20,
    fun = lm_robust),
  "Checks and balances aren't important" = list(
    formula = update.formula(sorting_plus_controls, dem2 ~ .),
    data = anes20,
    fun = lm_robust),
  "Helpful if president could act alone" = list(
    formula = update.formula(sorting_plus_controls, dem3 ~ .),
    data = anes20,
    fun = lm_robust),
  "Support for political violence" = list(
    formula = update.formula(sorting_plus_controls, vio_justy ~ .),
    data = anes20,
    fun = lm_robust),
  "Skeptical of election integrity" = list(
    formula = update.formula(sorting_plus_controls, fairelec ~ .),
    data = anes20,
    fun = lm_robust),
  "Compromise is selling out" = list(
    formula = update.formula(sorting_plus_controls, comp1.1 ~ .),
    data = anes20,
    fun = lm_robust),
  "Prefer leader sticks to principles" = list(
    formula = update.formula(sorting_plus_controls, comp2.1 ~ .),
    data = anes20,
    fun = lm_robust),
  "Participation" = list(
    formula = update.formula(sorting_plus_controls, MASONINDEX1 ~ .),
    data = anes20,
    fun = lm_robust),
  "Affective polarization" = list(
    formula = update.formula(sorting_plus_controls, ftdifference ~ .),
    data = anes20 |> mutate(ftdifference = ftdifference/100),
    fun = lm_robust)
)

r <- lapply(spec,
  \(x) (x$fun(x$formula, data=x$data, weights=x$data$weight) |>
          tidy() |>
          tibble() |>
          filter(term == "sorting_r"))) |>
  bind_rows() |>
  mutate(outcome = as.factor(outcome) |>
           fct_relabel(~COEFRENAMER[.x]) |>
           fct_relevel(COEFRENAMER) |>
           fct_rev())
# TODO: don't hard-code these
r$hypothesis <- c(rep("Democratic Norms", 7), rep("Participation and Affect", 2))

caption <- "Plot shows estimated relationship with sorting for each DV.\nEstimates created with OLS regression, using robust standard errors, and are weighted with ANES weights.\nShaded coefficients are significant at 95% confidence; unshaded are not."
p3 <- ggplot(r, aes(x=outcome, y=estimate, ymin=conf.low, color=hypothesis,
                    ymax=conf.high, alpha = if_else(p.value < 0.05, 1, 0))) +
  geom_point(size=4) +
  geom_linerange(linewidth=1.5) +
  coord_flip() +
  theme_bw() +
  scale_alpha_continuous(range=c(0.25, 1),
                         guide="none") +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_brewer(type="qual", palette=6) +
  labs(
    y="Coefficient",
    x="Outcome",
    title="Outcomes predicted by party-ideology sorting, 2020 ANES",
    color="Hypotheses",
    caption=caption
  ) +
  theme(text = element_text(size=16),
        plot.caption = element_text(size=12))

ggsave("results/figures/sorting_outcomes_2020.png", plot = p3, dpi=400, width=16, height=9, units="in")
ggsave("results/figures/sorting_outcomes_2020.pdf", plot = p3, dpi=400, width=16, height=9, units="in")


spec <- list(
  "Dissatisfaction with democracy" = list(
    formula = update.formula(sorting_plus_controls_16, dem1 ~ .),
    data = anes16,
    fun = lm_robust),
  "Support for political violence" = list(
    formula = update.formula(sorting_plus_controls_16, vio_justy ~ .),
    data = anes16,
    fun = lm_robust),
  "Skeptical of election integrity" = list(
    formula = update.formula(sorting_plus_controls_16, fairelec ~ .),
    data = anes16,
    fun = lm_robust),
  "Compromise is selling out" = list(
    formula = update.formula(sorting_plus_controls_16, comp1.1 ~ .),
    data = anes16,
    fun = lm_robust),
  "Prefer leader sticks to principles" = list(
    formula = update.formula(sorting_plus_controls_16, comp2.1 ~ .),
    data = anes16,
    fun = lm_robust),
  "Participation" = list(
    formula = update.formula(sorting_plus_controls_16, MASONINDEX1 ~ .),
    data = anes16,
    fun = lm_robust),
  "Affective polarization" = list(
    formula = update.formula(sorting_plus_controls_16, ftdifference ~ .),
    data = anes16 |> mutate(ftdifference = ftdifference/100),
    fun = lm_robust)
)
r <- lapply(spec,
            \(x) (x$fun(x$formula, data=x$data, weights=x$data$weight) |>
                    tidy() |>
                    tibble() |>
                    filter(term == "sorting_r"))) |>
  bind_rows() |>
  mutate(outcome = as.factor(outcome) |>
           fct_relabel(~COEFRENAMER[.x]) |>
           fct_relevel(COEFRENAMER) |>
           fct_rev())
# TODO: don't hard-code these
r$hypothesis <- c(rep("Democratic Norms", 5), rep("Participation and Affect", 2))

p4 <- ggplot(r, aes(x=outcome, y=estimate, color = hypothesis,
                    ymin=conf.low, ymax=conf.high, alpha = if_else(p.value < 0.05, 1, 0))) +
  geom_point(size=4) +
  geom_linerange(linewidth=1.5) +
  coord_flip() +
  theme_bw() +
  scale_alpha_continuous(range=c(0.25, 1),
                         guide="none") +
  scale_color_brewer(type="qual", palette=6) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(
    y="Coefficient",
    x="Outcome",
    title="Outcomes predicted by party-ideology sorting, 2016 ANES",
    color="Hypotheses",
    caption= caption
  ) +
  theme(text = element_text(size=16),
        plot.caption = element_text(size=12))

ggsave("results/figures/sorting_outcomes_2016.png", plot = p4, dpi=400, width=16, height=9, units="in")
ggsave("results/figures/sorting_outcomes_2016.pdf", plot = p4, dpi=400, width=16, height=9, units="in")

p5 <- bind_rows(
  select(anes20, sorting_r) |>
                  mutate(year=2020),
  select(anes16, sorting_r) |>
                  mutate(year=2016),
  select(anes12, sorting_r) |>
                  mutate(year=2012)) |>
  ggplot(aes(x=sorting_r)) +
  geom_histogram(binwidth=0.05) +
  facet_wrap(vars(year), ncol=1, scales="free") +
  theme_bw() +
  labs(x="Sorting",
       y="Count", title="Distribution of partisan-ideological sorting, 2012-2020",
       caption="Distributions are sourced from each year's respective ANES.\nA score of one indicates perfect sorting (strong Rep/strong con or strong Dem/strong lib)")

ggsave("results/figures/sorting_distributions.png", plot = p5, dpi=400, width=12, height=9, units="in")
ggsave("results/figures/sorting_distributions.pdf", plot = p5, dpi=400, width=12, height=9, units="in")



# Scratch ---------------------------------------------------------------

anes20 <- anes20 |> mutate(across(hannity:erin_burnett, ~map_int(., \(x) (case_when(is.na(x) ~ 0, TRUE ~ x)))))
anes16 <- anes16 |> mutate(across(hannity:cnn_en_espanol, ~map_int(., \(x) (case_when(is.na(x) ~ 0, TRUE ~ x)))))

anes20_rep <- filter(anes20, rep == 1)
anes20_dem <- filter(anes20, dem == 1)
anes20_ind <- filter(anes20, ind == 1)

anes16_rep <- filter(anes16, rep == 1)
anes16_dem <- filter(anes16, dem == 1)
anes16_ind <- filter(anes16, ind == 1)

spec <- list(
  "2016 Sorting (OLS)" = list(formula = sorting_formula_disaggregate_media_16_minus_pid,
                              data = anes16,
                              fun = lm_robust),
  "2016 Sorting (LASSO)" = list(formula = sorting_formula_disaggregate_media_16_minus_pid,
                                data = anes16,
                                fun = lasso_new),
  "2020 Sorting (OLS)" = list(formula = sorting_formula_disaggregate_media_minus_pid,
                              data = anes20,
                              fun = lm_robust),
  "2020 Sorting (LASSO)" = list(formula = sorting_formula_disaggregate_media_minus_pid,
                                data = anes20,
                                fun = lasso_new)
)
models_to_table(spec, output="results/tables/tex/disagg_media.tex",
                title = "Detailed results predicting sorting, whole sample")



spec <- list(
  "Republicans (OLS)" = list(formula = sorting_formula_disaggregate_media_minus_pid, data=anes20_rep, fun = lm_robust),
  "Republicans (LASSO)" = list(formula = sorting_formula_disaggregate_media_minus_pid, data=anes20_rep, fun = lasso_new),
  "Independents (OLS)" = list(formula = sorting_formula_disaggregate_media_minus_pid, data=anes20_ind, fun = lm_robust),
  "Independents (LASSO)" = list(formula = sorting_formula_disaggregate_media_minus_pid, data=anes20_ind, fun = lasso_new),
  "Democrats (OLS)" = list(formula = sorting_formula_disaggregate_media_minus_pid, data=anes20_dem, fun = lm_robust),
  "Democrats (LASSO)" = list(formula = sorting_formula_disaggregate_media_minus_pid, data=anes20_dem, fun = lasso_new)
)

models_to_table(spec,
                output = "results/tables/tex/disagg_media_partisan_2020.tex",
                title = "Detailed results predicting sorting, partisan categories, 2020",
                notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)


spec <- list(
  "Republicans (OLS)" = list(formula = sorting_formula_disaggregate_media_16_minus_pid, data=anes16_rep, fun = lm_robust),
  "Republicans (LASSO)" = list(formula = sorting_formula_disaggregate_media_16_minus_pid, data=anes16_rep, fun = lasso_new),
  "Independents (OLS)" = list(formula = sorting_formula_disaggregate_media_16_minus_pid, data=anes16_ind, fun = lm_robust),
  "Independents (LASSO)" = list(formula = sorting_formula_disaggregate_media_16_minus_pid, data=anes16_ind, fun = lasso_new),
  "Democrats (OLS)" = list(formula = sorting_formula_disaggregate_media_16_minus_pid, data=anes16_dem, fun = lm_robust),
  "Democrats (LASSO)" = list(formula = sorting_formula_disaggregate_media_16_minus_pid, data=anes16_dem, fun = lasso_new)
)

models_to_table(spec,
                output = "results/tables/tex/disagg_media_partisan_2016.tex",
                title = "Detailed results predicting sorting, partisan categories, 2016",
                notes = list("In 2016, the Facebook variable referred to both Facebook and Twitter use.")
)
