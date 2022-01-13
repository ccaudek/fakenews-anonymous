log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")


# 1.0 LIBRARIES -----------------------------------------------------------

suppressPackageStartupMessages({
  # Data Manipulation:
  library("tidyr") # pivot_longer
  library("tidyverse")
  library("here")
  # Modeling:
  library("cmdstanr")
  library("brms")
  # Graphics:
  library("bayesplot")
  color_scheme_set("brightblue")
  library("patchwork")
})
print("Loaded packages")



# 2.0 GET VARSEL OBJECT ---------------------------------------------------

varsel_td_pol <- readRDS(snakemake@input[["varsel_td_pol"]])
pol_dat <- readRDS(snakemake@input[["pol_data"]])
td_ref_pol_fit <- readRDS(snakemake@input[["fit_ref"]])
summary(pol_dat)

# Perform projection onto submodels of selected sizes or a specified feature
# combination.
proj1 <- projpred::project(
  varsel_td_pol,
  nterms = projpred::suggest_size(varsel_td_pol),
  seed = 123,
  ndraws = 2000
)
attributes(proj1)

# Compute the mean of the predictive distribution and evaluates the
# log density at the training points using the 8 most relevant variables.
pred_train_subset <- projpred::proj_linpred(
  varsel_td_pol,
  newdata = pol_dat,
  nterms = projpred::suggest_size(varsel_td_pol),
  integrated = TRUE
)

predvsreal_subset_fit <- tibble(
  predicted = pred_train_subset$pred,
  real = pol_dat$discernment
) %>%
  mutate(
    error = predicted - real
  )

# RMSE subset
round(sqrt(mean(predvsreal_subset_fit$error^2)), 3)

# Correlation between the submodel's predicted values and the y data.
cor(pred_train_subset$pred, pol_dat$discernment)

# R2 of the submodel
cor(pred_train_subset$pred, pol_dat$discernment)^2

# Plot of predicted values from the subset model and predicted values from
# the complete model.
ggplot() +
  geom_point(aes(x = pred_train_subset$pred, predict(td_ref_pol_fit)[, 1])) +
  geom_abline(slope = 1, color = "red") +
  labs(x = "yhat submodel", y = "yhat complete model")

ggsave(snakemake@output[["plot_1"]], width=8, height = 5)

# Plot of predicted values from the complete model and the observed data.
ggplot() +
  geom_point(aes(x = predict(td_ref_pol_fit)[, 1], pol_dat$discernment)) +
  geom_abline(slope = 1, color = "red") +
  labs(x = "yhat complete model", y = "y")

ggsave(snakemake@output[["plot_2"]], width=8, height = 5)

# Plot of predicted values from the subset model and the observed data.
ggplot() +
  geom_point(aes(x = pred_train_subset$pred, pol_dat$discernment)) +
  geom_abline(slope = 1, color = "red") +
  labs(x = "yhat submodel", y = "y")

ggsave(snakemake@output[["plot_3"]], width=8, height = 5)


# * 2.1 Subset fit with optimal coefficients ----

predictor_variables <-
  projpred::solution_terms(varsel_td_pol)[1:projpred::suggest_size(varsel_td_pol)]
# generate the model's formula by using the number
# of predictors suggested by projpred.
brm_formula <- formula(
  paste("discernment ~ ", paste(predictor_variables, collapse = " + "))
)

td_proj_pol_fit <- brms::brm(
  brm_formula,
  data = pol_dat, 
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept"),
    prior(student_t(4, 0, 1), class = "sigma")
  ),
  iter = 20000L,
  inits = 0.01,
  chains = 4L,
  cores = parallel::detectCores(),
  backend = "cmdstan",
  # file = here::here("code", "scripts", "models", NAME_SAVED_MODEL),
  seed = 123456
)
td_proj_pol_fit <- brms::add_criterion(td_proj_pol_fit, "loo")

loo(td_proj_pol_fit)

# The projected submodel's R2 is very similar (but not identical) when
# computed with projpred and with bayes_R2().
bayes_R2(td_proj_pol_fit, cores = 4) %>%
  round(3)
#    Estimate Est.Error  Q2.5 Q97.5
# R2    0.335     0.029 0.275  0.39

print(summary(td_proj_pol_fit), 3)


# CONCLUSION: When using 8 instead of 14 predictors, R2 decreases from
#  0.353 to 0.332.


## End of file (eof). ----------
