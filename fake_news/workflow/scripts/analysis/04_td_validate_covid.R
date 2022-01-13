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
  library("projpred")
  # Graphics:
  library("bayesplot")
  color_scheme_set("brightblue")
})
print("Loaded packages")


# 2.0 EXTERNAL VALIDATION -------------------------------------------------

# The submodel is validated on the COVID-19 data.

varsel_td_pol <- readRDS(snakemake@input[["varsel_td_pol"]])
covid_dat <- readRDS(snakemake@input[["covid_data"]])

pred_test <- projpred::proj_linpred(
  varsel_td_pol,
  newdata = covid_dat,
  nterms = suggest_size(varsel_td_pol),
  integrated = TRUE
)

attributes(pred_test)

length(pred_test$pred)

# This is R2 with the 8 predictors seleced from the political news data.
cor(pred_test$pred, covid_dat$discernment)^2
# This is the R2 for the COVID-19 data, when the predicted values are computed
# by using the 8 predictors selected from the political news data, with the
# same coefficients computed from the political news data.

# Plot of the predicted values from the CV subset model and the observed 
# COVID-19 data.
ggplot() +
  geom_point(aes(x = pred_test$pred, covid_dat$discernment)) +
  geom_abline(slope = 1, color = "red") +
  labs(x = "yhat submodel CV", y = "y")

ggsave(snakemake@output[["plot_1"]], width=8, height = 5)



## End of file (eof). ----------
