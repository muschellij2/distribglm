#
# This is a Plumber API. In RStudio 1.2 or newer you can run the API by
# clicking the 'Run API' button above.
#
# In RStudio 1.1 or older, see the Plumber documentation for details
# on running the API.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(distribglm)
library(readr)

# data source is somewhere else! PRIVATE DATA

data_source = "/path/to/data.csv"
# data_source = paste0(
#   "~/Dropbox/Projects/",
#   "distributed_model/data/logistic_data_Michael.csv")

# this may need to be changed if the data isn't CSV
# data = readr::read_csv(data_source)

#* @apiTitle Calculate gradient
#* @apiDescription A list of estimated values, including the gradient,
#* sample size, iteration number, covariance matrix (A_mat),
#* number of samples with non-zero weights, the sum of the dispersion
#* values (for overdispersion estimates), and a vector of values
#* for combining to create the population gradient (u), with length
#* of the number of beta values
#*
#* @param beta current beta value, leave NULL to initialize,
#@ otherwise a vector of comma-separated values
#* @param formula:character model formula to fit, with tilde syntax
#* @param family:character generalized linear model family, e.g. "binomial"
#* @param iteration_number:int number of fitting iteration, used for tracking
#* @param shuffle_rows:bool should the rows of the dataset be permuted, so
#* as to decrease privacy concerns
#*
#* @get /gradient_value
function(beta = NULL, formula,
         family = "binomial",
         iteration_number = NULL,
         shuffle_rows = TRUE) {
  if (!is.null(beta)) {
    if (is.character(beta)) {
      beta = strsplit(beta, ",")[[1]]
    }
    beta = as.numeric(beta)
  }
  formula = as.formula(formula)
  if (is.character(family)) {
    family = get(family, envir = .BaseNamespaceEnv)
  }
  if (is.function(family)) {
    family = family()
  }
  if (is.null(iteration_number)) {
    iteration_number = 0
  }
  gradient_value(beta = beta,
                 formula = formula,
                 data = data,
                 family = family,
                 shuffle_rows = shuffle_rows)

}


