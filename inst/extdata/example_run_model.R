
# file = system.file("extdata/plumber.R", package = "distribglm")
# plumber::plumb(file=file)$run(port = 8000)
site_name = "site1"
url = "http://138.68.235.233/glm"
model_name = "example_model"
data_filename = system.file("extdata",
                            paste0("data_", site_name, ".csv"),
                            package = "distribglm")
data = readr::read_csv(data_filename)

###################################################
# Install the packages if not available
###################################################
if (!requireNamespace("distribglm", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("muschellij2/distribglm")
}
library(distribglm)


# if you want to check the model specification
model_spec = api_model_specification(
  url = url,
  model_name = model_name)
print(model_spec)
###########################################################
# Site data Run
###########################################################
model = api_estimate_model(
  url = url, model_name = model_name,
  data = data,
  site_name = site_name,
  wait_time = 1,
  shuffle_rows = TRUE)

