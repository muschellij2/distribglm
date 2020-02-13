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


url = "http://138.68.235.233/glm"
model_name = "example_model"
all_site_names = c("site1", "site2")

setup = api_setup_model(
  url = url,
  model_name = model_name,
  formula = "y ~ x1 + x2",
  family = "binomial",
  link = "logit",
  all_site_names = all_site_names)
setup

#########################################
# Print Model Specification
#########################################
model_spec = api_model_specification(
  url = url,
  model_name = model_name)
model_spec
