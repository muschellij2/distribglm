#######################################
# Change these accordingly
#######################################

# which model are we running
model_name = "example_model"

# The name of this site - for naming and checking
site_name = "site1"

# synced shared folder for model fitting
synced_folder = "~/Dropbox/Projects/distributed_model"

# data source is somewhere else! PRIVATE DATA
data_filename = system.file("extdata",
                            paste0("data_", site_name, ".csv"),
                            package = "distribglm")
data = readr::read_csv(data_filename)




##################################################
# Do not change below here
##################################################
if (!requireNamespace("distribglm", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("muschellij2/distribglm")
}
library(distribglm)

final_file = model_output_file(model_name, synced_folder)
result = estimate_model(
  model_name = model_name,
  synced_folder = synced_folder,
  data = data,
  site_name = site_name,
  data = data,
  wait_time = 1)


site_mod = glm(result$setup$formula, data = data,
               family = result$setup$family)
summary(site_mod)
se = sqrt(diag(result$covariance))
cbind(Estimate = c(result$beta), se = se, t.value=c(result$beta/se))
vcov(site_mod)
result$covariance
