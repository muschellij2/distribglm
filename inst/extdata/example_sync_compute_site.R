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

#######################################
# Change these accordingly
#######################################
# which model are we running
model_name = "example_model"
# names of all the sites
# all_site_names = c("site1", "site2")
all_site_names = "site1"
# synced shared folder for model fitting
synced_folder = "~/Dropbox/Projects/distributed_model"


setup = setup_model(model_name, synced_folder = synced_folder,
                      formula = y ~ x1 + x2,
                      clear_model = TRUE,
                      all_site_names = all_site_names,
                      family = "quasibinomial", link = "logit")
setup = readr::read_rds(setup)
setup

#################################################
# Different folder by site
#################################################
final_file = model_output_file(model_name, synced_folder)

while (!file.exists(final_file)) {
  run = estimate_new_beta(
    model_name,
    synced_folder,
    all_site_names = all_site_names,
    tolerance = 1e-7)
  Sys.sleep(5)

}
result = readr::read_rds(final_file)
