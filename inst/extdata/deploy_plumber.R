library(analogsea)
library(plumber)
library(magrittr)
library(distribglm)

# do_deploy_glm_api()
droplet <- plumber::do_provision(...)
analogsea::install_r_package(droplet,
                             c("readr", "remotes",
                               "httr", "jsonlite") )
install_github_r_package = function(droplet, repo) {
  analogsea::droplet_ssh(
    droplet,
    sprintf("Rscript -e \"remotes::install_github('%s')\"",
            repo))
}

droplet_ls = function(droplet, path) {
  analogsea::droplet_ssh(
    droplet,
    sprintf("ls %s", path))
}


install_github_r_package(droplet, "muschellij2/distribglm")
local_file = system.file("extdata/compute_plumber.R", package = "distribglm")

tdir = tempfile()
tdir = path.expand(tdir)
dir.create(tdir, recursive = TRUE)
file.copy(local_file, file.path(tdir, "plumber.R"))

plumberDeploy::do_deploy_api(
  droplet,
  path = "glm",
  localPath = tdir,
  port = 8000,
  swagger = TRUE,
  forward = TRUE)
