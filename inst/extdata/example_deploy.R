library(distribglm)
library(analogsea)
droplet = droplets()
application_name = "glm"
if (length(droplet) > 0) {
  droplet = droplet[[1]]
} else {
  droplet = do_deploy_glm_api(application_name = application_name)
}
droplet$application_name = application_name

# Get the URL
url = droplet$networks$v4
if (length(url) == 0) {
  url = droplet$networks$v6
}
url = url[[1]]$ip_address
url = paste0(url, "/", droplet$application_name)
url = paste0("http://", url)
print(url)
