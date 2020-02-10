testthat::context("Estimate a model with a few steps")
data = data.frame(y = c(0, 0, 1),
                  pois_y = c(4, 1, 0),
                  x2 = c(-2.19021287072066,
                         -0.344307138450805, 3.47215796952745),
                  x1 = c(-0.263859503846267,
                         -0.985160029707486, 0.227262373184513))
tdir = tempfile()
dir.create(tdir)
model_name = "logistic_example"
form_file = setup_model(model_name = model_name,
                        synced_folder = tdir,
                        formula =  "y ~ x1 + x2", family =  "binomial")

testthat::test_that("multiplication works", {

  outfile = estimate_site_gradient(
    model_name = model_name, synced_folder = tdir,
    all_site_names = "site1",
    data = data)
  outfile = estimate_site_gradient(
    model_name = model_name, synced_folder = tdir,
    all_site_names = "site1",
    data = data)
  outfile = estimate_site_gradient(
    model_name = model_name, synced_folder = tdir,
    all_site_names = c("site1", "site2"),
    data = data)
  clear_model(model_name, tdir)
  testthat::expect_error({
    outfile = estimate_site_gradient(
      model_name = model_name, synced_folder = tdir,
      all_site_names = "site1",
      data = data)
  })
})
