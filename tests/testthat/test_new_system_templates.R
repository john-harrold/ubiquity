context("Creating new system files and model templates")
test_that("System examples", {

  require(tools)
  #Testing the template file
  expect_true(system_new(system_file="template", file_name="system_template.txt", overwrite=TRUE), info="copying system template")
  expect_true(md5sum("system_template.txt") == md5sum(system.file("ubinc", "templates", "system_template.txt", package="ubiquity")))

  examples = c("mab_pk", "pbpk", "pwc", "tmdd", "adapt", 
               "two_cmt_macro", "two_cmt_micro", "one_cmt_macro", "one_cmt_micro"  )

  for(example in examples){
    expect_true(system_new(system_file=example, file_name="system_test.txt", overwrite=TRUE), info=sprintf("system = %s", example))
    expect_true(md5sum("system_test.txt") == md5sum(system.file("ubinc", "systems", sprintf("system-%s.txt", example), package="ubiquity")))
  }
})

test_that("Template files", {
  tgen = c("ShinyApp",   "ubiquity_app.R",
           "ShinyApp",   "ui.R",
           "ShinyApp",   "server.R",
           "Simulation", "analysis_simulate.R",
           "Estimation", "analysis_estimate.R")

  tgen = matrix(data = tgen, ncol=2, byrow=TRUE)

  # creating a system file
  system_new(system_file="mab_pk", file_name="system.txt", overwrite=TRUE)

  # building the system file
  cfg = build_system()


  for(template in unique(tgen[,1])){
    expect_true(system_fetch_template(cfg, template = template, overwrite=TRUE)$isgood, info=sprintf("template = %s", template))
    fnames = tgen[tgen[,1] == template, 2]
    for(fname in fnames){
      expect_true(file.exists(fname), info=sprintf("%s --> %s", template, fname))
    }
  }
})

