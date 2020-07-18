context("Creating new system files and model templates")
test_that("System examples", {

  require(tools)
  #Testing the template file
  expect_true(system_new(system_file="template", file_name="system_template.txt", overwrite=TRUE), info="copying system template")
  expect_true(md5sum("system_template.txt") == md5sum(system.file("ubinc", "templates", "system_template.txt", package="ubiquity")))

  examples = c("mab_pk", "pbpk", "pwc", "tmdd", "adapt", 
               "two_cmt_cl", "two_cmt_micro", "one_cmt_cl", "one_cmt_micro"  )

  # This gets the names of all the example system files:
  sfs     = system_new_list()
  
  for(sf_ex in names(sfs)){
    # Making sure we can create the files:
    expect_true(system_new(system_file       = sf_ex,   
                           file_name         = "system_test.txt", 
                           output_directory  = tempdir(),
                           overwrite         = TRUE), info=sprintf("system = %s", sf_ex))
    # Comparing checksums:
    expect_true(md5sum(file.path(tempdir(), "system_test.txt")) == md5sum(sfs[[sf_ex]]$file_path))
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
  fr = system_new(file_name        = "system.txt", 
                  system_file      = "mab_pk", 
                  overwrite        = TRUE, 
                  output_directory = tempdir())
  # building the system file
  cfg = build_system(system_file  = file.path(tempdir(), "system.txt"),
                     output_directory          = file.path(tempdir(), "output"),
                     temporary_directory       = tempdir())

  for(template in unique(tgen[,1])){
    expect_true(system_fetch_template(cfg, 
                                      output_directory = tempdir(),
                                      template         = template, 
                                      overwrite        =TRUE)$isgood, info=sprintf("template = %s", template))
    fnames = tgen[tgen[,1] == template, 2]
    for(fname in fnames){
      expect_true(file.exists(file.path(tempdir(),fname)), info=sprintf("%s --> %s", template, fname))
    }
  }
})

