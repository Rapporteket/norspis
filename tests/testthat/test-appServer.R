# store current instance and set temporary config
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

# make pristine and dedicated config to avoid interference with other tests
Sys.setenv(R_RAP_CONFIG_PATH = file.path(tempdir(), "autoReportTesting"))
dir.create(Sys.getenv("R_RAP_CONFIG_PATH"))
file.copy(system.file(c("rapbaseConfig.yml", "dbConfig.yml", "autoReport.yml"),
                      package = "rapbase"),
          Sys.getenv("R_RAP_CONFIG_PATH"))

registryName <- "rapbase"


# rapbase modules are already tested. For now, just make sure the server runs
# by dummy class test of auto report list
test_that("server can run", {
  shiny::testServer(app = appServer, {
    expect_equal(class(report), "list")
  })
})


# Restore instance
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
