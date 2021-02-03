library(drake)

the_plan <-
  drake_plan(
  irs_raw = load_soi(),
  full_irs_clean = clean_soi(irs_raw),
  irs_app_data = target(
    prepare_app_data(full_irs_clean),
    format = "fst_dt"
  ),
  save_irs_app = write_fst(
    irs_app_data[, c(
      "year",
      "zipcode",
      "state",
      "county",
      "agi_level",
      "post_office_city",
      "n1",
      "a00100",
      "total_tax"
    )], 
    file_out("irs_app.fst"), 
    compress = 100),
  deployment = rsconnect::deployApp(
    appFiles = file_in(
      "irs_app.fst",
      "app.R"
    ),
    appName = "irs_dash",
    forceUpdate = TRUE
  )
)
