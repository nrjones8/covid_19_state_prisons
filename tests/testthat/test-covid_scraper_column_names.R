test_that("column names are as expected", {
  expect_named(delaware,
               c("facilities",
                 "staff_positive", 
                 "contract_staff_positive",
                 "inmates_positive",
                 "state",
                 "scrape_date"))

  
  expect_named(alaska,
               c("inmates_tested",
                 "inmates_positive", 
                 "inmates_negative",
                 "inmates_pending",
                 "state",
                 "scrape_date"))
  
  expect_named(arizona,
               c("facilities",
                 "inmates_tested", 
                 "inmates_negative",
                 "inmates_positive",
                 "inmates_pending",
                 "daily_total_population",
                 "state",
                 "scrape_date"))
  
  expect_named(iowa,
               c("facilities",
                 "inmates_tested", 
                 "inmates_positive",
                 "staff_positive",
                 "state",
                 "scrape_date"))
  
  expect_named(kansas,
               c("facilities",
                 "staff_positive", 
                 "inmates_positive",
                 "staff_recovered",
                 "inmates_recovered",
                 "state",
                 "scrape_date"))
  
  expect_named(louisiana,
               c("facilities",
                 "inmates_positive",
                 "inmates_current_positive",
                 "inmates_step_down",
                 "inmates_recovered",
                 "inmates_covid_deaths_underlying_conditions",
                 "inmates_deaths",
                 "inmates_deaths",
                 "staff_positive",
                 "staff_recovered",
                 "staff_deaths",
                 "state",
                 "scrape_date"))
  
  expect_named(pennsylvania,
               c("facilities",
                 "staff_positive",
                 "staff_negative",
                 "staff_deaths",
                 "inmates_positive",
                 "inmates_negative",
                 "inmates_deaths",
                 "state",
                 "scrape_date"))
  
  expect_named(south_carolina,
               c("facilities",
                 "staff_positive",
                 "inmates_positive",
                 "state",
                 "scrape_date"))
  
  
  expect_named(texas,
               c("facilities",
                 "inmates_pending",
                 "inmates_negative",
                 "inmates_positive",
                 "inmates_medical_restriction",
                 "inmates_medical_isolation",
                 "state",
                 "scrape_date"))
  
  expect_named(utah,
               c("inmates_positive",
                 "state",
                 "scrape_date"))
  
  expect_named(minnesota,
               c("facilities",
                 "inmates_tested",
                 "inmates_positive",
                 "inmates_negative",
                 "inmates_pending",
                 "inmates_presumed_positive",
                 "inmates_recovered",
                 "inmates_hospitalized",
                 "inmates_deaths",
                 "state",
                 "scrape_date"))
  
  
  expect_named(ohio,
               c("facilities",
                 "staff_positive",
                 "staff_deaths",
                 "staff_recovered",
                 "units_in_quarantine",
                 "inmates_quarantine",
                 "housing_type",
                 "inmates_isolation",
                 "inmates_positive",
                 "inmates_deaths_probable",
                 "inmates_deaths_confirmed",
                 "state",
                 "scrape_date"))
  
  expect_named(oklahoma,
               c("facilities",
                 "inmates_positive",
                 "units_in_quarantine",
                 "inmates_quarantine",
                 "housing_type",
                 "inmates_isolation",
                 "staff_positive",
                 "state",
                 "scrape_date"))
  
  expect_named(virginia,
               c("facilities",
                 "inmates_positive",
                 "inmates_hospital",
                 "inmates_deaths",
                 "staff_positive",
                 "state",
                 "scrape_date"))
  

  expect_named(north_carolina,
               c("facilities",
                 "inmates_tested",
                 "inmates_positive",
                 "inmates_negative",
                 "state",
                 "scrape_date"))
  
  expect_named(north_dakota,
               c("facilities",
                 "inmates_positive",
                 "inmates_negative",
                 "inmates_medical",
                 "inmates_recovered",
                 "inmates_deaths",
                 "state",
                 "scrape_date"))
  
  expect_named(missouri,
               c("inmates_positive",
                 "inmates_deaths",
                 "contract_staff_positive",
                 "staff_positive",
                 "staff_deaths",
                 "state",
                 "scrape_date"))
  
  expect_named(florida,
               c("facilities",
                 "inmates_security_quarantine",
                 "inmates_medical_quarantine",
                 "inmates_medical_isolation",
                 "inmates_pending",
                 "inmates_positive",
                 "staff_positive",
                 "state",
                 "scrape_date"))
  
  expect_named(georgia,
               c("facilities",
                 "staff_positive",
                 "inmates_positive",
                 "staff_recovered",
                 "inmates_recovered",
                 "staff_deaths",
                 "inmates_deaths",
                 "state",
                 "scrape_date"))
  
  expect_named(alabama,
               c("facilities",
                 "inmates_tested",
                 "inmates_pending",
                 "inmates_positive",
                 "inmates_deaths",
                 "state",
                 "scrape_date"))
  
  expect_named(illinois,
               c("facilities",
                 "staff_positive",
                 "staff_recovered",
                 "inmates_positive",
                 "inmates_recovered",
                 "state",
                 "scrape_date"))
  
  # expect_named(indiana,
  #              c("staff_positive",
  #                "staff_deaths",
  #                "inmates_isolated",
  #                "inmates_isolated_suspected_symptomatic",
  #                "inmates_quarantined",
  #                "type_of_housing",
  #                "inmates_positive",
  #                "inmates_deaths_probable",
  #                "inmates_deaths_confirmed",
  #                "state",
  #                "scrape_date"))
  
  expect_named(new_hampshire,
               c("facilities",
                 "staff_positive",
                 "inmates_tested",
                 "inmates_positive",
                 "state",
                 "scrape_date"))
  
  expect_named(new_york,
               c("facilities",
                 "inmates_recovered",
                 "inmates_deaths",
                 "inmates_positive",
                 "inmates_pending",
                 "inmates_negative",
                 "state",
                 "scrape_date"))
  
  expect_named(idaho,
               c("inmates_tested",
                 "inmates_pending",
                 "inmates_positive",
                 "inmates_negative",
                 "state",
                 "scrape_date"))
  
  expect_named(new_jersey,
               c("facilities",
                 "staff_positive",
                 "inmates_positive",
                 "inmates_deaths",
                 "state",
                 "scrape_date"))
  
  # expect_named(vermont,
  #              c("inmates_tests",
  #                "inmates_positive",
  #                "inmates_negative",
  #                "inmates_pending",
  #                "inmates_medical_isolation",
  #                "inmates_recovered",
  #                "inmcates_positive_currently_incarcerated",
  #                "inmates_hospital",
  #                "inmates_hosptal_discharges",
  #                "state",
  #                "scrape_date"))
  
  expect_named(federal,
               c("facilities",
                 "contract_number",
                 "inmates_recovered",
                 "inmates_deaths",
                 "inmates_positive",
                 "staff_positive",
                 "staff_recovered",
                 "staff_deaths",
                 "state",
                 "scrape_date"))
})
