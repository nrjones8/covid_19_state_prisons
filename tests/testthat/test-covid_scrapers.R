test_that("scrapers are silent", {
  expect_silent(get_alaska_covid_data(xml2::read_html("https://doc.alaska.gov/covid-19")))
  

  
  
  expect_silent(get_delaware_covid_data(xml2::read_html("https://doc.delaware.gov/views/covid19.blade.shtml")))
  
  expect_silent(get_georgia_covid_data(xml2::read_html("http://www.dcor.state.ga.us/content/cases")))
  
  expect_silent(get_illinois_covid_data(xml2::read_html("https://www2.illinois.gov/idoc/facilities/Pages/Covid19Response.aspx")))
  
  expect_silent(get_pa_covid_data(xml2::read_html("https://www.cor.pa.gov/Pages/COVID-19.aspx")))
  
  expect_silent(get_ala_covid_data(xml2::read_html("http://www.doc.alabama.gov/covid19news")))
  
  expect_silent(get_arizona_covid_data(xml2::read_html("https://corrections.az.gov/adcrr-covid-19-dashboard")))
  
  expect_silent(get_idaho_covid_data(xml2::read_html("https://www.idoc.idaho.gov/content/careers/covid-19")))
  
  expect_silent(get_federal_data(xml2::read_html("https://www.bop.gov/coronavirus/json/final.json?_=1587149952012")))
  
  expect_silent(get_fl_covid_data(xml2::read_html("http://www.dc.state.fl.us/comm/covid-19.html")))
  
  expect_silent(get_ks_covid_data(xml2::read_html("https://www.doc.ks.gov/kdoc-coronavirus-updates/kdoc-covid-19-status")))
  
  expect_silent(get_la_covid_data(xml2::read_html("https://doc.louisiana.gov/doc-covid-19-testing/")))
  
  expect_silent(get_nys_covid_data(xml2::read_html("https://doccs.ny.gov/doccs-covid-19-report")))
  
  expect_silent(get_ohio_covid_data(xml2::read_html("https://drc.ohio.gov/")))
  
  expect_silent(get_nj_covid_data(xml2::read_html("https://www.state.nj.us/corrections/pages/COVID19Updates.shtml")))
  
  expect_silent(get_nc_covid_data(xml2::read_html("https://opus.doc.state.nc.us/DOPCovid19Stats/services/facilitystatsServlet")))

  expect_silent(get_nd_covid_data(xml2::read_html("https://www.docr.nd.gov/covid-19-information")))
  
  expect_silent(get_minnesota_covid_data(xml2::read_html("https://mn.gov/doc/about/covid-19-updates/")))
  
  expect_silent(get_vermont_covid_data(xml2::read_html("https://doc.vermont.gov/covid-19-information-page")))
  
  expect_silent(get_sc_covid_data(xml2::read_html("http://www.doc.sc.gov/covid.html")))

  expect_silent(get_virginia_covid_data(xml2::read_html("https://vadoc.virginia.gov/news-press-releases/2020/covid-19-updates/")))
  
  expect_silent(get_washington_covid_data(xml2::read_html("https://www.doc.wa.gov/news/covid-19.htm#status")))
  
  expect_silent(get_texas_covid_data(xml2::read_html("https://www.tdcj.texas.gov/covid-19/offender_mac.html")))
  
  expect_silent(get_california_covid_data(xml2::read_html("https://www.cdcr.ca.gov/covid19/cdcr-cchcs-covid-19-status/")))
  
  expect_silent(get_montana_covid_data(xml2::read_html("https://cor.mt.gov/COVID-19")))
  
  expect_silent(get_iowa_covid_data(xml2::read_html("https://doc.iowa.gov/COVID19")))
  
  expect_silent(get_utah_covid_data(xml2::read_html("https://corrections.utah.gov/index.php/home/alerts-2/1237-udc-coronavirus-updates")))
  
  expect_silent(get_indiana_covid_data(xml2::read_html("https://www.in.gov/idoc/3780.htm")))
  
  expect_silent(get_new_hampshire_covid_data(xml2::read_html("https://www.nh.gov/nhdoc/covid/index.html")))
  
  expect_silent(get_oklahoma_covid_data(xml2::read_html("http://doc.ok.gov/")))
  
  expect_silent(get_missouri_covid_data(xml2::read_html("https://doc.mo.gov/media-center/newsroom/covid-19/data")))
  
  
  })
