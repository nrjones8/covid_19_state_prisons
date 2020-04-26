delaware <- get_delaware_covid_data()

alaska <- get_alaska_covid_data(xml2::read_html("https://doc.alaska.gov/covid-19"))

arizona <- get_arizona_covid_data(xml2::read_html("https://corrections.az.gov/adcrr-covid-19-dashboard"))

iowa   <- get_iowa_covid_data(xml2::read_html("https://doc.iowa.gov/COVID19"))

kansas <- get_ks_covid_data(xml2::read_html("https://www.doc.ks.gov/kdoc-coronavirus-updates/kdoc-covid-19-status"))

louisiana <- get_la_covid_data(xml2::read_html("https://doc.louisiana.gov/doc-covid-19-testing/"))

pennsylvania <- get_pa_covid_data(xml2::read_html("https://www.cor.pa.gov/Pages/COVID-19.aspx"))

south_carolina <- get_sc_covid_data(xml2::read_html("http://www.doc.sc.gov/covid.html"))

texas <- get_texas_covid_data(xml2::read_html("https://www.tdcj.texas.gov/covid-19/offender_mac.html"))

utah <- get_utah_covid_data(xml2::read_html("https://corrections.utah.gov/index.php/home/alerts-2/1237-udc-coronavirus-updates"))

ohio <- get_ohio_covid_data(xml2::read_html("https://drc.ohio.gov/Family/COVID-19-UPDATES"))
ohio <- ohio$ohio_facility

oklahoma <- get_oklahoma_covid_data(xml2::read_html("http://doc.ok.gov/"))
oklahoma <- oklahoma$ok_facilities

virginia <- get_virginia_covid_data(xml2::read_html("https://vadoc.virginia.gov/news-press-releases/2020/covid-19-updates/"))

north_carolina <- get_nc_covid_data(xml2::read_html("https://opus.doc.state.nc.us/DOPCovid19Stats/services/facilitystatsServlet"))

north_dakota <- get_nd_covid_data(xml2::read_html("https://www.docr.nd.gov/covid-19-information"))

montana <- get_montana_covid_data(xml2::read_html("https://cor.mt.gov/COVID-19"))

missouri <- get_missouri_covid_data(xml2::read_html("https://doc.mo.gov/media-center/newsroom/covid-19/data"))

minnesota <- get_minnesota_covid_data(xml2::read_html("https://mn.gov/doc/about/covid-19-updates/"))

florida <- get_fl_covid_data(xml2::read_html("http://www.dc.state.fl.us/comm/covid-19.html"))

georgia <- get_georgia_covid_data(xml2::read_html("http://www.dcor.state.ga.us/content/cases"))

alabama <- get_ala_covid_data(xml2::read_html("http://www.doc.alabama.gov/covid19news"))

federal <- get_federal_data(xml2::read_html("https://www.bop.gov/coronavirus/json/final.json?_=1587149952012"))

illinois <- get_illinois_covid_data(xml2::read_html("https://www2.illinois.gov/idoc/facilities/Pages/Covid19Response.aspx"))

#indiana <- get_indiana_covid_data(xml2::read_html("https://www.in.gov/idoc/3780.htm"))

new_hampshire <- get_new_hampshire_covid_data(xml2::read_html("https://www.nh.gov/nhdoc/covid/index.html"))

new_york <- get_nys_covid_data(xml2::read_html("https://doccs.ny.gov/doccs-covid-19-report"))
new_york <- new_york$facilities

idaho <- get_idaho_covid_data(xml2::read_html("https://www.idoc.idaho.gov/content/careers/covid-19"))

# vermont <- get_vermont_covid_data(xml2::read_html("https://doc.vermont.gov/covid-19-information-page"))

washington <- get_washington_covid_data(xml2::read_html("https://www.doc.wa.gov/news/covid-19.htm#status"))

federal <- get_federal_data(xml2::read_html("https://www.bop.gov/coronavirus/json/final.json?_=1587149952012"))


new_jersey <- get_nj_covid_data(xml2::read_html("https://www.state.nj.us/corrections/pages/COVID19Updates.shtml"))
