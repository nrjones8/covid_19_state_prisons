source(here::here("R/utils.R"))

# Oregon  -----------------------------------------------------------------
# https://www.oregon.gov/doc/research-and-requests/Pages/research-and-statistics.aspx
get_oregon_historical <- function() {
  download.file("https://www.oregon.gov/doc/Documents/offender-population-trends.pdf",
                destfile = here::here("data/raw_data/oregon_offender_population_trends.pdf"),
                mode = "wb")
}


# California --------------------------------------------------------------
# https://www.cdcr.ca.gov/research/weekly-total-population-report-archive-2/

get_california_historical <- function() {
  setwd(here::here("data/raw_data/california"))
  links2020 <- 
    read_html("https://www.cdcr.ca.gov/research/weekly-total-population-report-archive-2020/") %>%
    html_nodes("a") %>%
    html_attr('href')
  
  
  links2019 <- 
    read_html("https://www.cdcr.ca.gov/research/weekly-total-population-report-archive-2019/") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  links <- c(links2020,
             links2019)
  links <- grep("uploads/sites.*pdf$", links, value = TRUE)
  links[-grep("^https", links)] <- paste0("https://www.cdcr.ca.gov/",
                                          links[-grep("^https", links)])
  link_name <- gsub(".*/", "", links)
  for (i in 1:length(links)) {
    download.file(links[i], destfile = link_name[i], mode = "wb")
  }
}


# Hawaii ------------------------------------------------------------------
get_hawaii_historical <- function() {
  setwd(here::here("data/raw_data/hawaii"))
  links <- 
    read_html("https://dps.hawaii.gov/about/divisions/corrections/") %>%
    html_nodes("a") %>%
    html_attr('href')
  
  links <- grep("Pop-Reports.*pdf$", links, value = TRUE)
  link_name <- gsub(".*/", "", links)
  for (i in 1:length(links)) {
    download.file(links[i], destfile = link_name[i], mode = "wb")
  }
}


# Indiana -----------------------------------------------------------------
get_indiana_historical <- function() {
  setwd(here::here("data/raw_data/indiana"))
  links <- 
    read_html("https://www.in.gov/idoc/2376.htm") %>%
    html_nodes("a") %>%
    html_attr('href')
  
  links <- grep("Total.*Population.*pdf$", links, value = TRUE)
  link_name <- gsub(".*/", "", links)
  link_name <- gsub("%20", "_", link_name)
  link_name <- gsub("\\.+", ".", link_name)
  links <- paste0("https://www.in.gov", links)
  for (i in 1:length(links)) {
    download.file(links[i], destfile = link_name[i], mode = "wb")
  }
}

# Georgia -----------------------------------------------------------------
get_georgia_historical <- function() {
  setwd(here::here("data/raw_data/georgia"))
  links <- 
    read_html("http://www.gdc.ga.gov/Research/Monthly_Profile_all_inmates") %>%
    html_nodes("a") %>%
    html_attr('href')
  
  links <- grep("Profile_all_inmates.*pdf$", links, value = TRUE)
  links <- gsub("20.pdf</a></li>.pdf", "2003_09.pdf", links)
  links <- gsub("202006_10", "2006_10", links)
  link_name <- gsub(".*/", "", links)
  links <- paste0("http://www.gdc.ga.gov", links)
  for (i in 1:length(links)) {
    tryCatch({download.file(links[i], destfile = link_name[i], mode = "wb")}, error = function(e){})
  }
}



# Pennsylvania ------------------------------------------------------------
get_pennsylvania_historical <- function() {
  setwd(here::here("data/raw_data/pennsylvania"))
  links <- 
    read_html("https://www.cor.pa.gov/About%20Us/Statistics/Pages/Monthly-Population-Reports.aspx") %>%
    html_nodes("a") %>%
    html_attr('href')
  
  links <- grep("Monthly.*Report.*pdf$", links, value = TRUE)
  links <- links[-grep("Current.*pdf$", links)]
  link_name <- gsub(".*/", "", links)
  links <- paste0("https://www.cor.pa.gov", links)
  for (i in 1:length(links)) {
    tryCatch({
      download.file(links[i], destfile = link_name[i], mode = "wb")}, 
      error = function(e){
        Sys.sleep(60)
        download.file(links[i], destfile = link_name[i], mode = "wb")
      })
  }
}



# Rhode Island ------------------------------------------------------------


