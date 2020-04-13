get_all_current_website_pages <- function() {
  sheet_info <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1CwD8aie_ib1wj3FtqACK3N2xssT0W_vX3d_WkKGpdOw/")
  sheet_info$state <- tolower(sheet_info$state)
  sheet_info$state <- gsub(" ", "_", sheet_info$state)
  
  for (i in 1:nrow(sheet_info)) {
    if (!is.na(sheet_info$link[i])) {
      save_website_pages(sheet_info$link[i], state = sheet_info$state[i])
      
    }
  }
}

save_website_pages <- function(url, state) {
  png_name <- paste0(state, "//", state, "_", lubridate::now(), ".png")
  png_name <- gsub(":", "_", png_name)
  png_name <- paste0(here::here("data/webpages//"), png_name)
  
  pdf_name <- gsub(".png$", ".pdf", png_name)
  html_name <- gsub(".png$", ".html", png_name)
  
  if (grepl(".pdf$", url)) {
    download.file(url, destfile = pdf_name, mode = "wb")
  } else {
    
    
    links <- 
      read_html(url) %>%
      html_nodes("a") %>%
      html_attr('href')
    
    tryCatch({
      webshot::webshot(url,
                       file  = png_name,
                       delay = 1)
    }, error = function(e) {
      print(paste("Load error for PNG in:", state))
    })
    
    Sys.sleep(2)
    tryCatch({
      webshot::webshot(url, 
                       file      = pdf_name,
                       delay     = 1,
                       zoom      = 2)
    }, error = function(e) {
      print(paste("Load error for PDF in:", state))
    })
    Sys.sleep(2)
    
    tryCatch({
      html_object = xml2::read_html(url)
      xml2::write_xml(html_object, file = html_name)
    }, error = function(e) {
      print(paste("Load error for HTML in:", state))
    })
    Sys.sleep(2)
  }
}


files <- list.files()
for (file in files) {
  setwd(file)
  temp <- list.files()
  file.remove(temp)
  setwd("..")
}
