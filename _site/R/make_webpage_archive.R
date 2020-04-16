source(here::here("R", "utils.R"))

get_all_current_website_pages()

get_all_current_website_pages <- function() {
  sheet_info <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1CwD8aie_ib1wj3FtqACK3N2xssT0W_vX3d_WkKGpdOw/")
  sheet_info$state <- tolower(sheet_info$state)
  sheet_info$state <- gsub(" ", "_", sheet_info$state)
  
  for (i in 1:nrow(sheet_info)) {
    if (!is.na(sheet_info$link[i])) {
      save_website_pages(url = sheet_info$link[i], state = sheet_info$state[i])
    }
    print(paste0(sheet_info$state[i], " completed!"))
  }
}


download_site_pdf <- function(directory, url, link) {
  temp <- html_session(url) %>%
    jump_to(URLencode(link, reserved = FALSE))
  download.file(temp$url, 
                destfile = paste0(directory, 
                                  gsub(".*/|%20", "", temp$url)),
                mode = "wb",
                quiet = TRUE)
}

save_website_pages <- function(url, state) {
  
  directory <- paste0(here::here("data/webpages//"), state, "/", 
                      lubridate::today(), "/")
  
  dir.create(directory)
  
  png_name  <- paste0("webpage_archive_", state, "_", lubridate::today(), ".png")
  png_name  <- gsub(":", "_", png_name)
  png_name  <- paste0(directory, png_name)
  pdf_name  <- gsub(".png$", ".pdf", png_name)
  html_name <- gsub(".png$", ".html", png_name)
  
  if (grepl(".pdf$", url)) {
    download.file(url, destfile = pdf_name, mode = "wb")
  } else {
    
    links <- tryCatch({
        read_html(url) %>%
        html_nodes("a") %>%
        html_attr('href')
    },
    error = function(e){
      links <- NULL
      print(paste0("Links failure in ", state))
    })
    

    
    if (length(links) > 0) {
      links <- grep(".pdf$", links, value = TRUE)
      if (length(links) > 0) {
        for (link in links) {
          
          result <- ""
          attempt <- 1
          while(attempt == 1 | (is.null(result) && attempt <= 5)) {
            if (attempt > 1) {
              Sys.sleep(5)        
            }
            attempt <- attempt + 1
            result <- tryCatch({
              download_site_pdf(directory, url, link)
            },
            error = function(e){
              result <- NULL
            })
            if (is.null(result) & attempt == 6) {
              print(paste("Could not download PDF:", 
                          gsub(".*/|%20", "", link),  "in:", state))
            }
          }
        }
      }
    }
    
    # Save page as PNG
    result <- ""
    attempt <- 1
    while(attempt == 1 | (is.null(result) && attempt <= 5)) {
      if (attempt > 1) {
        Sys.sleep(5)        
      }
      attempt <- attempt + 1
      result <- tryCatch({
        webshot::webshot(url,
                         file  = png_name,
                         delay = 7)
      },
      error = function(e){
        result <- NULL
      })
      if (is.null(result) && attempt == 6) {
        print(paste("Load error for PNG in:", state))
      }
    }
    
    
    # Save page as PDF
    result <- ""
    attempt <- 1
    while(attempt == 1 | (is.null(result) && attempt <= 5)) {
      if (attempt > 1) {
        Sys.sleep(5)        
      }
      attempt <- attempt + 1
      result <- tryCatch({
        webshot::webshot(url,
                         file  = pdf_name,
                         delay = 7)
      },
      error = function(e){
        result <- NULL
      })
      if (is.null(result) && attempt == 6) {
        print(paste("Load error for PDF in:", state))
      }
    }
    
    # Save page as HTML
    result <- ""
    attempt <- 1
    while(attempt == 1 | (!is.null(result) && attempt <= 5)) {
      if (attempt > 1) {
        Sys.sleep(5)        
      }
      attempt <- attempt + 1
      result <- tryCatch({
        html_object <- xml2::read_html(url)
        xml2::write_html(html_object, file = html_name)
      },
      error = function(e){
        result <- 'ERROR'
      })
      if (!is.null(result) && attempt == 6) {
        print(paste("Load error for HTML in:", state))
      }
    }
    
  }
}


# files <- list.files()
# for (file in files) {
#   setwd(file)
#   temp <- list.files()
#   file.remove(temp)
#   setwd("..")
# }
