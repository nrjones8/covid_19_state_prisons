
# Vermont -----------------------------------------------------------------

# z <- magick::image_read_pdf("test.pdf",
#                             pages = 2)
# image_info(z)
# 
# 
# library(tesseract)
# eng <- tesseract("eng")
# z <- tesseract::ocr("test.pdf", engine = eng)
# z <- z[2]
# temp <- z
# z <- gsub(".*Inmate COVID Tracker as of ", "", z)
# z <- gsub("NOTE: DAILY Counts.*", "", z)
# 
# z <- strsplit(z, "\n")
# z <- z[[1]]
# z <- z[grep("^#", z)]
# z <- data.frame(category = z)
# z$daily <- gsub(".* ([0-9].*)", "\\1",  z$category)
# z$total <- gsub(".* \\S ", "", z$daily)
# z$daily <- gsub(" \\S .*", "", z$daily)
# z$category <- gsub(" [0-9].*", "",  z$category)
# 
# z$daily <- parse_number(z$daily)
# z$total <- parse_number(z$total)
