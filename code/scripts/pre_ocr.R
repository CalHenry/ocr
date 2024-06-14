source("code/functions/image_processing_function.R")

# Packages ----------------------------------------------------------------
library(tidyverse)
library(tesseract)
library(pdftools)
library(magick)

#' Import pdf files to png
#' prep the images to be processed for ocr. 
#' 2 parts :
#' - Cropping pages: focus on the tables at the center of the image.
#'    even and odd pages are not treated the same way.
#' - Image processing to clean the scans so we can hve the best out of OCR



doc1_p56 <- pdf_convert("data/raw/Statistique_industrie_minérale_1914-1918.pdf", format = "tiff", dpi = 400, pages = 56)
doc1_p56 <- pdf_convert("data/raw/Statistique_industrie_minérale_1914-1918.pdf", format = "tiff", dpi = 400, pages = 56:59)

# move the generated tiff image to the data/raw folder
tiff_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
destination_folder <- "data/raw"
for (file in tiff_files) {
  new_file <- file.path(destination_folder, basename(file))
  file.rename(file, new_file)
}

###   -----
tiff_file_list <- list.files("data/raw", pattern = "\\.tiff$", full.names = TRUE)
tiff_list <- lapply(tiff_file_list, image_read)

#crop and make even and odd lists
even_pages_list <- lapply(tiff_list, function(image) {
  im_c <- image %>%
    image_crop((info$width/2)-1280) %>%
    image_crop("x80%+0+800")
  return(im_c)
})

odd_pages_list <- lapply(tiff_list, function(image) {
  im_c <- image %>%
    image_rotate(-1) %>% 
    image_flop() %>% 
    image_crop((info$width/1.95)) %>% 
    image_flop() %>%
    image_crop(-1450)
  return(im_c)
})



#' The following lines import select, import, process and apply ocr to the tiff images.
#' lapply function is used to apply the same treatment to all elements of a list:
#' first, 'image_read' to read all elements of the files list
#' second, 'process_image' is a custom function that wraps up all the modification given to the images
#' third, 'ocr' is used on the proccesed images to retreive their content in text

# even_file_list <- list.files("data/raw", pattern = "\\d*[02468]\\.tiff$", full.names = TRUE)
# odd_file_list <- list.files("data/raw", pattern = "\\d*[13579]\\.tiff$", full.names = TRUE)

# even_tiff_list <- lapply(even_file_list, image_read)
# odd_tiff_list <- lapply(odd_file_list, image_read)

# Image processing
after_process <- lapply(even_file_list, process_image_even)

odd_after_process <- lapply(odd_pages_list, process_image_odd)


even_ocr_text <- lapply(after_process, ocr, engine = tesseract("fra")) 

odd_ocr_text <- lapply(odd_after_process, ocr, engine = tesseract("fra")) 









































