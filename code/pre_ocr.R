source("code/functions/image_processing_function.R")

# Packages ----------------------------------------------------------------
library(tidyverse)
library(tesseract)
library(pdftools)
library(magick)

#' Import pdf files to png
#' prep the images to be processed for ocr. 
#' 2 parts :
#' - Cropping pages: focus on the tables at the center of the iamge.
#'    even and odd pages are not treated the same way.
#' - Image processing to clean the scans so we can hve the best out of OCR



doc1_p56 <- pdf_convert("data/raw/Statistique_industrie_minérale_1914-1918.pdf", format = "tiff", dpi = 400, pages = 56)
doc1_p56 <- pdf_convert("data/raw/Statistique_industrie_minérale_1914-1918.pdf", format = "tiff", dpi = 400, pages = 56:58)


tiff_file_list <- list.files(pattern = "\\.tiff$", full.names = TRUE)

destination_folder <- "data/raw"
for (file in tiff_file_list) {
  new_file <- file.path(destination_folder, basename(file))
  file.rename(file, new_file)
}


# crop EVEN
p56 <- image_read(doc1_p56)
info <- image_info(p56)

#cropping even images, removal of the last col bc no valuable info in it.
p5 <- p56 %>%
  image_crop((info$width/2)-1280) %>% 
  image_crop("x80%+0+800")


p <- p5 %>% 
  image_convert(colorspace = "gray") %>% 
  image_rotate(-0.3) %>% 
  image_scale(geometry = "2000x") %>% 
  image_threshold("white", threshold = "85%") %>%
  image_threshold("black", threshold = "80%") %>%
  image_median(radius = 4) %>%
  image_blur(radius = 2, sigma = 1.5) %>%
  image_enhance()

h <- ocr(p, engine = tesseract("fra"))


# Get a list of all TIFF files in the source folder
file_list <- list.files("data/raw", pattern = "\\.tiff$", full.names = TRUE)

# Apply the image processing function to all TIFF files using lapply
destination_folder <- "data/int"
test_lap <- lapply(file_list, process_image)

test_ocr <- lapply(test_lap, ocr) 



###---###
s
# Get a list of all TIFF files in the source folder
file_list <- list.files("data/raw", pattern = "\\.tiff$", full.names = TRUE)
processed_images <- list()

# Process each TIFF file
for (file in file_list) {
  # Read the image
  im <- image_read(file)
  info <- image_info(im)
  
  # Apply image processing operations
  im_p <- im %>%
    image_crop((info$width/2)-1280) %>% 
    image_crop("x80%+0+800") %>%
    image_convert(colorspace = "gray") %>% 
    image_rotate(-0.3) %>% 
    image_scale(geometry = "2000x") %>% 
    image_threshold("white", threshold = "85%") %>%
    image_threshold("black", threshold = "80%") %>%
    image_median(radius = 4) %>%
    image_blur(radius = 2, sigma = 1.5) %>%
    image_enhance()
  
  # Save the processed image to the destination folder
  processed_images <- c(processed_images, list(im_p))
}

ocr(processed_images[1], engine=tesseract("fra"))

a <- processed_images[2]







































