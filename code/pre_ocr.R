

# Packages ----------------------------------------------------------------
library(tidyverse)
library(tesseract)
library(pdftools)

#' Import pdf files to png
#' prep the images to be processed for ocr. 
#' 2 parts :
#' - Cropping pages: focus on the tables at the center of the iamge.
#'    even and odd pages are not treated the same way.
#' - Image processing to clean the scans so we can hve the best out of OCR



doc1_p56 <- pdf_convert("data/raw/Statistique_industrie_minérale_1914-1918.pdf", format = "tiff", dpi = 400, pages = 56)
#doc1_p56 <- pdf_convert("data/raw/Statistique_industrie_minérale_1914-1918.pdf", format = "png", dpi = 400, pages = 56)


# crop EVEN
p56 <- image_read(doc1_p56)
info <- image_info(p56)

#cropping even images, removal of the last col bc no valuable info in it.
p5 <- p56 %>%
  image_crop((info$width/2)-1280) %>% 
  image_crop("x80%+0+800")

p <- p5 %>% 
  image_convert(colorspace = "gray") %>% 
  #image_rotate(-0.3) %>% 
  #image_scale(geometry = "3116x2916") %>% 
  #image_scale(geometry = "2464x2306") %>% 
  image_scale(geometry = "6000x") %>% 
  image_threshold("white", threshold = "85%") %>%
  image_threshold("black", threshold = "30%") %>%
  image_blur(radius = 3, sigma = 0.5) %>%
  image_median(radius = 4) %>%
  image_enhance()
p

h <- ocr(p, engine = tesseract("fra"))
h
























































