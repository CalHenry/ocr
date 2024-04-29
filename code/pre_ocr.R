

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



doc1_p56 <- pdf_convert("data/raw/Statistique_industrie_minérale_1914-1918.pdf", format = "tiff", dpi = 400, pages = 56:58)


# crop EVEN
p56 <- image_read(doc1_p56)
info <- image_info(p56)

#cropping even images, removal of the last col bc no valuable info in it.
p5 <- p56 %>%
  image_crop((info$width/2)-870) %>% 
  image_crop("x80%+0+800")


d <- image_canny(p5)

g <- image_hough_draw(d, bg = "transparent", overlay = T, size = 20)
f <- image_hough_txt(g)

write.csv(f, "f.txt")
-





















































