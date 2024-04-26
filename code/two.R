

# Packages ----------------------------------------------------------------
library(tidyverse)
library(tesseract)
library(pdftools)
library(magick)
library(imager)


# import et conversion pdf en png -----------------------------------------
# doc <- pdf_convert("data/raw/Statistique_industrie_min?rale_1914-1918.pdf", format = "png", dpi = 600)
# 
# doc1 <- pdf_convert("data/raw/Statistique_industrie_min?rale_1914-1918.pdf", format = "png", dpi = 300, pages = 56)
# 
# pdf_ocr_text("data/raw/Statistique_industrie_min?rale_1914-1918.pdf", dpi = 900, pages = 56, language = "fra")
# 
# pdf_ocr_text("test3.png", dpi = 600, pages = 56, language = "fra")


im <- image_read("data/Statistique_industrie_minérale_1914-1918_054.jpg")

# Image processing --------------------------------------------------------

# cut imagae in 2
info <- image_info(im)
info_width <- info$width

ima <- im %>%
  image_crop((info_width/2)-30)

imay <- image_convert(ima, "gray")

imay <- image_threshold(imay, "binary", 0.5)

imay <- image_negate(imay)

#imager
mask_size <- c(5, 5)
image_distance <- distance_transform(imay, 5)















