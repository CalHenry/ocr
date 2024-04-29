

# Packages ----------------------------------------------------------------
library(tidyverse)
library(tesseract)
library(pdftools)
library(magick)
#library(imager)


# import et conversion pdf en png -----------------------------------------
# doc <- pdf_convert("data/raw/Statistique_industrie_min?rale_1914-1918.pdf", format = "png", dpi = 600)
# 
doc1 <- pdf_convert("data/raw/Statistique_industrie_min?rale_1914-1918.pdf", format = "png", dpi = 400, pages = 56)
# 
# pdf_ocr_text("data/raw/Statistique_industrie_min?rale_1914-1918.pdf", dpi = 900, pages = 56, language = "fra")
# 
# pdf_ocr_text("test3.png", dpi = 600, pages = 56, language = "fra")


#im <- image_read("data/Statistique_industrie_minérale_1914-1918_054.jpg")
im <- image_read("data/bo.png")

# Image processing --------------------------------------------------------

# cut imagae in 2
info <- image_info(im)
info_width <- info$width

ima <- im %>%
  image_crop((info_width/2)-30)

p <- image_read(doc1)
info <- image_info(p)
p5 <- p %>%
  image_crop((info$width/2)-30) %>% 
  image_crop("x80%+0+600")

p56 <- p5 %>% 
  image_convert(colorspace = "gray") %>% 
  #image_rotate(-0.3) %>% 
  #image_scale(geometry = "3116x2916") %>% 
  image_scale(geometry = "2464x2306") %>% 
  #image_scale(geometry = "1583x1477") %>% 
  image_threshold("white", threshold = "85%") %>% 
  image_threshold("black", threshold = "50%") %>% 
  image_blur(radius = 3, sigma = 0.5) %>% 
  image_median(radius = 4) %>% 
  image_enhance() 
p56  


ocr(p56, engine = tesseract("fra"))

image_info(bobo)
image_info(p56)

image_write(p5, "data/int/p5.png")


p56 <- p5 %>% 
  image_convert(colorspace = "gray") %>% 
  image_scale(geometry = "2464x2306")
p56





