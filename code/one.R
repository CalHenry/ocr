#------------------------------------------------------------------------------#
# Packages ----------------------------------------------------------------

library(tesseract)
library(pdftools)
library(magick)


# Liste des choses ? faire : ----------------------------------------------
#
# Couper les scans en 2 (2 pages dans un scan)



# import et conversion pdf en png -----------------------------------------
doc <- pdf_convert("data/raw/Statistique_industrie_min?rale_1914-1918.pdf", format = "png", dpi = 600)

doc1 <- pdf_convert("data/raw/Statistique_industrie_min?rale_1914-1918.pdf", format = "png", dpi = 300, pages = 56)

pdf_ocr_text("data/raw/Statistique_industrie_min?rale_1914-1918.pdf", dpi = 900, pages = 56, language = "fra")

pdf_ocr_text("test3.png", dpi = 600, pages = 56, language = "fra")

## partie test tesserac
# tesseract_download("fra")
fra <- tesseract("fra")
nodots <- tesseract(language = "fra", options = list(tessedit_char_blacklist = "."))

text <- ocr("bo.png", engine = fra)

im <- image_read("bo.png")
im <- image_read("data/raw/Statistique_industrie_min?rale_1914-1918_56.png", pages = 56)


ima <- im %>%
  image_crop(605.5) %>%
  ocr() %>%
  cat()

text <- ocr("data/raw/Statistique_industrie_min?rale_1914-1918_56.png", pages = 56, engine = nodots)



text <- ocr("data/png/Statistique_industrie_min?rale_1914-1918_56.png", engine = fra)
cat(text)
text <- ocr("44.png", engine = fra)
cat(text)

####
imaa <- im %>%
  image_convert("gray")



ocr(im, engine = fra) %>%
  cat()
##
im <- image_read("data/bo_56 - Copie.png")
im <- image_read("data/Statistique_industrie_minérale_1914-1918_054.jpg")

# Image processing --------------------------------------------------------

# cut imagae in 2
info <- image_info(im)
info_width <- info$width

ima <- im %>%
  image_crop((info_width/2)-30)

# brightness
adjusted_image <- image_contrast(im, sharpen = 1000)

image_write(adjusted_image, "adjusted_image.png")

#resize
resized <- image_resize(im, "6000x") %>%
  ocr()

image_ocr(resized, engine = fra)

########## mso try

gray_img <- image_convert(im, "gray")
ima <- gray_img


edge_img <- image_canny(ima)

hough <- image_hough_draw(edge_img, color = "navyblue", geometry = '1x5')
hough

trim <- ima %>%
  image_scale("x800") %>%
  image_background("white", flatten = TRUE) %>%
  #image_noise() %>%
  image_enhance() %>%
  image_normalize() %>%
  image_contrast(sharpen = 1)
trim

text <- ocr(trim, "fra") 
text






trim <- ima %>% 
  image_modulate(brightness = 120)
trim  
trim <- ima %>%
  image_scale("x1000") %>%
  image_negate() %>%
  image_threshold("white", threshold = "20%") %>%
  image_morphology(method = "Erode", kernel = "Unity")
trim  

trim <- ima %>%
  image_scale("x1000") %>%
  image_negate() %>%
  image_threshold("white", threshold = "25%")


  image_morphology(method = "Dilate") %>%
  ocr()
trim  


trim <- ocr(trim)

man <- demo_image('man.gif')

