source("code/functions/image_processing_function.R")

# Packages ----------------------------------------------------------------
# library(tidyverse)
# library(tesseract)
# library(pdftools)
# library(magick)

#' Import pdf files to tiff
#' tiff format bc it might be better for tessereact but no real clue on that.
#' tiff is also heavier that png (to take into acount if your computer has low RAM).


# Single PDF file to several tiff images.  --------------------------------
doc1_p56 <- pdf_convert("data/raw/Statistique_industrie_minérale_1914-1918.pdf", format = "tiff", dpi = 400, pages = 56)
doc1_p56 <- pdf_convert("data/raw/Statistique_industrie_minérale_1914-1918.pdf", format = "tiff", dpi = 400, pages = 56:59)

# move the generated tiff images to the data/raw folder
tiff_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
destination_folder <- "data/raw"
for (file in tiff_files) {
  new_file <- file.path(destination_folder, basename(file))
  file.rename(file, new_file)
}

###  prepare even and odd lists -------------------------------------------
tiff_file_list <- list.files("data/raw", pattern = "\\.tiff$", full.names = TRUE)
tiff_list <- lapply(tiff_file_list, image_read)

#' Details of the below image transformations:
#' rotate the image to slitghly to the left
#' Even:crop the right part + more of the right to remove unnecessary cols
#' crop "x80%+0+800", 80% of the height and remove 800 pixel vertically (0 horizontally)
#' Odd: rotate by 1 degree to the left
#' flop reverse the image left/ right, so the right page is now on the left
#' crop: cut the page in half but bc we divide by 1.95 we keep more than half.
#' this is to leave blank space for a better tesseract later
#' we flop the image again to have it the right way
#' now we only have 1 page,
#' we cut 1450 pixels to the right as we don't need the informations.

# list of even pages
even_pages_list <- lapply(tiff_list, function(image) {
  im_c <- image %>%
    image_rotate(-0.3) %>% 
    image_crop((info$width/2)-1280) %>%
    image_crop("x80%+0+800")
  return(im_c)
})

# list fo odd pages
odd_pages_list <- lapply(tiff_list, function(image) {
  im_c <- image %>%
    image_rotate(-1) %>% 
    image_flop() %>% 
    image_crop((info$width/1.95)) %>% 
    image_flop() %>%
    image_crop(-1450)
  return(im_c)
})



# Image processing --------------------------------------------------------

#' The following lines import select, import, process and apply ocr to the tiff images.
#' lapply function is used to apply the same treatment to all elements of a list:
#' first, 'image_read' to read all elements of the files list
#' second, 'process_image' is a custom function that wraps up all the modification given to the images
#' third, 'ocr' is used on the proccesed images to retreive their content in text

even_after_process <- lapply(even_pages_list, process_image_even)

odd_after_process <- lapply(odd_pages_list, process_image_odd)


# OCR ---------------------------------------------------------------------

even_ocr_text <- lapply(even_after_process, ocr, engine = tesseract("fra")) 

odd_ocr_text <- lapply(odd_after_process, ocr, engine = tesseract("fra")) 


ocr_text <- c(even_ocr_text, odd_ocr_text)






































