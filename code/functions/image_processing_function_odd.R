# Custom Image processing function for odd pages
process_image_odd <- function(image) {
  info <- image_info(image)
  
  # Image processing operations
  im_p <- image %>%
    image_convert(colorspace = "gray") %>% 
    image_scale(geometry = "2000x") %>% 
    image_threshold("white", threshold = "75%") %>%
    image_threshold("black", threshold = "40%") %>%
    image_median(radius = 4) %>%
    image_blur(radius = 2, sigma = 1.5) %>%
    image_enhance()
}

#' Details of each line:
#' 1. turn the image to grayscale
#' 2. Scale the image to 2000x, 2000 width, we keep aspect ratio (by not telling the height)
#' 3. Threshold white 75%: all pixels more than 25% white are turn to full white pixels.
#' 4. same for black, 20% of black are turned to full black pixels.
#' 5. image_median is a smoothing function, 
#' the algorithm replaces each pixel with the median color in a circular neighborhood.
#' It "rounds" the letters a bit.
#' 6. Blur is used for th esame purposed, it limits the sharp edges of the tiny letters, 
#' making them easier to read by ocr.
#' 7. image_enhance is supposed to minimize noise (random odd color pixels)
