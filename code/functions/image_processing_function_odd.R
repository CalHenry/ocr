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
