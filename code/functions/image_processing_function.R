# Image processing function
process_image <- function(file) {
  # Read and get info
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
  
 # Save the processed image to a list
  # destination_file <- file.path(destination_folder, basename(file))
  # image_write(im_p, destination_file)
  return(p)

}

#' Details of each line:
#' 1. crop: cut the image in 2, we keep the left part (even) and cut some vertically on the right side to only have relevant cols.
#' 2. crop: ("x80%+0+800") take 80% of the width of the image, 0 from the left and 800 from the top. 
#' 3. turn the image to grayscale
#' 4. Rotate the even page sa tiny bit bc the top left is further than the bottom left
#' 5. Scale the image to 2000x, 2000 width, we keep aspect ratio (by not telling the height)
#' 6. Threshold white 85%: all pixels more than 15% white are turn to full white pixels.
#' 7. same for black, 20% of black are turned to full black pixels.
#' 8. image_median is a smoothing function, 
#' the algorithm replaces each pixel with the median color in a circular neighborhood.
#' It "rounds" the letters a bit.
#' 9. Blur is used for th esame purposed, it limits the sharp edges of the tiny letters, 
#' making them easier to read by ocr.
#' 10. image_enhance is supposed to minimize noise (random odd color pixels)
