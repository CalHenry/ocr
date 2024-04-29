



imay <- image_negate(imay)



imat <- imay %>% 
  image_threshold("black", threshold = "70%") %>% 
  image_threshold("white", threshold = "40%")



#blanc
imat <- imay %>% 
  image_threshold("white", threshold = "50%")
imat

#noir
imat <- imay %>% 
  image_threshold("black", threshold = "80%")
imat


imat <- imay %>% 
  image_threshold("white", threshold = "80%") %>% 
  image_threshold("black", threshold = "80%")
imat








#ocr
ocr(imat, engine = "fra")



copy <- image_read("data/copy.png")
ocr(copy, engine = "fra")

cop <- copy %>% 
  image_threshold("black", threshold = "75%")
cop

copy2 <- image_read("data/copy2.png")
ocr(copy2, engine = "fra")

####try chatgpt answer

library(magick)

# Read the image
image <- image_read("path/to/your/image.png")

smoothed_image <- im %>%
  image_blur(sigma = 1)

resized_image <- smoothed_image %>%
  image_resize("600x400", "sinc")

despeckled_image <- smoothed_image %>%
  image_despeckle()

denoised_image <- despeckled_image %>%
  image_reducenoise()

ocr(denoised_image, engine = "fra")





coco <- image_read("data/coco.png")
ocr(coco, engine = "fra")


g <- image_enhance(im)



bo_bo <- image_read("data/bo.png")
image_info(bo_bo)

bobo <- bo_bo %>% 
  image_convert(colorspace = "gray") %>% 
  image_rotate(-0.3) %>% 
  #image_scale(geometry = "3116x2916") %>% 
  image_threshold("white", threshold = "85%") %>% 
  image_threshold("black", threshold = "60%") %>% 
  image_blur(radius = 3, sigma = 0.5) %>% 
  image_reducenoise(radius = 2) 
bobo  


ocr(bobo, engine = "fra")


#2
bobo <- bo_bo %>% 
  image_convert(colorspace = "gray") %>% 
  image_rotate(-0.3) %>% 
  #image_scale(geometry = "3116x2916") %>% 
  image_threshold("white", threshold = "85%") %>% 
  image_threshold("black", threshold = "40%") %>% 
  image_blur(radius = 3, sigma = 0.5) %>% 
  image_median(radius = 3) %>% 
  image_enhance() 
bobo  


SAH <- ocr(bobo, engine = tesseract("nodots"))


write.csv(SAH, "sah.txt")


dc <- p5 %>%
  image_crop((info$width/2)-30) %>% 
  image_crop("x80%+0+600")
dc

info$height

h_png <- read.csv("h_png.txt")
h_tiff <- read.csv("h_tiff.txt")

diffChr(h_png, h_tiff)

#######################
p <- p5 %>% 
  image_convert(colorspace = "gray") %>% 
  #image_rotate(-0.3) %>% 
  #image_scale(geometry = "3116x2916") %>% 
  #image_scale(geometry = "2464x2306") %>% 
  image_scale(geometry = "2400x")
  # image_threshold("white", threshold = "85%") %>% 
  # image_threshold("black", threshold = "60%") %>% 
  # image_blur(radius = 3, sigma = 1) %>% 
  # image_median(radius = 4) %>% 
  # image_enhance() 
p

h <- ocr(p, engine = tesseract("fra"))
h


p <- p5 %>% 
  image_convert(colorspace = "gray") %>% 
  #image_rotate(-0.3) %>% 
  #image_scale(geometry = "3116x2916") %>% 
  #image_scale(geometry = "2464x2306") %>% 
  image_scale(geometry = "6000x") %>% 
  image_threshold("white", threshold = "85%") %>%
  image_threshold("black", threshold = "80%") %>%
  image_blur(radius = 3, sigma = 0.5) %>%
  image_median(radius = 4) %>%
  image_enhance()
p

h <- ocr(p, engine = tesseract("fra"))
h






