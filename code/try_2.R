###

library(imager)

ima <- image_read("data/raw/Statistique_industrie_minérale_1914-1918_34.tiff")


imag <- ima %>% 
  image_convert(colorspace = "gray") %>% 
  hist()


a <- load.image("data/int/p5.png")

grayscale(a) %>% hist()
