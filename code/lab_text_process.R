im <- image_read("data/raw/Statistique_industrie_minérale_1914-1918_58.tiff")
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

oo <- ocr(im_p, engine = tesseract("fra"))


# hunspell(oo, dict = dictionary("fr_FR"))
# 
# hs <- hunspell_parse(oo, dict = dictionary("fr_FR"))
# 
# hunspell_check(hs, dict = dictionary("fr_FR"))
# 


ines <- strsplit(oo, "\n", fixed = TRUE)[[1]]
ines <- as_tibble(ines)

data <- ines %>%
  mutate(content = gsub("\\.\\s+\\.", "..", value)) %>% 
  mutate(arg = str_split(content, "\\.+"))

data <- data %>% 
  separate_wider_delim(arg, delim = ",", names = "NA")










































