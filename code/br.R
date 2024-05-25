
# cut pges in 2 to have even and odd

file_list <- list.files("data/raw", pattern = "\\.tiff$", full.names = TRUE)
tiff_list <- lapply(file_list, image_read)

#even
p5 <- p56 %>%
  image_crop((info$width/2)-1280) %>% 
  image_crop("x80%+0+800")

  

#odd
p56 <- image_read("data/raw/Statistique_industrie_minérale_1914-1918_51.tiff")
info <- image_info(p56)
info_width <- info$width
p5 <- p56 %>%
  image_rotate(-1) %>% 
  image_flop() %>% 
  image_crop((info$width/1.95)) %>% 
  image_flop() %>%
  image_crop(-1450) %>% 
  image_crop()
p5

p5 <- p56 %>%
  image_rotate(-1) %>% 
  image_flop() %>% 
  image_crop((info$width/1.95)) %>% 
  image_flop() %>%
  image_crop("x80%+0+670")
p5

#####
even_file_list <- lapply(tiff_list, function(image) {
  im_c <- image %>%
    image_crop((info$width/2)-1280) %>%
    image_crop("x80%+0+800")
  return(im_c)
})

odd_file_list <- lapply(tiff_list, function(image) {
  im_c <- image %>%
    image_rotate(-1) %>% 
    image_flop() %>% 
    image_crop((info$width/1.95)) %>% 
    image_flop() %>%
    image_crop(-1450)
  return(im_c)
})

p52_list_ocr <- lapply(tiff_list, function(image) {
  p5 <- image %>%
  ocr(engine = tesseract("fra"))
})

# old parts ---------------------------------------------------------------
# crop EVEN
doc1_p56 <- pdf_convert("data/raw/Statistique_industrie_minérale_1914-1918.pdf", format = "tiff", dpi = 400, pages = 56)
p5_list <- lapply(tiff_list, function(image) {
  p5 <- image %>%
    image_crop((info$width/2)-1280) %>%
    image_crop("x80%+0+800")
  return(p5)
})

#crop odd
doc1_p57 <- pdf_convert("data/raw/Statistique_industrie_minérale_1914-1918.pdf", format = "tiff", dpi = 400, pages = 57)

p56 <- image_read(doc1_p56)
p57 <- image_read(doc1_p57)

info <- image_info(p56)

#cropping even images, removal of the last col bc no valuable info in it.
p5 <- p56 %>%
  image_crop((info$width/2)-1280) %>% 
  image_crop("x80%+0+800")

p5 <- p56 %>%
  image_scale("2000x") %>% 
  image_rotate(-1)
p5

p5 <- p56 %>%
  image_scale("2000x") %>%
  image_rotate(-1) %>% 
  image_flop() %>% 
  image_crop(info$width/2) %>% 
  image_flop() %>% 
  image_crop(-1450)
p5

p5 <- p56 %>%
  image_scale(geometry = "2000x") %>%
  image_rotate(-1) %>% 
  image_flop() %>% 
  image_crop(info$width/2) %>% 
  image_flop() %>% 
  image_crop(-1450)
p5



p <- p5 %>% 
  image_convert(colorspace = "gray") %>% 
  image_rotate(-0.3) %>% 
  image_scale(geometry = "2000x") %>% 
  image_threshold("white", threshold = "85%") %>%
  image_threshold("black", threshold = "80%") %>%
  image_median(radius = 4) %>%
  image_blur(radius = 2, sigma = 1.5) %>%
  image_enhance()

h <- ocr(p, engine = tesseract("fra"))
# old parts ---------------------------------------------------------------


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
  image_rotate(-0.3) %>% 
  #image_scale(geometry = "3116x2916") %>% 
  #image_scale(geometry = "2464x2306") %>% 
  image_scale(geometry = "2000x") %>% 
  image_threshold("white", threshold = "85%") %>%
  image_threshold("black", threshold = "80%") %>%
  image_blur(radius = 6, sigma = 0.5) %>%
  image_median(radius = 8) %>%
  image_enhance()
p

h <- ocr(p, engine = tesseract("fra"))
h

#blur afters mooth
p <- p5 %>% 
  image_convert(colorspace = "gray") %>% 
  image_rotate(-0.3) %>% 
  #image_scale(geometry = "3116x2916") %>% 
  #image_scale(geometry = "2464x2306") %>% 
  image_scale(geometry = "2000x") %>% 
  image_threshold("white", threshold = "85%") %>%
  image_threshold("black", threshold = "80%") %>%
  image_median(radius = 4) %>%
  image_blur(radius = 2, sigma = 1.5) %>%
  image_enhance()
p

h <- ocr(p, engine = tesseract("fra"))
h


write.csv(h, "h.txt")

##############

im <- image_read("data/raw/Statistique_industrie_min?rale_1914-1918_58.tiff")
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


hunspell(oo, dict = dictionary("fr_FR"))

hs <- hunspell_parse(oo, dict = dictionary("fr_FR"))

hunspell_check(hs, dict = dictionary("fr_FR"))





write.csv(oo, "oo.txt")


# Example OCR text
text <- "A otre-Dame-de-la-Gorge.....|} Plomb......,....... 400 In. e
4 Revenette-Blanche (La).....} Cnire ............. 400 Ia. ,
A Roche-de-Belmont.........] Mangan?se........... 188 Fe. '"

# Define the regular expression pattern
pattern <- "(.*?)\\.\\.\\.\\.\\.\\|\\} (.*?)\\.\\.\\.\\.\\.\\.\\.\\. (.*?) (.*?)\\."

# Extract the elements from the OCR text
matches <- gregexpr(pattern, text, perl = TRUE)
elements <- regmatches(text, matches)

# Print the extracted elements
for (i in 1:length(elements)) {
  print(elements[[i]])
}



ines <- strsplit(oo, "\n", fixed = TRUE)[[1]]

pattern <- "(.*?)\\+.(.*?)+\\.(.*?)+\\."

# Extract the three parts using str_match()
result <- str_match(ines, pattern)
















# Image processing operations
# im_p <- image %>%
#   image_rotate(-1) %>% 
#   image_flop() %>% 
#   image_crop(info$width/2) %>% 
#   image_flop() %>% 
#   image_crop(-1450) %>% 
#   image_convert(colorspace = "gray") %>% 
#   image_scale(geometry = "2000x") %>% 
#   image_threshold("white", threshold = "85%") %>%
#   image_threshold("black", threshold = "80%") %>%
#   image_median(radius = 4) %>%
#   image_blur(radius = 2, sigma = 1.5) %>%
#   image_enhance()
# }



split_split <- lapply(seq_along(even_ocr_text), function(i) {
  split_text <- unlist(strsplit(even_ocr_text[[i]], "\n"))
  tibble(text = split_text)
})

names(split_split) <- paste0("txt_", 1:length(split_split))

processed_split_datasets <- lapply(split_split, function(dataset) {
  dataset %>%
    mutate(content = str_replace(text, "\\.\\s+\\.", "..")) %>%
    mutate(content = str_replace(content, "^(\\p{L})\\s", "")) %>% 
    mutate(content = str_replace_all(content, "\\[|\\]|\\}|\\{|\\!", "|")) %>%
    mutate(content = str_replace(content, "^\\s*\\|", "")) %>% 
    mutate(content = str_replace(content, "\\|\\|", "|")) %>%
    mutate(content = str_replace(content, "(?<=\\..)1", "|")) %>% 
    mutate(content = str_replace_all(content, "(\\.{2,})\\s*([A-Za-z0-9])", "\\1|\\2")) %>%
    mutate(content = str_trim(content, side = "left")) %>% 
    mutate(content = str_replace(content, "(.)(dem)", "i\\2")) %>% 
    mutate(arg = str_split(content, "(?<!\\d)\\.+(?!\\d)")) %>% 
    mutate(arg = as.character(arg)) %>% 
    mutate(arg = str_replace_all(arg, "\", \"", "")) %>%
    mutate(arg = str_replace_all(arg, "^c\\(\"|\"\\)$", "")) %>% 
    separate_wider_delim(arg, delim = "|", names_sep = "", too_few = "align_start")
})

### nop ###
split_split <- lapply(seq_along(even_ocr_text), function(i) {
  split_text <- unlist(strsplit(even_ocr_text[[i]], "\n"))
  tibble(text = split_text)
})

names(split_split) <- paste0("txt_", 1:length(split_split))


sss_split_split <- lapply(even_ocr_text, split_text_and_process)


processed_split_datasets <- lapply(split_split, function(dataset) {
  dataset %>%
    mutate(content = str_replace(text, "\\.\\s+\\.", "..")) %>%
    mutate(content = str_replace(content, "^(\\p{L})\\s", "")) %>% 
    mutate(content = str_replace_all(content, "\\[|\\]|\\}|\\{|\\!", "|")) %>%
    mutate(content = str_replace(content, "^\\s*\\|", "")) %>% 
    mutate(content = str_replace(content, "\\|\\|", "|")) %>%
    mutate(content = str_replace(content, "(?<=\\..)1", "|")) %>% 
    mutate(content = str_replace_all(content, "(\\.{2,})\\s*([A-Za-z0-9])", "\\1|\\2")) %>%
    mutate(content = str_trim(content, side = "left")) %>% 
    mutate(content = str_replace(content, "(.)(dem)", "i\\2")) %>% 
    mutate(arg = str_split(content, "(?<!\\d)\\.+(?!\\d)")) %>% 
    mutate(arg = as.character(arg)) %>% 
    mutate(arg = str_replace_all(arg, "\", \"", "")) %>%
    mutate(arg = str_replace_all(arg, "^c\\(\"|\"\\)$", "")) %>% 
    separate_wider_delim(arg, delim = "|", names_sep = "", too_few = "align_start")
})




test <- bind_rows(processed_ocr_text)


# Function to extract decimal digits from a string
extract_decimal <- function(string) {
  decimal <- str_extract(string, "(?<=\\s)[0-9]+[.,][0-9]{3}")
  return(decimal)
}

# Apply the decimal extraction function to each "arg.." variable in the merged dataset
for (i in 1:6) {
  merged_dataset <- test %>%
    mutate(!!paste0("arg", i+1) := extract_decimal(!!sym(paste0("arg", i))))
}

# Rearrange the "arg.." variables
for (i in length(merged_dataset):3) {
  merged_dataset <- merged_dataset %>%
    mutate(!!paste0("arg", i) := !!sym(paste0("arg", i-1)))
}

# Set the value of "arg2" based on the extracted decimal
merged_dataset <- merged_dataset %>%
  mutate(arg2 = extract_decimal(arg1))











test2 <- test %>% 
  mutate_at(vars(starts_with("arg")), ~str_split(., pattern = "(?=In)", simplify = TRUE))

test2 <- test %>% 
  mutate_at(vars(starts_with("arg")), ~str_split(., pattern = "(?<=\\s)[0-9]+[.,][0-9]{3}", simplify = T))
test2 <- test %>% 
  mutate_at(vars(starts_with("arg")), ~str_split(., pattern = "(?<=\\s)[0-9]+[.,][0-9]{3}", simplify = T))






string <- "167 50 13,253 77"
str_extract(string, "(?<=\\s)[0-9]+[.,][0-9]{3}")

str_extract(string, "(?<=\\s)[0-9]+[.,][0-9]{3}")

df <- data.frame(
  col1 = c("This is idem a test", "Another example", "idem and more", "No idem here"),
  col2 = c("blablablal", "blebebla", "12", "12436464e"),
  stringsAsFactors = FALSE
)

# Split the string based on "idem"
dff <- df %>%
  mutate(col1 = str_split(col1, pattern = "(?<=idem)", simplify = TRUE))



string <- c("Ã‚|hello", "9}world", "b[test", "2||this is a test")

str_replace_all(string, "^[:graph:][\\/\\|\\}\\{\\[\\]]{1,}", "")
string <- c("VOSGES.TEST")

if_else((str_count(string, "[:punct:]+|[:blank:]|\\|+|[^.]") / str_length(string)) > 0.2, NA, string)
if_else((str_count(string, "[:punct:]|\\|+") / str_length(string)) > 0.2, NA, string)

str_replace(string, "^[^[a-zA-Z]:alnum:]*", "")

str_count(string, "^[:punct:]|\\|+|[^.]$") / str_length(string)
str_count(string, "^[[:punct:]]|\\|$") / str_length(string)
str_locate_all(string, "[[:punct:]&&[^.]]|[:blank:]|\\|")
str_count(string, "[[:punct:]&&[^.]]|[:blank:]|\\|") / str_length(string)

string <- c("VOSGES.TEST")
string <- c("VOSGES.test")

str_locate_all(string, "([:upper:]|[:upper:]&[:punct:]&[:upper:])")


#sert Ã  rien : tenta de faire une var "région"
# www <- test %>%
#   filter(str_detect(arg1, "(?:(?<!^)[A-Z]\\b|(?<!^[A-Z[:punct:]]*)\b[A-Z[:punct:]]+\\b(?![A-Z[:punct:]]*$))"))
# www <- test %>%
#   filter(str_detect(arg1, "^\\b[A-Z[:punct:]]*\\b$"))
# 
# 
# ,
# str_detect(arg1, "[[:upper:]]"))
# 
# www <- test %>%
#   mutate(words = str_extract_all(arg1, "[[:upper:]]+[-[:upper:]]*")) %>%
#   unnest(words) %>%
#   filter(str_detect(words, "^\\b[A-Z[:punct:]]*\\b$"))
# 
# www <- test %>%
#   mutate(words = str_extract_all(arg1, "([:upper:]|[:upper:]&[:punct:]+&[:upper:])"))
# 
# filter(str_detect(words, "([:upper:]|[:upper:]&[:punct:]&[:upper:])"))

d <- tes %>%
  filter(arg1,
         rowSums(sapply(function(col) str_detect(tes[[col]], "\\b\\w{11,}\\b"))) > 0)


d <- tes %>%
  mutate(has_long_word = as.integer(rowSums(sapply(c("arg1"), function(col) str_detect(tes[[col]], "\\b\\w{13,}\\b"))) > 0),
         has_long_number = as.integer(rowSums(sapply(c("arg1"), function(col) str_detect(tes[[col]], "\\b\\d{6,}\\b"))) > 0))


# spelling tries

#list_dictionaries()

tes %>%
  select(arg1) %>% 
  hunspell(dict = dictionary("fr_FR"))


h <- tes$arg2

d <- hunspell(h, dict = dictionary("fr_FR"))

d <- hunspell_suggest(h, dict = dictionary("fr_FR"))





































