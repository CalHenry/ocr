im <- image_read("data/raw/Statistique_industrie_minérale_1914-1918_38.tiff")
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

#turn result of ocr to dataset
ines <- as_tibble(strsplit(oo, "\n", fixed = TRUE)[[1]])

# data transform and regex
data <- ines %>%
  mutate(content = str_replace(value, "\\.\\s+\\.", "..")) %>%
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

# Memo ------
 #  mutate(content = str_replace(value, "\\.\\s+\\.", "..")) %>%:
 # remove whitespaces in between sequences of periods ".. ... -> ....."

 #  mutate(content = str_replace(content, "^(\\p{L})\\s", "")) %>%
 #   Remove alone leading letter followed by whitespace at beg of strings.

 #  mutate(content = str_replace_all(content, "\\[|\\]|\\}|\\{|\\!", "|")) %>%:
 #   Replace what is supposed to be "|" with "|".

 #  mutate(content = str_replace(content, "^\\s*\\|", "")) %>%:
 #   Remove leading "|".

 #  mutate(content = str_replace(content, "\\|\\|", "|")) %>%:
 #   Replace "||" with "|".

 #  mutate(content = str_replace(content, "(?<=\\..)1", "|")) %>%:
 #   Replace the "1" that have consecutive dots ahead with "|".
 # The ?<=\\.. is a positive lookbehind assertion that checks if there is a period before the "1".

 #  mutate(content = str_replace_all(content, "(\\.{2,})\\s*([A-Za-z0-9])", "\\1|\\2")) %>%
 #   Replace consecutive dots followed by alphanumeric character with "|".
 #   iow: We add "|" if it is not

 #  mutate(content = str_trim(content, side = "left")) %>%
 #   Trim leading whitespace.

 #  mutate(content = str_replace(content, "(.)(dem)", "i\\2")) %>%
 #   Replace "dem" preceded by any character with "idem".

 #  mutate(arg = str_split(content, "(?<!\\d)\\.+(?!\\d)")) %>%
 #   Split `content` column by non-digit surrounded by dots.

 #  mutate(arg = as.character(arg)) %>%
 #   Convert `arg` column to character.

 #  mutate(arg = str_replace_all(arg, "\", \"", "")) %>%
 #   Remove "\", and "\", " from `arg` column.

 #  mutate(arg = lapply(arg, function(x) str_replace_all(x, "^c\\(\"|\"\\)$", ""))) %>%
 #   Remove leading "c(" and trailing ")" from `arg` column.

 #  separate_wider_delim(arg, delim = "|", names_sep = "", too_few = "align_start")
 # Separate `arg` column into multiple columns using "|" as delimiter.


# mutate(content = gsub("\\.\\s+\\.", "..", value)) %>%:
#   replace occurrences of a period followed by one or more whitespace characters and another period with just two periods.
# Trad: remove whitespcaes in between sequences of periods ".. ... -> ....."

# mutate(content = gsub("\\[|\\]|\\}|\\{|\\!", "|", content)) %>%:
#   replaces occurrences of square brackets, curly brackets, and exclamation marks with a single pipe character.
# Trad: "reconstruct the cols of the talbe", changes all punct that are supposed to be pipe to pipe.

# mutate(content = str_replace(content, "^\\s*\\|", "")) %>%:
#   Remove "|" if it is at the beginning of the string, ignoring white spaces.

# mutate(content = gsub("\\|\\|", "|", content)) %>%:
#   replace occurrences of double pipe characters with a single pipe character in the content variable.

# mutate(content = gsub("(?<=\\..)1", "|", content, perl = TRUE)) %>%:
#   replace occurrences of the digit "1" that are preceded by a period with a pipe character.
# The ?<=\\.. is a positive lookbehind assertion that checks if there is a period before the "1".
# Trad: same as before but for the "1", treatment is different because 1 are also present in numbers so we use the lookbehind.

# mutate(content = str_replace_all(content, "(\\.{2,})\\s*([A-Za-z0-9])", "\\1|\\2")) %>%:
#   after 2 ore more dots, ignoring whitespaces, if there is an alphanumeric, insert a between the dots and the alphanumeric

# mutate(arg = str_split(content, "(?<!\\d)\\.+(?!\\d)")) %>%:
#   split the content variable into multiple parts based on the regex pattern "(?<!\d)\.+(?!\d)".
# This pattern matches sequences of characters between periods that are not preceded or followed by digits.
# Trad: split the content that is between periods
# ex: "Leschienx.... ss... Plomb.............. 339 In" -> "c("Leschienx", " ss", " Plomb", " 339 In", " â€œ")"

# mutate(arg = as.character(arg)) %>%: convert the arg variable to character type.
# 
# mutate(arg = lapply(arg, function(x) gsub("^c\\(|\\)$", "", x))) %>%:
#   remove the leading "c(" and trailing ")" characters from each element in the arg variable.
# Trad: because we turn a list to char, we keep the c(...) from the list, we remove it here.

# separate_wider_delim(arg, delim = "|", names_sep = "", too_few = "align_start"):
#   split the arg variable into separate columns by the delimiter "|".
# ----


#last one
# data <- ines %>%
#   mutate(content = str_replace(value, "\\.\\s+\\.", "..")) %>%
#   mutate(content = str_replace(content, "^(\\p{L})\\s", "")) %>% 
#   mutate(content = str_replace_all(content, "\\[|\\]|\\}|\\{|\\!", "|")) %>%
#   mutate(content = str_replace(content, "^\\s*\\|", "")) %>% 
#   mutate(content = str_replace(content, "\\|\\|", "|")) %>%
#   mutate(content = str_replace(content, "(?<=\\..)1", "|")) %>% 
#   mutate(content = str_replace_all(content, "(\\.{2,})\\s*([A-Za-z0-9])", "\\1|\\2")) %>%
#   mutate(content = str_trim(content, side = "left")) %>% 
#   mutate(content = str_replace(content, "(.)(dem)", "i\\2")) %>% 
#   mutate(arg = str_split(content, "(?<!\\d)\\.+(?!\\d)")) %>% 
#   mutate(arg = as.character(arg)) %>% 
#   mutate(arg = str_replace_all(arg, "\", \"", "")) %>%
#   mutate(arg = lapply(arg, function(x) str_replace_all(x, "^c\\(\"|\"\\)$", ""))) %>% 
#   separate_wider_delim(arg, delim = "|", names_sep = "", too_few = "align_start")


# data <- ines %>%
#   mutate(content = gsub("\\.\\s+\\.", "..", value)) %>%
#   mutate(content = gsub("\\[|\\]|\\}|\\{|\\!", "|", content)) %>%
#   mutate(content = gsub("\\|\\|", "|", content)) %>%
#   mutate(content = gsub("(?<=\\..)1", "|", content, perl = TRUE)) %>% 
#   mutate(content = str_replace_all(content, "(\\.{2,})\\s*([A-Za-z0-9])", "\\1|\\2")) %>% 
#   mutate(arg = str_split(content, "(?<!\\d)\\.+(?!\\d)"))
  
data <- ines %>%
  mutate(content = gsub("\\.\\s+\\.", "..", value)) %>%
  mutate(content = gsub("\\[|\\]|\\}|\\{|\\!", "|", content)) %>%
  mutate(content = str_replace(content, "^\\s*\\|", "")) %>% 
  mutate(content = gsub("\\|\\|", "|", content)) %>%
  mutate(content = gsub("(?<=\\..)1", "|", content, perl = TRUE)) %>% 
  mutate(content = str_replace_all(content, "(\\.{2,})\\s*([A-Za-z0-9])", "\\1|\\2")) %>% 
  mutate(arg = str_split(content, "(?<!\\d)\\.+(?!\\d)")) %>% 
  mutate(arg = as.character(arg)) %>% 
  mutate(arg = lapply(arg, function(x) gsub("^c\\(|\\)$", "", x))) %>% 
  separate_wider_delim(arg, delim = "|", names_sep = "", too_few = "align_start")


#test
data <- data %>%
  mutate(content2 = gsub("\\[|\\]|\\}|\\{|\\!", "|", content)) %>%
  mutate(content3 = gsub("(?<=\\..)1", "|", content2, perl = TRUE)) %>% 
  mutate(content4 = gsub("\\|\\|", "|", content3)) 





































































