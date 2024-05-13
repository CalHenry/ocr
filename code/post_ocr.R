# text processing

# Packages ----------------------------------------------------------------
library(tidyverse)
library(hunspell)



#  ----------------------------------------------------------------

h <- read.csv("h_png.txt")
h <- h$x
dict <- hunspell::dictionary("fr_FR")

hunspell(h, dict, format = "text")

hunspell_stem(h, dict)



# Function to check if a word is misspelled
is_misspelled <- function(word) {
  hunspell_check(word, dict)
}

misspelled_words <- Filter(is_misspelled, ocr_text)


spell_check <- lapply(ocr_text, function(text) {
  a <- hunspell_check(word, dict)
  return(a)
})

#  ----------------------------------------------------------------
#  ----------------------------------------------------------------

processed_list <- lapply(even_ocr_text, function(value) {
  ines <- as_tibble(strsplit(value, "\n", fixed = TRUE)[[1]])
  text_cleanning(ines)
})


processed_list <- lapply(data, function(value) {
  content <- str_replace(value, "\\.\\s+\\.", "..")
  content <- str_replace(content, "^(\\p{L})\\s", "")
  content <- str_replace_all(content, "\\[|\\]|\\}|\\{|\\!", "|")
  content <- str_replace(content, "^\\s*\\|", "")
  content <- str_replace(content, "\\|\\|", "|")
  content <- str_replace(content, "(?<=\\..)1", "|")
  content <- str_replace_all(content, "(\\.{2,})\\s*([A-Za-z0-9])", "\\1|\\2")
  content <- str_trim(content, side = "left")
  content <- str_replace(content, "(.)(dem)", "i\\2")
  arg <- str_split(content, "(?<!\\d)\\.+(?!\\d)")
  arg <- as.character(arg)
  arg <- str_replace_all(arg, "\", \"", "")
  arg <- str_replace_all(arg, "^c\\(\"|\"\\)$", "")
  separate_wider_delim(arg, delim = "|", names_sep = "", too_few = "align_start")
})















































