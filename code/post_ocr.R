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

wc_split_split <- lapply(even_ocr_text, split_and_process_ocr_text)

names(split_split) <- paste0("txt_", seq_along(split_split))



































