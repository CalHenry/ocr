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

processed_ocr_text <- lapply(ocr_text, split_and_process_ocr_text)

# detail of the text cleanning in the function file

#names(processed_ocr_text) <- paste0("txt_", seq_along(processed_ocr_text))

#' Datasets of the ocr are stored in a list
#' Each ds of the list is a scan.
#' In eahc dataset:
#' - text is a var containing the raw text from OCR
#' - content is the var that received all the modifications.
#' - the "arg" vars are "content" var cut into pieces, supposed to reproduce the colomns of the table on the scan.
#' Outputcannot be perfect due to OCR quality.


#test <- bind_rows(processed_ocr_text)


even_txt_pages <- processed_ocr_text[seq(1, 28)]
names(even_txt_pages) <- paste0("txt_", seq_along(even_txt_pages), "_e")

odd_txt_pages <- processed_ocr_text[seq(29, 56)]
names(odd_txt_pages) <- paste0("txt_", seq_along(odd_txt_pages), "_o")

reordered_list <- vector("list", (length(e_pages)*2))

for (i in 1:28) {
  reordered_list[[2*i-1]] <- even_txt_pages[i]
  reordered_list[[2*i]] <- odd_txt_pages[i]
}

reordered_list <- unlist(reordered_list, use.names = TRUE, recursive = FALSE)


test <- bind_rows(reordered_list)































