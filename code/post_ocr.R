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
#' In each dataset:
#' - text is the raw text from OCR. We never modify this
#' - content is the var that received all the modifications, it is a "cleanned" version of text
#' - the "arg" vars are "content" var cut into pieces, supposed to reproduce the colomns of the table on the scan.
#' Outputcannot be perfect due to OCR quality. They receive modifications later in the code

# This part reoder the datasets so the dataset are in the same order as the pages in the document.
#We have an even page then the corresponding odd page, and so forth. Suffixes indicates if the page is even or odd.

even_txt_pages <- processed_ocr_text[seq(1, 28)]
names(even_txt_pages) <- paste0("txt_", seq_along(even_txt_pages), "_e")

odd_txt_pages <- processed_ocr_text[seq(29, 56)]
names(odd_txt_pages) <- paste0("txt_", seq_along(odd_txt_pages), "_o")

reordered_list <- vector("list", (length(even_txt_pages)*2))

for (i in 1:28) {
  reordered_list[[2*i-1]] <- even_txt_pages[i]
  reordered_list[[2*i]] <- odd_txt_pages[i]
}

reordered_list <- unlist(reordered_list, use.names = TRUE, recursive = FALSE)


# TUrn the list to a proper dataset
test <- bind_rows(reordered_list)



# tes <- test %>%
#   filter(if_any((3:4), ~ !is.na(.) & . != "")) #remove rows where arg1 and arg2 are empty (meanninless strings)
# 
# 

# tes <- test %>%
#   mutate(content = if_else((str_count(text , "[[:punct:]&&[^.]]|[:blank:]|\\|") / str_length(content)) > 0.6, NA, text)) %>% #repalce with na if the string has more than 60% of punctuation
#   mutate(content = if_else(str_length(text) <= 2, NA, content)) %>% # re^place with NA if the string lenght is <=2
#   filter(if_any((3:4), ~ !is.na(.) & . != "")) #remove rows where arg1 and arg2 are empty (meanninless strings)
# 
# tes <- test %>%
#   mutate(arg1 = str_replace(arg1, "^[^a-zA-Z]+(.*)$", "\\1")) %>% # remove leading non alpha from the string (arg1 is mine names and we don't expect numbers)
#   mutate_at(vars(4:ncol(test)), ~ str_replace(., "^[^[:alnum:]]+(.*)$", "\\1")) # same but include number bc it's arg2 to argn


tes <- test %>%
  mutate(content = if_else((str_count(text , "[[:punct:]&&[^.]]|[:blank:]|\\|") / str_length(content)) > 0.6, NA, content)) %>% #repalce with na if the string has more than 60% of punctuation
  mutate(content = if_else(str_length(text) <= 2, NA, content)) %>% # re^place with NA if the string lenght is <=2
  filter(if_any((3:4), ~ !is.na(.) & . != "")) %>%  #remove rows where arg1 and arg2 are empty (meanninless strings)
  mutate(arg1 = str_replace(arg1, "^[^a-zA-Z]+(.*)$", "\\1")) %>% # remove leading non alpha from the string (arg1 is mine names and we don't expect numbers)
  mutate_at(vars(4:ncol(test)), ~ str_replace(., "^[^[:alnum:]]+(.*)$", "\\1")) %>% # same but include number bc it's arg2 to argn
  mutate(has_long_word = as.integer(rowSums(sapply(c("arg1"), function(col) str_detect(tes[[col]], "\\b\\w{13,}\\b"))) > 0),
         has_long_number = as.integer(rowSums(sapply(c("arg1"), function(col) str_detect(tes[[col]], "\\b\\d{6,}\\b"))) > 0)) #dummies


#write.xlsx(test, "data/int/test.xlsx")






