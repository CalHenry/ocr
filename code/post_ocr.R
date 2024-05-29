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
  mutate(content = if_else(str_length(text) <= 2, NA, content)) %>% # replace with NA if the string lenght is <=2
  filter(if_any((3:4), ~ !is.na(.) & . != "")) %>%  #remove rows where arg1 and arg2 are empty (meanninless strings)
  mutate(arg1 = str_replace(arg1, "^[^a-zA-Z]+(.*)$", "\\1")) %>% # remove leading non alpha from the string (arg1 is mine names and we don't expect numbers)
  mutate_at(vars(4:ncol(test)), ~ str_replace(., "^[^[:alnum:]]+(.*)$", "\\1")) %>% # same but include number bc it's arg2 to argn

tes <- tes %>%
  mutate(has_long_word = as.integer(rowSums(sapply(c("arg1"),
                                                   function(col) str_detect(tes[[col]], "\\b\\w{13,}\\b"))) > 0), 
         has_long_number = as.integer(rowSums(sapply(c("arg1"),
                                                   function(col) str_detect(tes[[col]], "\\b\\d{6,}\\b"))) > 0)) 
#Dummies to help identfy weird vaues. 
# first one for words (13+ char), second for numbers (6+ digits)

tes <- test %>%
  mutate(content = str_replace_all(content, "([^a-zA-Z])(La|Le|L')([^a-zA-Z])", function(x) {
    if (str_detect(x, "\\([a-zA-Z]+\\)")) {
      return(x)
    } else {
      return(paste0(str_extract(x, "([^a-zA-Z])"), "(", str_extract(x, "(La|Le|L')"), ")", str_extract(x, "([^a-zA-Z])")))
    }
  })) %>%
  mutate_all(~ ifelse(. == "" | nchar(.) <= 2, NA, .)) %>%
  rowwise() %>%
  mutate(arg2 = coalesce(arg2, arg3),
         arg3 = if_else(arg2 != arg3, arg3, NA)) %>% 
  ungroup()

#' This part modify the dataset, 3 distinct operations: 
#' 1. first mutate: goal: find the Le, La and L' and enclose them into parenthesis if they are not
#' str_replace_all() detect the Le/La/L'
#' the "if" statement is used to make the code do nothing if the pattern is already into parenthesis.
#' if (str_detect(x, "\\([a-zA-Z]+\\)")) 
#' the else add a "(" or a ")" to the find pattern
#' 2. second mutate: replace all empty string or string that contains 2 or less charaters to proper NA
#' 3. Rowwise, third mutate: if arg2 is NA, copy the content of arg3 to arg2.
#' then if arg2 is the same value as arg3, turn arg3 to NA.
#' (iow, after copying arg3 to arg2, we need to replace arg3 with NA).
#' This last operation is done on arg3 only because the content of the other arg vars don't correspond to proper columns as well as arg2 does.





#write.xlsx(test, "data/int/test.xlsx")


















