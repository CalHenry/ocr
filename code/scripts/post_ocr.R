###########################################################################
###                                                                     ###
###                      second script: "post_ocr.R"                    ###
###                           OCR project                               ###
###                           Calixte HENRY                             ###
###                                                                     ###
###########################################################################


#  split and process the ocr output  -------------------------------------------

processed_ocr_text <- lapply(ocr_text, split_and_process_ocr_text)

# detail of the text cleaning in the function file

#' The datasets of the ocr are stored in a list
#' Each ds of the list is a scan.
#' In each dataset:
#' - text is the raw text from OCR. We never modify this
#' - content is the var that received all the modifications, it is a "cleanned" version of text
#' - the "arg" vars are "content" var cut into pieces, supposed to reproduce the columns of the table on the scan.
#' Output cannot be perfect due to OCR quality. They receive modifications later in the code

# reorder the list ----
# This part reorder the datasets so they are in the same order as the pages in the document.
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


# Turn the list to a proper dataset ----
ds_text <- bind_rows(reordered_list)

ds_text <- ds_text %>%
  mutate(content = if_else((str_count(text , "[[:punct:]&&[^.]]|[:blank:]|\\|") / str_length(content)) > 0.6, NA, content)) %>% 
  mutate(content = if_else(str_length(text) <= 2, NA, content)) %>% 
  filter(if_any((3:4), ~ !is.na(.) & . != "")) %>%  
  mutate(arg1 = str_replace(arg1, "^[^a-zA-Z]+(.*)$", "\\1")) %>% 
  mutate_at(vars(4:ncol(ds_text)), ~ str_replace(., "^[^[:alnum:]]+(.*)$", "\\1")) 

# Detail of each line in order:
# replace with na if the string has more than 60% of punctuation
# replace with NA if the string lenght is <=2
# remove rows where arg1 and arg2 are empty (meanninless strings)
# remove leading non alpha from the string (arg1 is mine names and we don't expect numbers)
# same but include numbers because it's arg2 to argn


### Dummies to help identify weird values ----
ds_text <- ds_text %>%
  mutate(has_long_word = as.integer(rowSums(sapply(c("arg1"),
                                                   function(col) str_detect(ds_text[[col]], "\\b\\w{13,}\\b"))) > 0), 
         has_long_number = as.integer(rowSums(sapply(c("arg1"),
                                                   function(col) str_detect(ds_text[[col]], "\\b\\d{6,}\\b"))) > 0)) 
# first one for words (13+ char), second for numbers (6+ digits)

### More data manipulation ----
ds_text <- ds_text %>%
  mutate(content = str_replace_all(content, "([^a-zA-Z])(La|Le|L')([^a-zA-Z])", function(x) {
    if (is.na(x) || str_detect(x, "\\([a-zA-Z]+\\)")) {
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
#' if (str_detect(x, "\\([a-zA-Z]+\\)")), if the pattern match, do nothing.
#' the else add a "(" or a ")" to the find pattern
#' 2. second mutate: replace all empty string or string that contains 2 or less charaters to proper NA
#' 3. Rowwise, third mutate: if arg2 is NA, copy the content of arg3 to arg2.
#' then if arg2 is the same value as arg3, turn arg3 to NA.
#' (iow, after copying arg3 to arg2, we need to replace arg3 with NA).
#' This last operation is done on arg3 only because the content of the other arg vars don't correspond to proper columns as well as arg2 does.

ds_text$


# Export dataset to excel ----
wb <- createWorkbook() # create a workbook

addWorksheet(wb, "OCR", gridLines = TRUE) #add a worksheet to the workbook

writeData(wb, "OCR", ds_text)
addFilter(wb, 1, row = 1, cols = 12:ncol(ds_text))

warm1Style <- createStyle(fontColour = "#000000", bgFill = "#FFFF00")

conditionalFormatting(wb, "OCR",
                      rows = 1:nrow(ds_text),
                      cols = 12:ncol(ds_text),
                      rule = "1",
                      type = "contains",
                      style = warm1Style)

addStyle(wb, "OCR", cols = 1, rows = 1, style = createStyle(fgFill = "#9BBB59"))
addStyle(wb, "OCR", cols = 2, rows = 1, style = createStyle(fgFill = "#DA9694"))
addStyle(wb, "OCR", cols = 3:(ncol(ds_text)-2), rows = 1, style = createStyle(fgFill = "#92CDDC"))
addStyle(wb, "OCR", cols = 12:ncol(ds_text), rows = 1, style = createStyle(fgFill = "#B1A0C7"))

saveWorkbook(wb, "data/output/ds_text.xlsx", overwrite = TRUE)
