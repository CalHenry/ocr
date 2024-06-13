split_and_process_ocr_text <- function(text) {
  processed_dataset <- tibble(text = unlist(strsplit(text, "\n"))) %>%
    mutate(content = str_replace(text, "\\.\\s+\\.", "..")) %>%
    mutate(content = str_replace(content, "\\.\\,\\.", "..")) %>%
    mutate(content = str_replace(content, "^(\\p{L})\\s", "")) %>% 
    mutate(content = str_replace_all(content, "\\[|\\]|\\}|\\{|\\!|(,<=\\.\\))", "|")) %>%
    mutate(content = str_replace(content, "^\\s*\\|+\\s*", "")) %>% 
    mutate(content = str_replace(content, "(?<=\\.\\.)1", "|")) %>% 
    mutate(content = str_replace_all(content, "(\\.{2,})\\s*([A-Za-z0-9])", "\\1|\\2")) %>%
    mutate(content = str_trim(content, side = "left")) %>% 
    mutate(content = str_replace(content, "(.)(dem)", "i\\2")) %>% 
    mutate(content = str_replace(content, "^\\d+\\s*", "|'")) %>% 
    mutate(content = str_replace_all(content, "([^a-zA-Z])(La|Le|L')([^a-zA-Z])", function(x) {
      if (is.na(x) || str_detect(x, "\\([a-zA-Z]+\\)")) {
        return(x)
      } else {
        return(paste0(str_extract(x, "([^a-zA-Z])"), "(", str_extract(x, "(La|Le|L')"), ")", str_extract(x, "([^a-zA-Z])")))
      }
    })) %>%
    mutate(content = str_replace_all(content, "\\s*(\\d{1,3}[.,]\\d{1,3}[^\\|]*)", function(x) {
      return(paste0("|", x, "|"))
    })) %>%
    mutate(content = 
             {
               if (any(str_detect(content, "(\\d{1,3}[.,]{1}\\d{1,3})"))) {
                 str_replace_all(content, "\\s*(\\d{1,3}[.,]\\d{1,3})", "\\|\\0\\|")
               } else {
                 str_replace_all(content, "\\d{1,3}\\s+", "\\|\\0\\|")
               }
             }) %>% 
    mutate(content = str_replace_all(content, "\\|\\s*\\|*", "|")) %>% 
    mutate(arg = str_split(content, "(?<!\\d)\\.+(?!\\d)")) %>% 
    mutate(arg = as.character(arg)) %>% 
    mutate(arg = str_replace_all(arg, "\", \"", "")) %>%
    mutate(arg = str_replace_all(arg, "^c\\(\"|\"\\)$", "")) %>% 
    separate_wider_delim(arg, delim = "|", names_sep = "", too_few = "align_start")
  
  return(processed_dataset)
}

# ----------------------- Breakdown of the function ---------------------------

# processed_dataset <- tibble(text = unlist(strsplit(text, "\n"))) %>%
#   split the text by new lines "\n", then turn the list of these texts to dataset.
#   Getting ready for all the text cleanning.

#  mutate(content = str_replace(value, "\\.\\s+\\.", "..")) %>%
#  mutate(content = str_replace(content, "\\.\\,\\.", "..")) %>%:
# remove whitespaces (comma for the second line) in between sequences of periods ".. ... -> ....."

#  mutate(content = str_replace(content, "^(\\p{L})\\s", "")) %>%
#   Remove alone leading letter followed by whitespace at beg of strings.

#   mutate(content = str_replace_all(content, "\\[|\\]|\\}|\\{|\\!|(,<=\\.\\))", "|")) %>%
#  replace all occurrences of square brackets, curly braces, exclamation marks, and the pattern (,<=.)) with a vertical bar (|).

#  mutate(content = str_replace(content, "^\\s*\\|+\\s*", "")) %>% 
# removes any leading whitespace and vertical bars from the content strings.

#  mutate(content = str_replace(content, "(?<=\\.\\.)1", "|")) %>%  
# replaces any occurrences of "..1" with a vertical bar (|).

#  mutate(content = str_replace_all(content, "(\\.{2,})\\s*([A-Za-z0-9])", "\\1|\\2")) %>%
# replace any sequence of two or more periods followed by optional whitespace and then a letter or digit 
# with the sequence of periods, a vertical bar, and the letter or digit.

# mutate(content = str_trim(content, side = "left")) %>%
# removes any leading whitespace from the content strings.

# mutate(content = str_replace(content, "(.)(dem)", "i\\2")) %>% 
# replaces any occurrence of a single character followed by "dem" with "idem".

# mutate(content = str_replace(content, "^\\d+\\s*", "|'"))
# replaces any leading digits followed by optional whitespace with a vertical bar and a single quote (|')

# mutate(content = str_replace_all(content, "([^a-zA-Z])(La|Le|L')([^a-zA-Z])", function(x) {
#   if (is.na(x) || str_detect(x, "\\([a-zA-Z]+\\)")) {
#     return(x)
#   } else {
#     return(paste0(str_extract(x, "([^a-zA-Z])"), "(", str_extract(x, "(La|Le|L')"), ")", str_extract(x, "([^a-zA-Z])")))
#   }
# }))
# replaces any occurrences of [any alpha symbol]("La" or "Le" or "L'")[any alpha symbol]
# by itself if the string is == NA or has parenthesis around a word
# by a reconstructed matched text by extracting the non-alphabetic characters before and after the "La", "Le", or "L'"
# and inserting the "La", "Le", or "L'" between them.

# mutate(content = str_replace_all(content, "\\s*(\\d{1,3}[.,]\\d{1,3}[^\\|]*)", function(x) {
#   return(paste0("|", x, "|"))
# })) %>%
# find any sequence of 1 to 3 digits, followed by a period or comma, followed by 1 to 3 more digits,
# and optionally followed by any non-vertical bar characters.
# replaces it by the match wrapped in "|" : |match|

# mutate(contents = 
#          {
#            if (any(str_detect(content, "(\\d{1,3}[.,]{1}\\d{1,3})"))) {
#              str_replace_all(content, "\\s*(\\d{1,3}[.,]\\d{1,3})", "\\|\\0\\|")
#            } else {
#              str_replace_all(content, "\\d{1,3}\\s+", "\\|\\0\\|")
#            }
#          })

# This code find digits and enclose them into pipes.
# We use a "if" statement because we differentiate 2 cases:
# when the number has decimals and when it doesn't.
# "if" statement correspond to number with digits (123,456)
# "else" statement are the other numbers
# the match is returned enclosed into pipes |123,456|

# mutate(content = str_replace_all(content, "\\|\\s*\\|*", "|")) %>%
# replace any sequence of pipes (including pipes with whitespaces inbetween) by a single pipe
# "||  |||" --> "|"

# mutate(arg = str_split(content, "(?<!\\d)\\.+(?!\\d)")) %>% 
# is used to keep the relevant content of each string.
# split the string based on a pattern that catch sequence of 1 or more dots.
# output is a vector containing all that was in between the sequences of dots
# "Bert....................| Houille..............| 1,337 62.125 361|"
# --> "Bert" "| Houille" "| 1,337 62.125 361|"

# mutate(arg = as.character(arg)) %>% 
# turn to character class

# mutate(arg = str_replace_all(arg, "\", \"", "")) %>%
# mutate(arg = str_replace_all(arg, "^c\\(\"|\"\\)$", "")) %>% 
# These lines are used to remove the c("   "), that appears after converting to char class
# first line remove the "
# second line removes the "c(" and the ")"

# separate_wider_delim(arg, delim = "|", names_sep = "", too_few = "align_start")
# cut the arg var into pieces based on the delimiter "|"
# we obtain the final form of the dataset with the "arg" split for each distinct element of the string.

