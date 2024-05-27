split_and_process_ocr_text <- function(text) {
  processed_dataset <- tibble(text = unlist(strsplit(text, "\n"))) %>%
    mutate(content = str_replace(text, "\\.\\s+\\.", "..")) %>%
    mutate(content = str_replace(content, "^(\\p{L})\\s", "")) %>% 
    mutate(content = str_replace_all(content, "\\[|\\]|\\}|\\{|\\!", "|")) %>%
    mutate(content = str_replace(content, "\\|\\|", "|")) %>%
    mutate(content = str_replace(content, "^\\s*\\|\\s*", "")) %>% 
    mutate(content = str_replace(content, "(?<=\\.\\.)1", "|")) %>% 
    mutate(content = str_replace_all(content, "(\\.{2,})\\s*([A-Za-z0-9])", "\\1|\\2")) %>%
    mutate(content = str_trim(content, side = "left")) %>% 
    mutate(content = str_replace(content, "(.)(dem)", "i\\2")) %>% 
    mutate(content = str_replace(content, "^\\d+\\s*", "|'")) %>% 
    mutate(arg = str_split(content, "(?<!\\d)\\.+(?!\\d)")) %>% 
    mutate(arg = as.character(arg)) %>% 
    mutate(arg = str_replace_all(arg, "\", \"", "")) %>%
    mutate(arg = str_replace_all(arg, "^c\\(\"|\"\\)$", "")) %>% 
    separate_wider_delim(arg, delim = "|", names_sep = "", too_few = "align_start")
  
  return(processed_dataset)
}

# Breakdown of the function -------------------------------------------------

# processed_dataset <- tibble(text = unlist(strsplit(text, "\n"))) %>%
#   split the text by new lines "\n", then turn the list of these texts to dataset.
#   Getting ready for all the text cleanning.

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

# mutate(content = str_replace(content, "(?<=\\.\\.)1", "|")) %>%:
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
