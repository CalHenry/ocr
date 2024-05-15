split_text_and_process <- function(text) {
  processed_dataset <- tibble(text = unlist(strsplit(text, "\n"))) %>%
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
  
  return(processed_dataset)
}