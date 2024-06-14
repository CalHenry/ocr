# This function regroup many string manipulation commands.
# Ultimate goal is to clean the "content" var then separate it into several var

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
