na_if_high_punctuation <- function(str) {
  punctuation_ratio <- str_count(str, "[:punct:]") / str_length(str)
  if (punctuation_ratio > 0.8) {
    return(NA)
  } else {
    return(str)
  }
}