# This function takes a string (label) and shorten it to a specificied lenght (nchar), included the ".." placed as placeholder for the ripped part.
#I: string (label) with a certain (excessive) lenght
#O: return the shortened version of the input string, where the ripped part is substituted with "..."

label_shortener <- function(label, n_char) {
  ifelse(nchar(label) > (n_char - 3), 
         {substr(label, 1, n_char) %>% paste0(., "...")},
         label)
}