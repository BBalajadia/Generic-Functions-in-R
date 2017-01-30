#####################################################################
#     Bryan R. Balajadia                                           
#     Fn: cosine.match                                             
#     Function to extract string using cosine similarity measure  
#     Arguments: str.pattern - string pattern; character vector 
#            string - vector on which to search for the str.pattern
#     Dependency: stringdist package
#####################################################################

cosine.match <- function (str.pattern, string) {
  string <- Filter(function(x) !all(c("") %in% x), string)
  match.string.loc <- amatch(str.pattern, string, method = "cosine", 
                             maxDist = Inf, q = 1, matchNA = FALSE)
  match.string <- string[match.string.loc]
  df <- data.frame(str.pattern,match.string.loc, match.string)
  return(df)
}
