#' \code{pepe} package
#'
#'
#' See the README on
#'  \href{https://github.com/seymakalay/pepe#readme}{GitHub}
#'
#' @docType package
#' @name pepe
#'@importFrom dplyr %>%
NULL

if(getRversion() >= "2.15.1")   globalVariables(c(".", "where", "Variable", "Mean", "Levels",
                                                 "Max", "Min", "Diff", "var", "values",
                                                 "name.levels"))
                                                 #"X", "name", "value", "prop", "n",


#output <-   NULL

#Pvot.by.Factr  <- function(dsc){
#  (dsc
#   %>% pivot_longer(-X)       ## spread out variables (vs, am)
#   %>% group_by(X, name)
#   %>% mutate(n = n())            ## obs per cyl/var combo
#   %>% group_by(X, name, value)
#   %>% summarise(prop = n()/n)    ## proportion of 0/1 per cyl/var
#   %>% unique()                 ## not sure why I need this?
#   %>% pivot_wider(id_cols = c(X, name),
#                   names_from = value, values_from = prop)

# %>% mutate(name = factor(name, levels = c("Gender", "Married", "Employed", "Education", "Party",
#                                           "HR", #"Region",
#                                           "Access Loan", "Formal", "Informal", "L.Both", "No.Loan")))
#   %>% group_by(X)
#   %>% arrange(name, .by_group = TRUE)
#  )
#}
