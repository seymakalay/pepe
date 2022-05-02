#' Pivot Table by Factor
#'
#' @param dsc The matrix of factor variables.
#' @return The output from  \code{\link{Pvot.by.Factr}}.
#' @export
#' @importFrom  dplyr mutate summarise group_by select
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom  utils globalVariables
#' @importFrom dplyr %>%




#df <- round(as.data.frame(lapply(Pvot.by.Factr(dscriptve)[,-c(1,2)], function(x) x*100)),2); df
#df <- as.data.frame(lapply(df, function(x) paste0(x,'%'))); df

# df <- round(as.data.frame(lapply(Pvot.by.Factr(dscriptve)[,-c(1,2)], function(x) x*100)),2)
# df <- as.data.frame(lapply(df, function(x) paste0(x,'%')))
#
#name.df <- Pvot.by.Factr(dscriptve)[, c(1,2)];name.df

#if(getRversion() >= "2.15.1")  globalVariables(c("X", "name", "value", "prop", "n"))


Pvot.by.Factr  <- function(dsc){
  (dsc
   %>% pivot_longer(-X)       ## spread out variables (vs, am)
   %>% group_by(X, name)
   %>% mutate(n = n())            ## obs per cyl/var combo
   %>% group_by(X, name, value)
   %>% summarise(prop = n()/n)    ## proportion of 0/1 per cyl/var
   %>% unique()                 ## not sure why I need this?
   %>% pivot_wider(id_cols = c(X, name),
                   names_from = value, values_from = prop)

   # %>% mutate(name = factor(name, levels = c("Gender", "Married", "Employed", "Education", "Party",
   #                                           "HR", #"Region",
   #                                           "Access Loan", "Formal", "Informal", "L.Both", "No.Loan")))
   %>% group_by(X)
   %>% arrange(name, .by_group = TRUE)
  )
}



