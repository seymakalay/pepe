#' Pivot Table by Factor
#'
#' @param df The data frame of factor variables.
#' @return The output from  \code{\link{Pvot.by.Factr}}.
#' @export
#' @importFrom dplyr %>% bind_rows
#' @examples
#' df <- sample_data[c("multi.level",
#' "Formal","L.Both","No.Loan",
#' "region", "sex", "educ", "political.afl",
#' "married", "havejob", "rural",
#' "fin.knowldge", "fin.intermdiaries")]
#' Pvot.by.Factr(df)

Pvot.by.Factr  <- function(df){

  s <- sapply(df, function(x) if("factor" %in% class(x)) {prop.table(table(x))})

  a <- bind_rows(s) %>% as.data.frame %>% `rownames<-`(names(df)) %>% round(4)

  a[] <- lapply(a, function(x)  x*100)
  a[] <- lapply(a, function(x) paste0(x,'%'))
  print(a[])

}


# @importFrom base %in%
# @importFrom base   sapply  lapply


# @importFrom  dplyr mutate summarise group_by select
# @importFrom tidyr pivot_longer pivot_wider
# @importFrom  utils globalVariables
# @importFrom dplyr %>%

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



#' sample_data$X <- 1
#' dscriptve <-  as.data.frame(sample_data[, c("X", "sex",
#' "married", "havejob", "educ","political.afl", "rural","region",
#' "multi.level", "Formal", "Informal", "L.Both", "No.Loan")])
#'
#' colnames(dscriptve) <- c("X", "Gender", "Married", "Employed",
#' "Education", "Party", "HR", "Region",
#' "Access Loan", "Formal", "Informal", "L.Both", "No.Loan")
#'
#' dscriptve[] <- lapply(dscriptve[], factor); names(dscriptve)
#' Pvot.by.Factr(dscriptve)
#'
#' name.df <- Pvot.by.Factr(dscriptve)[, c(1,2)]
#' df <- round(as.data.frame(lapply(Pvot.by.Factr(dscriptve)[,-c(1,2)], function(x) x*100)),2); df
#' df <- as.data.frame(lapply(df, function(x) paste0(x,'%'))); df
#' Pvot.by.Factr <- cbind(name.df, df); head(Pvot.by.Factr,20)
#' Pvot.by.Factr <- subset(Pvot.by.Factr, select = -c(X))
#'






#df <- round(as.data.frame(lapply(Pvot.by.Factr(dscriptve)[,-c(1,2)], function(x) x*100)),2); df
#df <- as.data.frame(lapply(df, function(x) paste0(x,'%'))); df

# df <- round(as.data.frame(lapply(Pvot.by.Factr(dscriptve)[,-c(1,2)], function(x) x*100)),2)
# df <- as.data.frame(lapply(df, function(x) paste0(x,'%')))
#
#name.df <- Pvot.by.Factr(dscriptve)[, c(1,2)];name.df

#if(getRversion() >= "2.15.1")  globalVariables(c("X", "name", "value", "prop", "n"))






