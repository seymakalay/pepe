#' Summary Statistics by Factor
#'
#' @param var The vector to set summary statistics.
#' @param df The name of the Data set.
#' @return The output from  \code{\link{Stats.by.Factr}}.
#' @export
#' @importFrom psych describe
#' @examples
#' df <- sample_data[c("Formal","Informal","L.Both","No.Loan",
#' "sex","educ","political.afl","married",
#' "havejob","rural","age","Income","Networth","Liquid.Assets",
#' "NW.HE","fin.knowldge","fin.intermdiaries")]
#' CN = colnames(df)
#' var <- c("educ")
#' Stats.by.Factr(var, df)
#'


# var <- c("cyl", "vs", "am")
# Stats.by.Factr(var, mtcars)

Stats.by.Factr <- function(var, df){

  df <- df[, !sapply(df, is.character)] #REMOVE THE CHARACTER COLUMNS
  CN = colnames(df)

  k = 0
  #Summ.Stats.2 <- NULL
  lst.names <- NULL
  Mean.tab <- NULL
  for (i in 1:length(var)) {
    temp <- which(CN == var[i])
    #
    res <- split(df, df[,temp])
    names(res) <- paste(var[i], names(res), sep = ".") #"educ.0" "educ.1"
    lst.names <- c(lst.names, names(res))

    for (j in 1:length(res))
    {
      k = k + 1
      tab <- describe(res[[j]])
      #Summ.Stats.2 [[k]] <- tab
      Mean.tab[[k]] <- tab[, c("mean", "sd", "n","median",
                               "min","max","skew","kurtosis")]
    }
  }

  #names(Summ.Stats.2) <- lst.names
  #return(Summ.Stats.2)
  names(Mean.tab) <- lst.names
  return(Mean.tab)
}


