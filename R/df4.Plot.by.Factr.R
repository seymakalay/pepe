#' Creating Dataset for Plot.by.Factr
#'
#' @param var Vector of factor variables.
#' @param df  Dataset.
#'
#' @return The output from  \code{\link{df4.Plot.by.Factr}}
#' @export
#' @importFrom psych describe
#' @importFrom tidyr gather
#' @importFrom dplyr group_by summarise arrange mutate ungroup filter across
#' @importFrom dplyr %>%
#' @examples
#' df <- sample_data[c("Formal","Informal","L.Both","No.Loan",
#' "sex","educ","political.afl","married",
#' "havejob","rural","age","Income","Networth","Liquid.Assets",
#' "NW.HE","fin.knowldge","fin.intermdiaries")]
#' CN = colnames(df)
#' var <- c("educ", "rural", "sex", "havejob", "political.afl")
#' df4.Plot.by.Factr(var,df)
#'


df4.Plot.by.Factr <- function(var, df) {

  tab.0.prop <- tab.1.prop <- NULL
  Summ.Stats <- Summ.Stats.long <-NULL
  out <- NULL

  for (i in 1:(length(var))) {
    CN = colnames(df)

    temp <- which(CN==var[i])
    aux.0 <- df %>% filter(df[,temp]==0)
    aux.1 <- df  %>% filter(df[,temp]==1)

    fname.0 <- paste0(paste(var[i], "0", sep = "_"))
    fname.1 <- paste0(paste(var[i], "1", sep = "_"))
    col.diff <- paste0(paste(var[i], "diff", sep = "_"))

    Summ.0 <- aux.0 %>%  summarise(across(where(is.numeric), mean),
                                   across(where(is.factor), ~ prop.table(table(.x))[2]))

    Summ.1 <- aux.1 %>%  summarise(across(where(is.numeric), mean),
                                   across(where(is.factor), ~ prop.table(table(.x))[2]))

    col.tab.names <- colnames(Summ.0)

    colnames(Summ.0) <- colnames(Summ.1) <- NULL
    Summ.0 <- as.numeric(Summ.0)
    Summ.1 <- as.numeric(Summ.1)

    diff <- abs(Summ.1 - Summ.0)
    tab <- round(cbind(Summ.0, Summ.1, diff),3)
    #tab <- round(cbind(Summ.0, Summ.1, diff),3)
    rownames(tab) <- col.tab.names
    colnames(tab) <- c(fname.0, fname.1,  col.diff)
    Summ.Stats [[i]] <- tab

    out <- gather(as.data.frame(tab), levels, values, fname.0:fname.1)
    tab2 <- cbind(out,rep(col.tab.names, 2))

    colnames(tab2) <- c("Diff", "Levels", "Mean", "Variable")
    Summ.Stats.long[[i]] <- tab2
  }

  Summ.Stats
  Summ.Stats.long
  output <- list( Summ.Stats = Summ.Stats,
                  Summ.Stats.long = Summ.Stats.long)

  return(output)

}
