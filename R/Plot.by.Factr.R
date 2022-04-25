#' Plot by Factor and Continuous
#'
#' @param XXX object to be plotted.
#' @return The output from  \code{\link{Plot.by.Factr}}.
#' @export
#' @importFrom psych describe
#' @importFrom tidyr gather spread
#' @importFrom  dplyr group_by summarise arrange mutate ungroup filter
#' @importFrom   ggplot2 ggplot
#' @examples
#' \dontrun{
#' df <- sample_data[c("Formal","Informal","L.Both","No.Loan",
#' "sex","educ","political.afl","married",
#' "havejob","rural","age","Income","Networth","Liquid.Assets",
#' "NW.HE","fin.knowldge","fin.intermdiaries")]
#' CN = colnames(df)
#' var <- c("educ","rural","sex","havejob","political.afl")
#' df4.Plot.by.Factr(var,df)
#' Plot.by.Factr(df4.Plot.by.Factr(var,df)$Summ.Stats.long)
#'
#' }

# XXX = df4.Plot.by.Factr(var,df)$Summ.Stats.long

#Plot.by.Factr <-  df4.Plot.by.Factr(var,df); Plot.by.Factr
#var <- c("educ", "rural", "sex", "havejob", "political.afl")




Plot.by.Factr <- function(XXX){

  #descriptive.plots  <- "descriptive.plots."

  for (i in seq_along(XXX)) {

    #plot <- ggplot(out[[i]], aes(x= values, y= variables, colour=levels )) +
    #   geom_point()

    XXX[[i]]$Variable <- factor(XXX[[i]]$Variable,
                                levels = rev(unique(XXX[[i]]$Variable)),
                                ordered = TRUE)


    city_rev <- XXX[[i]]  %>%
      group_by(Variable) %>%
      summarise(Mean = sum(Mean, na.rm = TRUE)) %>%
      arrange(Mean) %>%
      mutate(Variable  = factor(Variable , levels = .$Variable))
    #dplyr::mutate(Variable = factor(Variable, levels = rev(unique(.$Variable))))


    df_groupby <- XXX[[i]]  %>%
      group_by(Variable, Levels) %>%
      summarise(Mean = sum(Mean, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Variable = factor(Variable, levels = city_rev$Variable))

    df_groupby$Variable <- factor(df_groupby$Variable,
                                  levels = rev(unique(df_groupby$Variable)), ordered = TRUE)

    right_label <- df_groupby  %>%
      group_by(Variable) %>%
      arrange(desc(Mean)) %>%
      top_n(1)

    left_label <- df_groupby %>%
      group_by(Variable) %>%
      arrange(desc(Mean)) %>%
      slice(2)

    # create data frame that identifies revenue differences over 20%
    big_diff <- df_groupby %>%
      spread( Levels, Mean) %>%
      #dplyr::group_by(Variable) %>%
      mutate(Max = pmax(.[[2]] , .[[3]]),
             Min = pmin(.[[2]] , .[[3]]),
             Diff = Max / Min - 1) %>%
      arrange(desc(Diff)) %>%
      filter(Diff > .1) %>%
      group_by(Variable)

    big_diff <- big_diff[-1,] #creates inf

    # filter the label data frames to only include those cities where the
    # difference exceeds 20%
    right_label <- filter(right_label, Variable %in% big_diff$Variable)
    left_label  <- filter(left_label, Variable %in% big_diff$Variable)

    highlight <- filter(df_groupby, Variable %in% big_diff$Variable)

    # create a new label data frame
    plot_label <- big_diff %>%
      select(Variable, Mean = Max, Diff) %>%
      right_join(right_label)

    #out <- df_groupby
    #for(i in seq_along(out)) {
    #out[[i]] <- out[[i]][(out[[i]]$Variable == exog),]
    #}


    p <- ggplot(df_groupby, aes(x = Mean, y = Variable)) +
      geom_line(aes(group = Variable), alpha = .3) +
      scale_color_manual(values = c("black", "darkgrey"))+
      labs(x = "Mean Differences in %") +  #, y = "Variables")+

      scale_y_discrete(limits = rev(levels(df_groupby$Variable))) +

      geom_point(aes(color = Levels), size = 1.5, alpha = .3) +
      #xlim(0,1) +
      geom_line(data = highlight, aes(group = Variable)) +
      geom_point(data = highlight, aes(color =  Levels), size = 2, pch=19,  alpha=1) +
      geom_text(data = plot_label, aes(color = Levels,
                                       #label = paste0("+", scales::percent(round(Diff, 2)))),
                                       label = paste0("+", (round(Diff, 2))*100,"%")),
                size = 3, hjust = -.5) +
      theme_minimal() +
      theme(axis.title = element_blank())+
      scale_x_log10()


    ggsave(p, filename = paste("descriptive.plots.",
                               var[i], ".png", sep=''), scale=2)

    # ggsave(p, file = paste(descriptive.plots,
    #                           var[i], ".png", sep=''), scale=2)

    #path = "descriptive.plots", #filename ="out[[i]].png",
    # var[i], ".png", sep='',
    # width = 45, height = 15, units = "cm")

    print(p)
  }
}


#Plot.by.Factr()
