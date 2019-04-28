#
# Writen by: Andreé Johnsson <andreejohnsson@outlook.com>
#
#
bar.plot.aj <- function(df, grouping.var, label.vector = NULL) {
  # Returns a barchart with standard error bars, by specidying a grouping
  # variable given a tidy dataframe of only the geouping variable and the
  # data to be plotted.
  require(magrittr)
  require(tidyverse)
  grouping.var <- enquo(grouping.var)
  df.sum <-
    df %>%
    group_by(!!grouping.var) %>%
    summarise_if(is.double, list(mean = mean, sd = sd)) %>%
    gather(., measure, value, 2:ncol(.)) %>%
    separate(measure, into = c("measure", "statistic"), sep = "_") %>%
    spread(statistic, value) %>%
    mutate(
      seer.up = mean + sd / sqrt(mean),
      seer.down = mean - sd / sqrt(mean)
    )
  p.1 <-
    df.sum %>%
    ggplot(data = ., aes(
      x = measure, y = mean,
      group = (!!grouping.var), fill = (!!grouping.var)
    )) +
    geom_bar(
      stat = "identity", color =
        "black", position = position_dodge()
    ) +
    geom_errorbar(aes(ymin = seer.down, ymax = seer.up),
      width = .2,
      position = position_dodge(.9)
    ) +
    theme_bw() +
    scale_fill_brewer(palette = "Spectral")

  if (!is.null(label.vector)) {
    p.1 <- p.1 + labs(
      x = label.vector[1], y = label.vector[2],
      title = label.vector[3]
    )
  }

  return(p.1)
}

# Example 
# ex.iris <-
#     iris %>%
#     bar.plot.aj(Species,label.vector = c("Meassure","Mean (cm)"
#                     , "A barplot with error bars of the Iris DataFrame"))
# ex.iris + facet_wrap(~measure,scales = "free_x")
