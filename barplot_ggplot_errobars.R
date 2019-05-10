#
# Written by: Andreé Johnsson,andreejohnsson@outlook.com
#
#
barPlot.aj <- function(df, group) {
    require(magrittr);require(tidyverse)
    #
    # Returns a barchart with standard error bars, by specidying a grouping
    # variable given a tidy dataframe of only the geouping variable and the
    # data to be plotted.
    names(group)= NULL
    df.mean <- df %>% 
        select_if(is.numeric) %>% aggregate(group,mean)
    df.sd <- df %>% 
        select_if(is.numeric) %>% aggregate(group,sd)
    df.sd[-1] <- df.sd[-1]/sqrt(table(unlist(group))) # This specifies the stadard error
    
    df.mean  %<>% gather(type,mean, -Group.1)
    df.sd %<>% gather(type,se, -Group.1) # This names the stadard error,se
    df.full <- inner_join(df.mean,df.sd,by =c("type","Group.1"))
    
    p.1 <-
        df.full %>%
        ggplot(data=., aes(x=type, y=mean,mgroup=Group.1, fill= Group.1)) +
        geom_bar(stat = "identity", color = "black", 
                 position = position_dodge() ) +
        geom_errorbar(aes(ymax = mean+se, ymin = mean-se),
                      width = .2, position = position_dodge(.9) ) +
        theme_bw() +
        scale_fill_brewer(palette = "Spectral") 
    
    return(p.1)
}
# Example 
# ex.iris <-
#     iris %>%
#     bar.plot.aj(Species,label.vector = c("Meassure","Mean (cm)"
#                     , "A barplot with error bars of the Iris DataFrame"))
# ex.iris + facet_wrap(~measure,scales = "free_x")
