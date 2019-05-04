#
#
#
#
volcano.plot.2 <- function(res.df, title="Volcano Plot", use.labels=FALSE) {
    # res.df is a regular df with at least the columns
    # log2FoldChange and padj 
    require(ggplot2);require(ggrepel)
    res.df %<>% rownames_to_column(var="genes")
    res.df %<>% filter(!is.na(padj))
    res.df %<>% 
        mutate(genes,
               log2FoldChange,
               neg.log.10.padj = -log10(padj),
               Significance = ifelse(
                   (padj <= .01 & !is.na(padj) & abs(log2FoldChange) >= 0.5)
                   , "Yes", "No") )
    res.df$Significance %<>% factor(levels =c("Yes","No") )
    
    res.df.sig <- filter(res.df,Significance=="Yes")
    up.down <-
        res.df.sig %>%
        summarise(Up = sum(log2FoldChange>0, na.rm = TRUE),
                  Down = sum(log2FoldChange<0, na.rm = TRUE) )
    not.sig.label <- paste(c("Not Sig: "),
                             nrow(res.df)-sum(up.down),
                             collapse = "")
    up.down <- paste(c("    Up: ","\nDown: "),up.down,collapse = "")
    
    p.1 <- res.df %>% 
        ggplot(aes(x =log2FoldChange, y =neg.log.10.padj,
                   fill = Significance) ) +
        geom_point(shape = 21, col="white", size=3, alpha=0.7) + 
        theme_light(base_size = 12) + 
        theme(legend.position = "top") +
        scale_fill_manual(values=c("red", "blue"),
                          labels = c(up.down, not.sig.label) ) +
        labs(x="Log2FoldChange", y="- Log10 (padj-value)",
             title = title, 
             caption = "\nThreshold: padj <= 0.01 & Log2FoldChange >= 0.5") +
        geom_hline(yintercept= -log10(0.01),
                   linetype="dashed", color = "black") +
        geom_vline(xintercept=c(-log2(2^0.5),log2(2^0.5)),
                   linetype="dashed", color = "black")
        
    if (use.labels == TRUE) {
        p.1 = p.1 +
            geom_text_repel(data= res.df.sig,
            aes(label = res.df.sig$genes),
            box.padding = unit(0.8, "lines"),
            point.padding = unit(0.8, "lines") 
            )
    }
    return(p.1)
}
