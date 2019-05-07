#
#
#
#
volcano.plot.2 <- function(res.df, title="Volcano Plot", use.labels=FALSE) {
    # res.df is a regular df with at least the columns
    # log2FoldChange and padj 
    require(ggplot2);require(ggrepel)
    res.df %<>% as.data.frame() %>% rownames_to_column(var="genes")
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
    res.df.sig %<>%
        transmute(genes,log2FoldChange,padj,direction =
                   map_chr(log2FoldChange,~ifelse(.x>0,"up","down")))
    up.down <- table(res.df.sig$direction)

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
    return( list("plot" =p.1, "sig.genes" =res.df.sig) )
}

