

# icoplot() to make creating congruency plots easier
# to do: build in variables for data source, score vs log odds, faceting by study, language, or semantic domain, etc.

icoplot <- function(var=NULL,saveplot=FALSE) {
  
  if(is.null(var)) {
    var <- "language"
    print("No variable specified")
  } 
  print(paste("Plotting",var))
  
  # collect data
  
  # build plot
  p <- ggplot(data=dp, aes_string(x="study",y="score",fill=substitute(var),colour=substitute(var))) +
    theme_tufte() + ylim(0,1) + 
    theme(legend.position="none",axis.title.x=element_blank(),plot.margin=margin(0,0,10,0))
  # set colour scheme based on nature of variable
  if (var %in% c("C_cumulative","C_ternary","language","C_simple","C_nosound")) {
    p <- p +
      scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
      scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9)
  } else {
    p <- p +
      scale_colour_manual(values=c("#B6308B","#FCCE25")) +   scale_fill_manual(values=c("#B6308B","#FCCE25"))
  }
  p <- p +
    stat_summary(fun.y=median,geom="point",size=8,shape=21,stroke=1,fill="white") +
    geom_dotplot(colour="white",method="dotdensity",stackgroups=T,dotsize=1.5,binwidth=0.01,binaxis="y",stackdir = "center",na.rm=T)
  
  
  # save plot
  if (saveplot) {
    filename <- paste0("dotplot-",var,".png")
    print(paste("Saving as",filename))
    ggsave(file=paste0("figures//",filename,width=5,height=7.5))
  }
  
  # plot plot
  suppressWarnings(print(p))
  
}
