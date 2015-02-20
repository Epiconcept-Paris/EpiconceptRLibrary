library(methods)

setClass("ec.freq",
         # ==== Inheritance
         contain = "EpiPlot",
         # ==== Properties
         representation (
           vardate      = "character",
           vartype      = "character",
           varcut       = "character",
           tableFreq    = "data.frame",
           EPC.MAXY     = "numeric"
         )           
);

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ec.freq",
  function(.Object, x, by=NULL, where=NULL, caption) {
    if (is.character(by)) {
      if (is.vector(where)) {
        R = data.frame(table(GDS[where, x], GDS[where, by]));
        #return(R)
        names(R) <- c(x, by, "Freq");
        .Object@tableFreq <- R;
        return(.Object);
      }
      R = data.frame(table(VAL(x), VAL(by)));
      #R = table(VAL(x), VAL(by));
      names(R) <- c(x, by, "Freq");
      .Object@tableFreq <- R;
      return(.Object);
    }
    if (is.vector(where)) {
      R = data.frame(table(GDS[where, x]));
      names(R) <- c(x, "Freq");
      .Object@tableFreq <- R;
      return(.Object);
    }
    R = data.frame(table(GDS[, x]));
    names(R) <- c(x, "Freq");
    .Object@tableFreq <- R;
    return(.Object);

  }
);
# =============================================================================

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"ec.freq" ,
  function(object) {
#digits =  c(0,0,0,0,1,0,0,1,2,3,3,4);
#align  =  c("l","r","c","c","r","c","c","r","r","r","r","r");
    df <- xtable(object@tableFreq);
    print(df, type = "html", include.rownames = F);
  }
);

setMethod ("ec.plot" , signature="ec.freq",
           function(this,
                    title    ="",
                    fillcolor="",
                    ylabel   ="",
                    xlabel   ="",
                    bgcolor  =""
           ) {
             DF <- this@tableFreq;
             varname = names(DF)[1];
             varpart = ifelse(ncol(DF) == 3, names(DF)[2], "") ;
             G_TITLE = ifelse(title != "", sprintf("%s\n",title), "");
             G_LABELY = ifelse(ylabel != "", ylabel, "Frequency");
             G_LABELX = ifelse(xlabel != "", xlabel, varname);
             G_FILLCOLOR = ifelse(fillcolor != "", fillcolor, "lightblue");
             T_BGCOLOR = ifelse(bgcolor == "", "white", bgcolor);
             THEME <- theme(panel.background = element_rect(fill = T_BGCOLOR)) +
               theme(axis.ticks.margin=unit(c(0.25,0.25),'line')) +
               theme(plot.margin = unit(c(0.5,0.5,0,0), "cm")) +
               theme(panel.margin = unit(c(0,0,0,0), "mm"))
             
             # -------------------------------------------------------------------------
             # Effective drawing
             # -------------------------------------------------------------------------    
             Draw <- function(O) {
               P_ <- ggplot(DF, aes_string(x = varname, y="Freq", ymax=50))
               P_ <- P_ + geom_bar(stat="identity", colour="black", fill=G_FILLCOLOR, na.action=na.exclude);
               if (G_TITLE != "_AUTO_") P_ <- P_ + ggtitle(G_TITLE);
               P_ <- P_ + xlab(G_LABELX);
               P_ <- P_ + ylab(G_LABELY);
               P_ <- P_ + scale_y_continuous(expand = c(0,0))
               P_ <- P_ + THEME;
               if (varpart != "") {
                 P_ <- P_ + facet_wrap(eval(parse(text = paste('~', varpart, sep=''))), ncol=2);
               }
               plot(P_);
             }
             Draw(this);
           });

# -----------------------------------------------------------------------------
# function: ec.freq (call real constructor)
# Return: an object of type AgePyramide
# -----------------------------------------------------------------------------
ec.freq <- function(x, by=NULL, where=NULL, caption)
{
  return(new("ec.freq", x=x, by=by, where=where, caption=caption));
}


         