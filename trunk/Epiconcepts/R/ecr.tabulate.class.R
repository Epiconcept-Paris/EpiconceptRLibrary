library(methods)

setClass("ecr.tabulate",
         # ==== Inheritance
         # ==== Properties
         representation (
           varname      = "character",
           missing      = "logical",
           caption      = "character",
           tabulate     = "data.frame"
         )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ecr.tabulate",
          function(.Object, data, x, nolabel, missing, varname, caption)
          {

            .Object@varname <- varname;
            .Object@caption <- caption;
            
            getNames <- function(xfreq, nolabel)
            {
              if (nolabel == FALSE) {
                return(names(xfreq));
              }
              return(seq(0, length(names(xfreq))-1, by=1));
            }
            
            frequencies <- function(data, x, miss, namevar)
            {
              .MIS = sum(is.na(x));
              .OBS = nrow(data) - .MIS;
              
              if (miss == TRUE) {
                .OBS = .OBS + .MIS;
              }
              
              .freq = table(x);
              .rel.freq <- as.numeric(sprintf("%5.3f", (.freq / .OBS) * 100));
              .cum.rel.freq <- as.numeric(sprintf("%5.3f", cumsum(.rel.freq)));
              L0 <- c(namevar, "Freq.", "Percent", "Cumul.");
              C1 <- c(getNames(.freq, nolabel), "Total");
              C2 <- as.vector(.freq);
              C2 <- c(C2, sum(C2));
              C3 <- as.vector(.rel.freq);
              C3 <- c(C3, sum(C3));
              C4 <- as.vector(.cum.rel.freq);
              C4 <- c(C4, NA);
              DF = data.frame(cbind(C1, C2, C3, C4));
              names(DF) <- L0;
              DF;
            }
            
            .Object@tabulate <- frequencies(data, x, missing, varname)
            .Object;
          });


# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"ecr.tabulate" ,
          function(object) {
            #digits =  c(0,0,0,0,1,0,0,1,2,3,3,4);
            align  =  c("l","r","c","r","r");
            ec.xtable(object@tabulate, align = align)
          }
)

# -----------------------------------------------------------------------------
# function: ecr.tabulate (call real constructor)
# Return: an object of type ecr.tabulate
# -----------------------------------------------------------------------------
ecr.tabulate <- function(df, x, nolabel=FALSE, missing=TRUE, varname="X", caption="")
{
  return(new("ecr.tabulate", data=df, x=x, nolabel=nolabel, missing=missing, varname=varname, caption=caption));
}
