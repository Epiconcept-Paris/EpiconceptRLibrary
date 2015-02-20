library(methods)

setClass("ec.summary",
   # ==== Inheritance
   # ==== Properties
   representation (
     varname      = "character",
     detail       = "logical",
     type         = "character",
     by           = "character",
     to           = "character",
     summary      = "data.frame"
   )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ec.summary",
  function(.Object, x, detail=FALSE, ...)
  {
    
    .Object@detail = detail;
    
    factorSummary <- function()
    {
      L0 = c("Factor", "Factor min.", "Factor max.");
      .min = min(levels(GDS[,x]));
      .max = max(levels(GDS[,x]));
      DF = df = as.data.frame(rbind(c(x, .min, .max)));
      names(DF) <- L0;
      return(DF);
    }
    
    simpleSummary <- function()
    {
      L0 = c("Variable", "Obs", "Mean", "Std. Dev.", "Min", "Max");
      .MIS = sum(is.na(GDS[, x]));
      .OBS = c(nrow(GDS) - .MIS);
      .MIN = c(ec.min(x));
      .MAX = c(ec.max(x));
      .MEA = c(mean(GDS[, x], na.rm=TRUE));
      .STD = c(sd(GDS[, x], na.rm=TRUE));
      #L1 = c(x, .OBS, .MEA, .STD, .MIN, .MAX); 
      DF <- data.frame(cbind(c(x)));
      DF <- cbind(DF,
                  .OBS, .MEA, .STD, .MIN, .MAX);
      names(DF) <- L0;
      return(DF)
    }

    detailSummary <- function()
    {
      L0 = c("Summary", x);
      L1 = c("Observations", "Missing", "Mean");
      L2 = c("Min.", "Q0.25", "Median", "Q0.75", "Max.");
      L3 = c("S.D", "Variance", "Skewless", "Kurtosis")
      Q = gdsQuantile(x);
      .MIS = sum(is.na(GDS[, x]));
      .OBS = nrow(GDS) - .MIS;
      .MIN = min(GDS[, x], na.rm=TRUE);
      .MAX = max(GDS[, x], na.rm=TRUE);
      .MEA = ec.mean(x);
      .MED = median(GDS[, x], na.rm=TRUE);
      .Q25 = Q[2];
      .Q75 = Q[4];
      .STD = ec.sd(x);
      .VAR = ec.variance(x);
      .SKW = ec.skewness(x);
      .KUR = ec.kurtosis(x);
      
      C1 = c(L1, L2, L3);
      C2 = c(.OBS, .MIS, .MEA, .MIN, .Q25, .MED, .Q75, .MAX, .STD, .VAR, .SKW, .KUR);
      DF = data.frame(cbind(C1,C2));
      names(DF) <- L0;
      return(DF);
    }
    
    T <- sapply(GDS, class);
    .Object@type <- T[x];
    
    if (.Object@type == "factor") {
      .Object@summary <- factorSummary();
      return(.Object);
    }
    
    if (detail == FALSE) {
      .Object@summary <- simpleSummary();
    }
    else {
      .Object@summary <- detailSummary();
    }
    .Object;
  }
);

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"ec.summary" ,
  function(object) {
    if (object@type == "factor") {
      #digits =  c(0,0,0);
      align  =  c("l","r","c","c");
      df <- xtable(object@summary,  align=align);
      print(df, type = "html", include.rownames = F);
    }
    else {
      if (object@detail == TRUE) {
        digits =  c(0,0,5);
        align  =  c("l","r","c");
        df <- xtable(object@summary, digits=digits, align=align);
        print(df, type = "html", include.rownames = F);
      }
      else {
        #digits =  c(0,0,4,4,4,4,4);
        align  =  c("l","r","c","r","r","c","c");
        df <- xtable(object@summary, align=align);
        print(df, type = "html", include.rownames = F);
      }
    }
  }
)

# -----------------------------------------------------------------------------
# function: ec.summary (call real constructor)
# Return: an object of type ec.summary
# -----------------------------------------------------------------------------
ec.summary <- function(x, detail=FALSE)
{
  return(new("ec.summary", x=x, detail=detail));
}
