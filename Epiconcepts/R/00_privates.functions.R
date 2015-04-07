# return labels or indices of table names
# -------------------------------------------------------------------
p.getTableNames <- function(xfreq, nolabel)
{
  if (nolabel == FALSE) {
    return(names(xfreq));
  }
  return(seq(0, length(names(xfreq))-1, by=1));
}

# return stats for tablate functions ec/ecr.tabulate
# -------------------------------------------------------------------
p.frequencies <- function(tf, nbObs, nolabel, namevar)
{
  .freq = tf;
  .rel.freq <- as.numeric(sprintf("%5.3f", (.freq / nbObs) * 100));
  .cum.rel.freq <- as.numeric(sprintf("%5.3f", cumsum(.rel.freq)));
  L0 <- c(namevar, "Freq.", "Percent", "Cumul.");
  C1 <- c(p.getTableNames(.freq, nolabel), "Total");
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


# return quantiles of a numeric variable
# -------------------------------------------------------------------
p.Quantile <- function(x)
{
  C = class(x);
  if (C == "Date" | C == "Factor") {
    return(c("", "", "", ""));
  }
  
  Q <- quantile(x, na.rm=TRUE);
  return(as.vector(Q));
}

p.kurtosis <- function(x)
{
  if (class(x) == "Date") {
    return(NA);
  }
  
  kurtosis(x, method="moment", na.rm=TRUE);
}

p.skewness <- function(x)
{
  if (class(x) == "Date") {
    return("");
  }
  skewness(x, na.rm=TRUE, method="moment");
}


# return a simple summary of a variable within a data.frame
# -------------------------------------------------------------------
p.simpleSummary <- function(x, namevar)
{
  L0 = c("Variable", "Obs", "Mean", "Std. Dev.", "Min", "Max");
  .MIS = sum(is.na(x));
  .OBS = c(length(x) - .MIS);
  .MIN = c(min(x, na.rm=TRUE));
  .MAX = c(max(x, na.rm=TRUE));
  .MEA = c(mean(x, na.rm=TRUE));
  .STD = c(sd(x, na.rm=TRUE));
  L1 = c(namevar, .OBS, .MEA, .STD, .MIN, .MAX);
  DF <- data.frame(cbind(c(namevar)));
  DF <- cbind(DF, .OBS, .MEA, .STD, .MIN, .MAX);
  names(DF) <- L0;
  return(DF)
}

p.detailSummary <- function(x, namevar)
{
  L0 = c("Summary", namevar);
  L1 = c("Observations", "Missing", "Mean");
  L2 = c("Min.", "Q0.25", "Median", "Q0.75", "Max.");
  L3 = c("S.D", "Variance", "Skewless", "Kurtosis")
  Q = p.Quantile(x);
  .MIS = sum(is.na(x));
  .OBS = length(x) - .MIS;
  .MIN = min(x, na.rm=TRUE);
  .MAX = max(x, na.rm=TRUE);
  .MEA = mean(x, na.rm=TRUE);
  .MED = median(x, na.rm=TRUE);
  .Q25 = Q[2];
  .Q75 = Q[4];
  .STD = sd(x, na.rm=TRUE);
  .VAR = var(x, na.rm=TRUE);
  .SKW = p.skewness(x);
  .KUR = p.kurtosis(x);
  
  C1 = c(L1, L2, L3);
  C2 = c(.OBS, .MIS, .MEA, .MIN, .Q25, .MED, .Q75, .MAX, .STD, .VAR, .SKW, .KUR);
  print(C2)
  DF = data.frame(cbind(C1,C2));
  str(DF)
  names(DF) <- L0;
  return(DF);
}

p.factorSummary <- function(x, namevar)
{
  L0 = c("Factor", "Factor min.", "Factor max.");
  .min = min(levels(x));
  .max = max(levels(x));
  DF = df = as.data.frame(rbind(c(namevar, .min, .max)));
  names(DF) <- L0;
  return(DF);
}



