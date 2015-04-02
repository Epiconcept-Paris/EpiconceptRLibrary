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
#   .MIS = sum(is.na(x));
#   .OBS = nrow(data) - .MIS;
#   
#   if (miss == TRUE) {
#     .OBS = .OBS + .MIS;
#   }
  
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

