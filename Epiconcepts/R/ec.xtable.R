ec.xtable <- function(df, align=NULL, digits=NULL, caption=NULL) {
  require(xtable);
  output <- xtable(df, align=align, digits=digits, caption=caption);
  print(output, type = "html", include.rownames = F, comment = F);
}