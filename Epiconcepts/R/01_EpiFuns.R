# =============================================================================
# Utilities for Epiconcept
# =============================================================================
require(ggplot2)
require(plyr)
require(grid)
require(gridExtra)
require(xtable);
library(methods)
require(boot)
require(jsonlite)
library(fBasics)

setGeneric("Plot", function(this, ...) {
  return(standardGeneric("Plot"))
})

setGeneric("ec.plot", function(this, ...) {
  return(standardGeneric("ec.plot"))
})

setGeneric("Close", function(this) {
  return(standardGeneric("Close"))
})

ec.use <- function(df = "RAW", extension="csv", header=TRUE, sep=";", encoding="utf8", colClasses=NA) 
{
  if (extension == 'df') {
    data(list=c(as.name(df)), envir=.GlobalEnv);
    assign('GDS', eval(parse(text=df)), envir=.GlobalEnv);
    rm(list=c(df), envir=.GlobalEnv);
    x <- gc();
  }
  else {
    rds = paste(df,"rds", sep=".");
    csv = paste(df,extension, sep=".");
    
    if (file.exists(rds)) {
      #GDS <<- readRDS(rds);
      assign('GDS', readRDS(rds), envir=.GlobalEnv);
    }
    
    else if (file.exists(csv)) {
      assign('GDS', read.table(csv, header=header,
                               na.strings = "", sep=sep, encoding=encoding,
                               colClasses=colClasses),
             envir=.GlobalEnv);
      ec.save("RAW");
    }
  }
}


VAL <- function(varname)
{
  #GDS = get("GDS", envir=.GlobalEnv);
  return(GDS[,varname]);
}

freq <- function(x, by=NULL, where=NULL)
{
  if (is.character(by)) {
    if (is.vector(where)) {
      #R = data.frame(table(GDS[where, x], GDS[where, by]));
      R = table(GDS[where, x], GDS[where, by]);
      return(R)
      names(R) <- c(x, by, "Freq");
      return(R);
    }
    #R = data.frame(table(VAL(x), VAL(by)));
    R = table(VAL(x), VAL(by));
    #names(R) <- c(x, by, "Freq");
    return(R);
  }
  if (is.vector(where)) {
    R = data.frame(table(GDS[where, x]));
    names(R) <- c(x, "Freq");
    return(R);
  }
  R = data.frame(table(GDS[, x]));
  names(R) <- c(x, "Freq");
  return(R);
}

ec.max <- function(x)
{
  C = class(GDS[, x]);
  if (C == "factor") {
    return(c("", "", "", ""));
  }

  return(max(GDS[, x], na.rm=TRUE));
}

ec.min <- function(x)
{
  C = class(GDS[, x]);
  if (C == "factor") {
    return(c("", "", "", ""));
  }

  return(min(GDS[, x], na.rm=TRUE));
}

ec.mean <- function(x)
{
  return(sprintf("%5.5f", mean(GDS[, x], na.rm=TRUE)));
}

ec.median <- function(x)
{
  return(sprintf("%5.5f",median(GDS[, x], na.rm=TRUE)));
}

# ec.kurtosis <- function(x)
# {
#   if (class(GDS[, x]) == "Date") {
#     return("");
#   }
# 
#   K <- kurtosis(GDS[, x], method="moment", na.rm=TRUE);
#   return(sprintf("%5.5f", K[1]));
# }
# 
# ec.sd <- function(x)
# {
#   SD = sd(GDS[, x], na.rm=TRUE)
#   return(sprintf("%5.5f", SD));
# }
# 
# ec.skewness <- function(x)
# {
#   if (class(GDS[, x]) == "Date") {
#     return("");
#   }
#   S <- skewness(GDS[, x], na.rm=TRUE, method="moment");
#   return(sprintf("%5.5f", S[1]));
# }
# 
# ec.variance <- function(x)
# {
#   return(sprintf("%5.5f", var(GDS[, x], na.rm=TRUE)));
# }

ec.proportion <- function(c)
{
#  library(boot);
  set.seed(123)
  x<-GDS[,c]
  N_ = nrow(GDS);
  
  myprop<-function(x,i){
    sum(x[i]==0)/length(x)
  }
  
  B <- boot(x, myprop, 100);
  SD_ = sprintf("%5.6f", sd(B$t));
  T = table(x);
  NAMES_ = names(T)
  M1_ = T[1];
  M2_ = T[2];
  NDF = c(c, "Obs.","Proportion", "Std. Err.", "95% CI.Low", "95% CI.Low");

  P = prop.test(M1_, N_, conf.level=0.95);
  PE1_ = P$estimate;
  CI1_LOW = P$conf.int[1];
  CI1_HIG = P$conf.int[2];
  
  P = prop.test(M2_, N_, conf.level=0.95);
  PE2_ = P$estimate;
  CI2_LOW = P$conf.int[1];
  CI2_HIG = P$conf.int[2];
  
  R1 = c(NAMES_[1], M1_, PE1_, SD_, CI1_LOW, CI1_HIG);
  R2 = c(NAMES_[2], M2_, PE2_, SD_, CI2_LOW, CI2_HIG);
  
#  DF = data.frame(rbind("1"=R1, "2"=R2));
  DF = data.frame(cbind(NAMES_));
  DF = cbind(DF, as.numeric(c(M1_,M2_)));
  DF = cbind(DF, as.numeric(c(PE1_,PE2_)));
  DF = cbind(DF, as.numeric(c(SD_,SD_)));
  DF = cbind(DF, as.numeric(c(CI1_LOW,CI2_LOW)));
  DF = cbind(DF, as.numeric(c(CI1_HIG,CI2_HIG)));
  names(DF) <- NDF;
  digits =  c(0,0,0,4,4,5,5);
  align  =  c("l","c","c","r","r","r","r");
  odf <- xtable(DF, digits=digits, align=align);
  print(odf, type = "html", include.rownames = F);
}



computeRiskCI <- function(risk, X1, N1, X2, N2)
{
  A = ((N1-X1)/X1)/N1;
  B = ((N2-X2)/X2)/N2;
  R1 = log(risk) + (1.96*sqrt(A + B));
  R2 = log(risk) - (1.96*sqrt(A + B));
  E1 = exp(R1);
  E2 = exp(R2);
  
  return(c(E2, E1));
}

rr <- function(Tb)
{
  TE = Tb[2,1]+Tb[2,2];
  TU = Tb[1,1]+Tb[1,2];
  CE = Tb[2,2];
  CU = Tb[1,2];
  TO = TE + TU;
  
  RE = CE / TE;
  RU = CU / TU;
  
  RR  = RE/RU;
  CI = computeRiskCI(RR, CE, TE, CU, TU);
  RRCIL = CI[1];
  RRCIH = CI[2];
  
  return(c(RR, RRCIL, RRCIH))
}

# ========================================================================
# CASES CONTROLS STUDY
# ========================================================================
# Comppute ODDS ratio
# -------------------
or <- function(T)
{
  O <- (T[2,2]/T[2,1]) / (T[1,2]/T[1,1]);
  x <- matrix(T, 2, byrow = TRUE);
  R <- fisher.test(x);
  CIL <- R$conf.int[1];
  CIH <- R$conf.int[2];
  return(c(O, CIL, CIH));
}

CC_AR <- function(C)
{
  T = epi.2by2(dat=C, method="case.control");
  S <- summary(T);
  return(c(S$AFest[1], S$AFest[2], S$AFest[3]));
}

CC_PAR <- function(C)
{
  T = epi.2by2(dat=C, method="case.control", outcome="as.columns");
  S <- summary(T);
  return(S$AFp[1]);
}

CC_STATS <- function(C)
{
  T = epi.2by2(dat=C, method="case.control", outcome="as.columns", homogeneity="woolf");
  S <- summary(T);
  return(S);
}

computeOddsRatioCI <- function(O, a,b,c,d)
{
  LNO = log(O);
  R1 = sqrt((1/a)+(1/b)+(1/c)+(1/d));
  CIL = exp(LNO - 1.96 * R1);
  CIH = exp(LNO + 1.96 * R1);
  return(c(CIL, CIH));
}

computeExactORCI <- function(a,b,c,d)
{
  x <- matrix(c(a, b, c, d), 2, byrow = TRUE);
  R <- fisher.test(x);
  CIL <- R$conf.int[1];
  CIH <- R$conf.int[2];
  return(c(CIL, CIH, R$p.value));
}

computeDiffRiskCI <- function(RE, RU, NE, NU)
{
  A = RE - RU;
  B = (RE * (1-RE))/NE;
  C = (RU * (1-RU))/NU;
  D = 1.96*sqrt(B + C);
  R1 = A + D;
  R2 = A - D;
  
  return(c(R2, R1));
}

CMHrr <- function(A, B)
{
  # Stratum 1 =====================
  Ce1 = A[2,2]    ; # Cases exposed
  Cu1 = A[1,2]    ; # Cases unexposed
  He1 = A[2,1]    ; # Healthy exposed
  Hu1 = A[1,1]    ; # Healthy unexposed

  Te1 = Ce1 + He1 ; # Total exposed
  Tu1 = Cu1 + Hu1 ; # Total unexposed
  T1  = Te1 + Tu1 ; # Total strate 1
  H1  = Hu1 + He1 ; # Total healthy
  C1  = Cu1 + Ce1 ; # Total cases
  
  # Stratum 2 =====================
  Ce2 = B[2,2]    ; # Cases exposed
  Cu2 = B[1,2]    ; # Cases unexposed
  He2 = B[2,1]    ; # Healthy exposed
  Hu2 = B[1,1]    ; # Healthy unexposed

  Te2 = Ce2 + He2 ; # Total exposed
  Tu2 = Cu2 + Hu2 ; # Total unexposed
  T2  = Te2 + Tu2 ; # Total strate 2
  H2  = Hu2 + He2 ; # Total healthy
  C2  = Cu2 + Ce2 ; # Total cases
  
  

  R1 = ((Ce1 * Tu1) / T1) + ((Ce2 * Tu2) / T2);
  R2 = ((Cu1 * Te1) / T1) + ((Cu2 * Te2) / T2);
  rrmh = R1 / R2;

  R3 = ((C1*Te1*Tu1) - (Ce1*Cu1*T1)) / T1^2;
  R4 = ((C2*Te2*Tu2) - (Ce2*Cu2*T2)) / T2^2; 
  R5 = R3 + R4;
  R6 = R5 / (R1 * R2);
  R7 = sqrt(R6);
  
  L = log(rrmh) - (1.96 * R7);
  H = log(rrmh) + (1.96 * R7);
  
  CIL = exp(L);
  CIH = exp(H);
  
  return(c(rrmh, CIL, CIH));
}

MH_HomogeneityTest <- function(mht)
{
  T = epi.2by2(dat=mht, homogeneity="breslow.day");
  S <- summary(T);
  return(c(S$RR.homog[1], S$RR.homog[3]));
}

computeKHI2 <- function(A, B, C, D)
{
  t <- chisq.test(matrix(c(A,B,C,D),ncol=2), correct=FALSE);
  return(c(t$statistic, t$p.value));
}




# =============================================================================
# Age functions
# -----------------------------------------------------------------------------

.getAge <- function(X, Y, unit="year")
{
  if (unit == "year") {
    return(as.integer(floor((Y - X) / 365.25)));
  }
  if (unit == "month") {
    return(as.integer(floor((Y - X) / 30.437)));
  }
  if (unit == "week") {
    return(as.integer(floor((Y - X) / 7)));
  }
  if (unit == "day") {
    return (as.integer(Y - X));
  }
  stop("Bad period for addAgeVar!")
}

gdsAddAgeVar <- function(varname, birthdate, onsetdate, unit="year")
{
  X <- as.Date(GDS[,birthdate]);
  if (onsetdate %in% colnames(GDS)) {
    Y = as.Date(GDS[,onsetdate]);
  }
  else {
    Y = as.Date(onsetdate)
  }
  X_AGE <- .getAge(X, Y, unit);
  GDS <<- cbind(GDS, X_AGE);
  gdsRenameCol("X_AGE", varname);
  
}

# -----------------------------------------------------------------------------
createAgeScale  <- function(x, lower = 0, upper=100, by = 10, sep = "-", above.char = "+") {
  
  if (by == 1) {
    labs <- c(seq(lower, upper, by = by))
  }
  else {
    labs <- c(paste(seq(lower, upper - by, by = by),
                    seq(lower + by - 1, upper - 1, by = by),
                    sep = sep),
              paste(upper, above.char, sep = ""))
  }
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}

toFactor <- function(column, cut=3)
{
  R <- cut(GDS[,column], breaks = cut);
  cmd = sprintf("transform(GDS, %s = R)", column);
  return (eval(parse(text = cmd)));
}


printHTML <- function(x, ..., digits = 0, include.rownames = TRUE, 
                       header=names(x)){
  # print html table with customized header, 
  # without permanently changing column names of the dataframe
  origHeader <- names(x)
  names(x) <- header
  print(xtable(x, digits = digits, ...), 
        type = 'html', include.rownames = include.rownames)
  names(x) <- origHeader
}

# -----------------------------------------------------------------------------
deviceOn <- function(name, format="png", width=700, heigth=0)
{
  W = width / 90;
  print(W);
  fname <- paste(name, format, sep=".");
  if (format == "svg") {
    svg(filename=fname, width=W);
  }
  else if (format == "png") {
    png(filename=fname, width=W);
  }
  else if (format == "jpeg") {
    jpeg(filename=fname, width=W);
  }
  else if (format == "pdf") {
    pdf(filename=fname, width=W);
  }
}


getDataset <- function(csvname="DATA", extension="csv")
{
  rda = paste(csvname,"Rdata", sep=".");
  csv = paste(csvname,extension, sep=".");
  
  if (file.exists(rda)) {
    GDS <<- readRDS(rda);
    #attach(GDS);
    return(TRUE);
  }
  
  if (file.exists(csv)) {
    GDS <<- read.csv(csv, na.strings = "");
    #attach(GDS);
    return(TRUE);
  }
  
  msg <- sprintf("File [%s] not found in current directory!", csv);
  stop(msg);
}



# x <- matrix(c(158, 27, 7,94), 2, byrow = TRUE)
#epi.2by2(dat=mht1, method="cohort.count", conf.level = 0.95, units = 100, homogeneity = "breslow.day",outcome = "as.columns")
#mhor(GDS$ill, GDS$wmousse, GDS$tira)
