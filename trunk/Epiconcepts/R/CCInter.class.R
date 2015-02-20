library(methods)

setClass("CCInter",
         # ==== Inheritance
         # ==== Properties
         representation (
           varname        = "character",
           detail         = "logical",
           by             = "character",
           to             = "character",
           ExposeField   = "character",
           Colnames       = "character",
           TotalExposed   = "numeric",
           TotalUnexposed = "vector",
           CasesExposed   = "vector",
           CasesUnexposed = "vector",
           ARExposed      = "vector",
           ARUnexposed    = "vector",
           RiskRatio      = "vector",
           RiskCILow      = "vector",
           RiskCIHight    = "vector",
           Pvalue         = "vector",
           output         = "data.frame"
         )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "CCInter",
          function(.Object, x, exposure, by)
          {
            #.Object <-  .Object;
            
            if (length(exposure) < 1) {
              stop("Exposure is empty.");
            }
            
             .Col1Label = sprintf("CCInter %s / %s", x, exposure);
             .Object@Colnames = c( .Col1Label, "Cases", "Controls", "P.est.",
                                   "Statistics", "95%CI-L", "95%CI-H");

            label0 = sprintf("%s = Exposed", by);
            label1 = sprintf("%s = Unexposed", by);
            label2 = sprintf("Crude OR for %s", exposure);
            label3 = sprintf("MH OR %s adjusted for %s", exposure, by);  
            .Col1Values = c(label0, "Exposed", "Unexposed", "Total", "Exposed %", "",
                            label1, "Exposed", "Unexposed", "Total", "Exposed %", "",
                            "Number of obs", "Missing", "MH test of Homogeneity pvalue",
                             label2, label3, "Adjusted/crude relative change %")

            LABS_ESTIM <- c("Odds Ratio", "Attrib.risk.exp", "Attrib.risk.pop", "", "","");
            L_CASES     = vector();
            L_CONTROLS  = vector();
            L_ESTIMATE  = vector();
            L_STATS     = vector();
            L_CIL       = vector();
            L_CIH       = vector();
            NB_TOTAL    = 0;
            
            for (i in 1:0) {
              T = table(GDS[VAL(by)==i, exposure], GDS[VAL(by)==i, x])
              #print(T)
              L_CASES <- c(L_CASES, "", T[2,2], T[1,2]);
              TOTAL <-  T[2,2] + T[1,2];
              NB_TOTAL = NB_TOTAL + TOTAL;
              EXPOSED_PC <- sprintf("%3.0f", (T[2,2] / TOTAL) * 100);
              L_CASES <- c(L_CASES, TOTAL, EXPOSED_PC,"");
              
              # CONTROLS
              # ------------------------------------------------------------
              L_CONTROLS <- c(L_CONTROLS, "", T[2,1], T[1,1]);
              TOTAL <-  T[2,1] + T[1,1];
              NB_TOTAL = NB_TOTAL + TOTAL;
              EXPOSED_PC <- sprintf("%3.0f", (T[2,1] / TOTAL) * 100);
              L_CONTROLS <- c(L_CONTROLS, TOTAL, EXPOSED_PC,"");
              
              R = or(T); # ODDS RATIO ==============================
              #print(R);
              V_OR  = R[1]; # ODDS ratio
              V_CIL = R[2]; # Confidence interval low
              V_CIH = R[3]; # Confidence interval hight
              OR = sprintf("%3.2f", V_OR);
              CIL = sprintf("%3.2f", V_CIL);
              CIH = sprintf("%3.2f", V_CIH);
              L_STATS <- c(L_STATS, OR);
              L_CIL = c(L_CIL, CIL);
              L_CIH = c(L_CIH, CIH);

              R <- CC_AR(T);
              V_AR  = R[1]; # Attrib.risk.exp
              V_CIL = R[2]; # Confidence interval low
              V_CIH = R[3]; # Confidence interval hight
              AR = sprintf("%3.2f", V_AR);
              CIL = sprintf("%3.2f", V_CIL);
              CIH = sprintf("%3.2f", V_CIH);
              L_STATS <- c(L_STATS, AR);
              L_CIL = c(L_CIL, CIL, "", "", "", "");
              L_CIH = c(L_CIH, CIH, "", "", "", "");
              
              R <- CC_PAR(T);
              AFP = sprintf("%3.2f", R);
              L_STATS <- c(L_STATS, AFP, "", "", "");
              
            }
            
            T <- table(GDS[,x], GDS[,exposure], GDS[,by]);
            R <- CC_STATS(T);
            #print(R)

            # Number of obs
            # ------------------------------------------------------------
            L_CASES = c(L_CASES, NB_TOTAL);
            
            # MISSING
            # ------------------------------------------------------------
            MIS_TO = nrow(GDS) - NB_TOTAL;
            MIS_PC = sprintf("%3.2f%s", (MIS_TO / nrow(GDS))*100, '%');
            L_CASES = c(L_CASES, MIS_TO);

            # MH test of Homogeneity pvalue
            # ------------------------------------------------------------
            STAT = sprintf("%3.3f", R$OR.homog[3]);
            L_STATS <- c(L_STATS, "", "", STAT);
            #print(sprintf("MH test of Homogeneity pvalue : %s", STAT));
            
            # Crude OR for exposure
            # ------------------------------------------------------------
            STAT = sprintf("%3.2f", R$OR.crude[1]);
            CIL = sprintf("%3.2f", R$OR.crude[3]);
            CIH = sprintf("%3.2f", R$OR.crude[4]);
            L_STATS <- c(L_STATS, STAT);
            L_CIL = c(L_CIL, "", "", "", CIL);
            L_CIH = c(L_CIH, "", "", "", CIH);

            # MH OR for exposure adjusted for by
            # ------------------------------------------------------------
            STAT = sprintf("%3.2f", R$OR.mh[1]);
            CIL = sprintf("%3.2f", R$OR.mh[3]);
            CIH = sprintf("%3.2f", R$OR.mh[4]);
            L_STATS <- c(L_STATS, STAT);
            L_CIL = c(L_CIL, CIL);
            L_CIH = c(L_CIH, CIH);
            
            # Adjusted/crude relative change
            # ------------------------------------------------------------
            RC = 100 * ((R$OR.mh[1]-R$OR.crude[1])/R$OR.crude[1]);
            STAT = sprintf("%3.2f", RC);
            L_STATS <- c(L_STATS, STAT);
            
            
            COL2 = c(L_CASES, "", "", "", "");
            COL3 = c(L_CONTROLS, "", "", "", "", "", "");
            COL4 = c(LABS_ESTIM, LABS_ESTIM, "", "", "", "", "", "")
            COL5 = c(L_STATS);
            COL6 = c(L_CIL, "");
            COL7 = c(L_CIH, "");
 
            DF <- data.frame(cbind(.Col1Values));
            DF = cbind(DF, COL2, COL3, COL4, COL5, COL6, COL7);
            names(DF) <- .Object@Colnames;
            
            .Object@output <- DF;
            .Object;
          });

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"CCInter" ,
  function(object){
    align  =  c("l","r","c","c","r","r","r","r");
    ec.xtable(object@output, align=align);
  }
)

# -----------------------------------------------------------------------------
# function: CCInter (call real constructor)
# Return: an object of type CCInter
# -----------------------------------------------------------------------------
CCInter <- function(x, exposure="", by="")
{
  return(new("CCInter", x=x, exposure=exposure, by=by));
}
