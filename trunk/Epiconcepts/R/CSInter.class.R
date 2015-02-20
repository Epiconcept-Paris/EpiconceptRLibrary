library(methods)

setClass("CSInter",
         # ==== Inheritance
         # ==== Properties
         representation (
           varname        = "character",
           exposure       = "character",
           by             = "character",
           to             = "character",
           ExposeField    = "character",
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
           CSInter        = "data.frame"
         )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "CSInter",
          function(.Object, x, exposure, by)
          {
            .Object <-  .Object;
            
            if (length(exposure) < 1) {
              stop("Exposure is empty.");
            }
            
#             #PLabel = ifelse(exact == T, "p-value (Fisher)", "p-value (chi²)")
#             
#             .Object@ExposeField = exposure;
#             
             .Col1Label = sprintf("CSInter %s / %s", x, exposure);
#             
             .Object@Colnames = c( .Col1Label, "Total", "Cases", "Risk %", "P.est.",
                                   "Statistics", "95%CI-L", "95%CI-H");
#             
            label0 = sprintf("%s = Exposed", by);
            label1 = sprintf("%s = Unexposed", by);
            label2 = sprintf("Crude RR for %s", exposure);
            label3 = sprintf("MH RR %s adjusted for %s", exposure, by);  
             .Col1Values = c(label0, "Exposed", "Unexposed", "",
                             label1, "Exposed", "Unexposed", "",
                             "Missing", "MH test of Homogeneity Chi2 / pvalue",
                             label2, label3, "Adjusted/crude relative change")

            
            T1 = table(GDS[VAL(by)==1, exposure], GDS[VAL(by)==1, x])
            BY1_TE = T1[2,1]+T1[2,2];
            BY1_TU = T1[1,1]+T1[1,2];
            BY1_CE = T1[2,2];
            BY1_CU = T1[1,2];
            BY1_TO = BY1_TE + BY1_TU;

            BY1_RE = BY1_CE / BY1_TE;
            BY1_PRE = sprintf("%3.2f", BY1_RE*100);
            BY1_RU = BY1_CU / BY1_TU;
            BY1_PRU = sprintf("%3.2f", BY1_RU*100);
            
            # VBY1_RDF : Risk difference -------------------------------------
            VBY1_RDF = BY1_RE - BY1_RU;
            BY1_RDF  = sprintf("%3.2f", VBY1_RDF);
            CI = computeDiffRiskCI(BY1_RE, BY1_RU, BY1_TE, BY1_TU);
            BY1_RDCIL = sprintf("%3.2f", CI[1]);
            BY1_RDCIH = sprintf("%3.2f", CI[2]);
            
            # RR : Risk Ratio ------------------------------------------------
            RR = rr(T1);
            VBY1_RR    = RR[1];
            VBY1_RRCIL = RR[2];
            VBY1_RRCIH = RR[3];
            BY1_RR = sprintf("%3.2f", VBY1_RR);
            BY1_RRCIL = sprintf("%3.2f", VBY1_RRCIL);
            BY1_RRCIH = sprintf("%3.2f", VBY1_RRCIH);

            # ========== Attribuable / Preventive fraction exposed ==========
            if (BY1_RDF > 0) {
              # ARE : Attrib.risk.exp ---------------------------------------
              AFE = VBY1_RDF / BY1_RE;
              BY1_AFE = sprintf("%3.2f", AFE);
              BY1_AFECIL = sprintf("%3.2f", (VBY1_RRCIL - 1) / VBY1_RRCIL);
              BY1_AFECIH = sprintf("%3.2f", (VBY1_RRCIH - 1) / VBY1_RRCIH);
              
              # AFP ---------------------
              VAL_RT = (BY1_CE+BY1_CU)/(BY1_TO);
              VAL_AFP = (VAL_RT-BY1_RU)/VAL_RT;
              BY1_AFP = sprintf("%3.2f", VAL_AFP);
            } else {
              # ==========( Prev. frac. ex. )==========
              AFE = 1 - VBY1_RR;
              BY1_AFE = sprintf("%3.2f", AFE);
              BY1_AFECIL = sprintf("%3.2f", 1 - VBY1_RRCIH);
              BY1_AFECIH = sprintf("%3.2f", 1 - VBY1_RRCIL);
              # ==========( Prev. frac. pop )==========
#               VAL_RT = (BY1_CE+BY1_CU)/(BY1_TO);
#               VAL_AFP = (VAL_RT-BY1_RU)/VAL_RT;
#               BY1_AFP = sprintf("%3.2f", VAL_AFP);
              Pe = BY1_TE / (BY1_TE + BY1_TU);
              VAL_AFP = Pe * (1 - VBY1_RR);
              BY1_AFP = sprintf("%3.2f", VAL_AFP);
#               .Col1Values = c("Cases", "Non Cases", "Total", "Risk", "Risk difference", "Risk ratio",
#                               "Prev. frac. ex.", "Prev. frac. pop", "chi2(1)", "Pr>chi2")
            }

#             # ARE ---------------------
#             AFE = VBY1_RDF / BY1_RE;
#             BY1_AFE = sprintf("%3.2f", AFE);
#             BY1_AFECIL = sprintf("%3.2f", (VBY1_RRCIL - 1) / VBY1_RRCIL);
#             BY1_AFECIH = sprintf("%3.2f", (VBY1_RRCIH - 1) / VBY1_RRCIH);
# 
#             # AFP ---------------------
#             VAL_RT = (BY1_CE+BY1_CU)/(BY1_TO);
#             VAL_AFP = (VAL_RT-BY1_RU)/VAL_RT;
#             BY1_AFP = sprintf("%3.2f", VAL_AFP);
            
            # ==== by = 0
            T2 = table(GDS[VAL(by)==0, exposure], GDS[VAL(by)==0, x])
            BY0_TE = T2[2,1]+T2[2,2];
            BY0_TU = T2[1,1]+T2[1,2];
            BY0_CE = T2[2,2];
            BY0_CU = T2[1,2];
            BY0_TO = BY0_TE + BY0_TU;

            BY0_RE = BY0_CE / BY0_TE;
            BY0_PRE = sprintf("%3.2f", BY0_RE*100)
            BY0_RU = BY0_CU / BY0_TU;
            BY0_PRU = sprintf("%3.2f", BY0_RU*100)
            
            # RD ----------------------
            VBY0_RDF = BY0_RE - BY0_RU;
            BY0_RDF  = sprintf("%3.2f", VBY0_RDF);
            CI = computeDiffRiskCI(BY0_RE, BY0_RU, BY0_TE, BY0_TU);
            BY0_RDCIL = sprintf("%3.2f", CI[1]);
            BY0_RDCIH = sprintf("%3.2f", CI[2]);
            
            # RR ----------------------
            RR = rr(T2);
            VBY0_RR    = RR[1];
            VBY0_RRCIL = RR[2];
            VBY0_RRCIH = RR[3];
            BY0_RR = sprintf("%3.2f", VBY0_RR);
            BY0_RRCIL = sprintf("%3.2f", VBY0_RRCIL);
            BY0_RRCIH = sprintf("%3.2f", VBY0_RRCIH);

            # ARE ---------------------
            AFE = VBY0_RDF / BY0_RE;
            BY0_AFE = sprintf("%3.2f", AFE);
            BY0_AFECIL = sprintf("%3.2f", (VBY0_RRCIL - 1) / VBY0_RRCIL);
            BY0_AFECIH = sprintf("%3.2f", (VBY0_RRCIH - 1) / VBY0_RRCIH);
            
            # AFP ---------------------
            VAL_RT = (BY0_CE+BY0_CU)/(BY0_TO);
            VAL_AFP = (VAL_RT-BY0_RU)/VAL_RT;
            BY0_AFP = sprintf("%3.2f", VAL_AFP);
 
            # MISSING -----------------
            MIS_TO = nrow(GDS) - (BY1_TO + BY0_TO);
            MIS_PC = sprintf("%3.2f%s", (MIS_TO / nrow(GDS))*100, '%');
            
            # ==== Whitout by
            TG = table(GDS[, exposure], GDS[, x])
            C = rr(TG)
            V_RR = C[1];
            C_RR = sprintf("%3.2f", V_RR);
            C_RRCIL = sprintf("%3.2f", C[2]);
            C_RRCIH = sprintf("%3.2f", C[3]);
            

            R = CMHrr(T1, T2);
            V_MHRR = R[1]
            MHRR = sprintf("%3.2f", V_MHRR);
            CIL = sprintf("%3.2f", R[2]);
            CIH = sprintf("%3.2f", R[3]);
            RCHG = sprintf("%3.2f%%", 100 * (V_MHRR - V_RR) / V_RR);
            T = mht <- table(GDS[,x], GDS[,exposure], GDS[,by]);
            R = MH_HomogeneityTest(T);
            CHI2 = sprintf("%3.3f", R[1]);
            PVAL = sprintf("%3.3f", R[2]);

            COL2 = as.character(c(BY1_TO, BY1_TE, BY1_TU, "", BY0_TO, BY0_TE, BY0_TU, "", MIS_TO, CHI2, "", "", ""));
            COL3 = as.character(c(    "", BY1_CE, BY1_CU, "",     "", BY0_CE, BY0_CU, "", MIS_PC, PVAL, "", "", ""));
            COL4 = as.character(c(    "", BY1_PRE,BY1_PRU,"",     "", BY0_PRE,BY0_PRU,"",     "", "", "", "", ""));

            COL6 = as.character(c(BY1_RDF, BY1_RR,BY1_AFE, BY1_AFP, BY0_RDF, BY0_RR,BY0_AFE, BY0_AFP,     "", "", C_RR, MHRR, RCHG));

            COL7 = as.character(c(BY1_RDCIL, BY1_RRCIL, BY1_AFECIL,"", BY0_RDCIL, BY0_RRCIL, BY0_AFECIL,"", "", "",C_RRCIL, CIL, ""));
            COL8 = as.character(c(BY1_RDCIH, BY1_RRCIH, BY1_AFECIH,"", BY0_RDCIH, BY0_RRCIH, BY0_AFECIH,"", "", "",C_RRCIH, CIH, ""));
            
            COL5 = as.character(c("Risk difference", "Risk Ratio", "Attrib.risk.exp", "Attrib.risk.pop",
                                  "Risk difference", "Risk Ratio", "Attrib.risk.exp", "Attrib.risk.pop",
                                  "","","","",""));

            DF <- data.frame(cbind(.Col1Values));
            DF = cbind(DF, COL2, COL3, COL4, COL5, COL6, COL7, COL8);
            names(DF) <- .Object@Colnames;
            .Object@CSInter <- DF;
            .Object;
          });

# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"CSInter" ,
  function(object) {
    align  =  c("l","r","r","r","r","r","r","r","r");
    ec.xtable(object@CSInter, align=align);
  }
)

# -----------------------------------------------------------------------------
# function: CSInter (call real constructor)
# Return: an object of type CSInter
# -----------------------------------------------------------------------------
CSInter <- function(x, exposure="", by="")
{
  return(new("CSInter", x=x, exposure=exposure, by=by));
}
