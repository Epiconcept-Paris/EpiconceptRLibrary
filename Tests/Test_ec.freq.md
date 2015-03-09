# PACKAGE "EPICONCEPTS"


```r
library(Epiconcepts)
ec.use("Tiramitsu", extension="df");
```

## fonction ec.freq()

SYNTAXE :

  ec.freq(x [, by=y][, where=condition])

EXEMPLE :


```r
ec.freq("ill");
```

<!-- html table generated in R 3.1.2 by xtable 1.7-3 package -->
<!-- Mon Mar  9 13:58:24 2015 -->
<TABLE border=1>
<TR> <TH> ill </TH> <TH> Freq </TH>  </TR>
  <TR> <TD> 0 </TD> <TD align="right"> 188 </TD> </TR>
  <TR> <TD> 1 </TD> <TD align="right"> 103 </TD> </TR>
   </TABLE>

```r
ec.freq("ill", by="sex");
```

<!-- html table generated in R 3.1.2 by xtable 1.7-3 package -->
<!-- Mon Mar  9 13:58:24 2015 -->
<TABLE border=1>
<TR> <TH> ill </TH> <TH> females </TH> <TH> males </TH>  </TR>
  <TR> <TD> 0 </TD> <TD align="right">  86 </TD> <TD align="right"> 102 </TD> </TR>
  <TR> <TD> 1 </TD> <TD align="right">  53 </TD> <TD align="right">  50 </TD> </TR>
   </TABLE>
## On peut aussi poser une condition sur un ou plusieurs champs

```r
ec.freq("ill", where=c(VAL("tira")==1));
```

<!-- html table generated in R 3.1.2 by xtable 1.7-3 package -->
<!-- Mon Mar  9 13:58:24 2015 -->
<TABLE border=1>
<TR> <TH> ill </TH> <TH> Freq </TH>  </TR>
  <TR> <TD> 0 </TD> <TD align="right">  27 </TD> </TR>
  <TR> <TD> 1 </TD> <TD align="right">  94 </TD> </TR>
   </TABLE>

```r
ec.freq("ill", where=c(VAL("tira")==1 & VAL("beer") == 1));
```

<!-- html table generated in R 3.1.2 by xtable 1.7-3 package -->
<!-- Mon Mar  9 13:58:24 2015 -->
<TABLE border=1>
<TR> <TH> ill </TH> <TH> Freq </TH>  </TR>
  <TR> <TD> 0 </TD> <TD align="right">  14 </TD> </TR>
  <TR> <TD> 1 </TD> <TD align="right">  27 </TD> </TR>
   </TABLE>

```r
ec.freq("ill", where=c(VAL("tira")==1 & VAL("beer") == 0));
```

<!-- html table generated in R 3.1.2 by xtable 1.7-3 package -->
<!-- Mon Mar  9 13:58:24 2015 -->
<TABLE border=1>
<TR> <TH> ill </TH> <TH> Freq </TH>  </TR>
  <TR> <TD> 0 </TD> <TD align="right">  12 </TD> </TR>
  <TR> <TD> 1 </TD> <TD align="right">  63 </TD> </TR>
   </TABLE>

```r
ec.freq("tira", "beer", c(VAL("ill")==1));
```

<!-- html table generated in R 3.1.2 by xtable 1.7-3 package -->
<!-- Mon Mar  9 13:58:24 2015 -->
<TABLE border=1>
<TR> <TH> tira </TH> <TH> 0 </TH> <TH> 1 </TH>  </TR>
  <TR> <TD> 0 </TD> <TD align="right">   4 </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD> 1 </TD> <TD align="right">  63 </TD> <TD align="right">  27 </TD> </TR>
   </TABLE>

## Il est possible de tracer un histogramme de la fréquence.


```r
res <- ec.freq("ill");
ec.plot(res);
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
ec.plot(ec.freq("ill", by="sex"));
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png) 

