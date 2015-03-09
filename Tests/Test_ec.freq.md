# PACKAGE "EPICONCEPTS"


```r
library(Epiconcepts)
```

```
## Loading required package: ggplot2
## Loading required package: plyr
## Loading required package: fBasics
## Loading required package: MASS
## Loading required package: timeDate
## Loading required package: timeSeries
## 
## Attaching package: 'fBasics'
## 
## The following object is masked from 'package:base':
## 
##     norm
## 
## Loading required package: gridExtra
## Loading required package: grid
## Loading required package: jsonlite
## 
## Attaching package: 'jsonlite'
## 
## The following object is masked from 'package:utils':
## 
##     View
```

```r
ec.use("Tiramitsu", extension="df");
```

### fonction ec.freq()

SYNTAXE :

  ec.freq(x [, by=y][, where=condition])

EXEMPLE :


```r
ec.freq("ill");
```

<!-- html table generated in R 3.1.2 by xtable 1.7-3 package -->
<!-- Mon Mar  9 10:13:14 2015 -->
<TABLE border=1>
<TR> <TH> ill </TH> <TH> Freq </TH>  </TR>
  <TR> <TD> 0 </TD> <TD align="right"> 188 </TD> </TR>
  <TR> <TD> 1 </TD> <TD align="right"> 103 </TD> </TR>
   </TABLE>

```r
ec.freq("ill", by="sex");
```

<!-- html table generated in R 3.1.2 by xtable 1.7-3 package -->
<!-- Mon Mar  9 10:13:14 2015 -->
<TABLE border=1>
<TR> <TH> ill </TH> <TH> females </TH> <TH> males </TH>  </TR>
  <TR> <TD> 0 </TD> <TD align="right">  86 </TD> <TD align="right"> 102 </TD> </TR>
  <TR> <TD> 1 </TD> <TD align="right">  53 </TD> <TD align="right">  50 </TD> </TR>
   </TABLE>

Il est possible de tracer un histogramme de la fréquence.


```r
res <- ec.freq("ill");
ec.plot(res);
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
ec.plot(ec.freq("ill", by="sex"));
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-2.png) 

```r
tab = table(GDS$ill, GDS$sex)
print(xtable(tab), type="html")
```

```
## Error in print(xtable(tab), type = "html"): erreur d'évaluation de l'argument 'x' lors de la sélection d'une méthode pour la fonction 'print' : Erreur : impossible de trouver la fonction "xtable"
```

