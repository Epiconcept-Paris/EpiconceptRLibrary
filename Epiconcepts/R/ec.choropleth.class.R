library(methods)
library(ggplot2);
library(plyr)
library(maptools)
#library(rgeos)
#library(gpclib)
library(sp);

setClass("ec.choropleth",
  # ==== Inheritance
  # ==== Properties
  representation (
    varname        = "character",
    scaleColors = "vector",
    gadmFile    = "character",
    geoDF       = "data.frame",
    GADM        = "SpatialPolygonsDataFrame",
    geoData     = "data.frame"
  )           
)

# ------------------------------------------------------------------------------
# Real constructor
# ------------------------------------------------------------------------------
setMethod("initialize", "ec.choropleth",
  function(.Object, x, country, level, names, palette)
  {
    # LOAD MAP DATA
    #load("TCD_adm1.RData");
    #.Object@GADM <-gadm;
    #gadm;
    .Object;
  });
          
# -----------------------------------------------------------------------------
# method show
# -----------------------------------------------------------------------------
setMethod("show" ,"ec.choropleth" ,
  function(this){
    plot(this@GADM);
    ggplot() + geom_map(data = DAT, aes(map_id = REG, fill = POP2), map = SDF) + expand_limits(x = SDF$long, y = SDF$lat) + 
      geom_text(data = distcenters, aes(x = clong, y = clat, label = id, size = 0.2));
    
    #    align  =  c("l","r","c","c","r","r","r","r");
#    ec.xtable(object@output, align=align);
  }
)

# -----------------------------------------------------------------------------
# function: ec.choropleth (call real constructor)
# Return: an object of type ec.choropleth
# -----------------------------------------------------------------------------
ec.choropleth <- function(x, country, level, name )
{
  return(new("ec.choropleth", x=x, country=country, level=level, name=name));
}

ec.choropleth()
