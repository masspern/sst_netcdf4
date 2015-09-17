
#' readsst_nc4
#'
#' @description Use to be read nc sst file and convert in  R  raster and write it in GTiff format. Please install rhdf5 
#' from BioClite Repository 
#'
#' @param  f   Character  Name of file in nc4 format
#' @return raster object
#' @author  Consorzio LaMMA  Massimo Perna \email{perna@@lamma.rete.toscana.it} Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  DISIT,twitter vigilance
#' @reference http://www.r-bloggers.com/working-with-hdf-files-in-r-example-pathfinder-sst-data/
#' @example 
#' files <- list.files(pattern="nc")
#  res=lapply(files,FUN=readsst_nc4)
#'
#' @export

readsst_nc4 = function(fname, write.file=TRUE )   {
                            require(rhdf5)
                            # source("http://bioconductor.org/biocLite.R")
                            # biocLite("rhdf5")
                            require(raster)
                            require(sp)
                            lon <- h5read(fname, "lon")
                            lat <- h5read(fname, "lat")
                            sst <- h5read(fname, "sea_surface_temperature")
                            sst=as.vector(sst)
                            sst <- replace(sst, sst == -32768, NaN)
                            sst <- (sst + 273.15) * 0.01
                            sst_points <- na.omit(data.frame(lon=as.numeric(lon),lat=as.numeric(lat),sst=as.numeric(sst)))
                            coordinates(sst_points) = ~ lon+lat
                            proj4string(sst_points) = CRS("+init=epsg:4326")
                            r <- raster(fname,varname="lat")
                            extent(r)=extent(sst_points)
                            ras <- rasterize(sst_points, r, field = "sst", fun='last', background=NA)
                            f1 <- sub("^([^.]*).*", "\\1", fname) 
                            if ( write.file==TRUE) {
                                                   writeRaster(ras, filename=paste0(f1,".tif"), format="GTiff")
                                                   }
                            return(ras)
                            }



