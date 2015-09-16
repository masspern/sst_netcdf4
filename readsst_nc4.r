
#' readsst_nc4
#'
#' @description Use to be read nc sst file and convert in  R  raster and write it in GTiff format.
#'
#' @param  f   Character  Name of file in nc4 format
#' @return raster object
#' @author  Consorzio LaMMA  Massimo Perna \email{perna@@lamma.rete.toscana.it}
#' @keywords  DISIT,twitter vigilance
#' @example 
#' files <- list.files(pattern="nc")
#  res=lapply(files,FUN=readsst_nc4)
#'
#' @export

readsst_nc4 = function(f, write.file=TRUE )   {
                            require(ncdf4)
                            require(raster)
                            
                            x <- values(raster(f, varname= xyznames[1]))
                            y <- values(raster(f, varname= xyznames[2]))
                            z <- values(raster(f, varname = xyznames[3]))
                            sst <- data.frame(x,y,z)
                            sst <- na.omit(data.frame(apply(sst, 2, function(x) as.numeric(as.character(x)))))
                            xmn=min(sst[,1]); xmx=max(sst[,1])
                            ymn=min(sst[,2]); ymx=max(sst[,2])
                            r <- raster(nrows=2048, ncols=1080, 
                            xmn=xmn, xmx=xmx, 
                            ymn=ymn, ymx=ymx )
                            ras <- rasterize(sst[,1:2], r, field = sst[,3])
                            f1 <- sub("^([^.]*).*", "\\1", f) 
                            if ( write.file==TRUE) {
                                                   writeRaster(ras, filename=paste0(f1,".tif"), format="GTiff")
                                                   }
                            return(ras)
                            }




