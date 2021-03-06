# Load some libraries
#library(rPython)
#library(ncdf4)
library(data.table)
library(plyr)
library(RNetCDF)
library(ggplot2)
library(grid)
library(gridExtra)

########################################################################################
# set working directory, read in single file,and place variable names into "vars"
# use the "vars" variable to find what you are looking for
workdir <- '~/tmp/run/vb_e_t_n-_BR-Sa1_I20TRCLM45CN/run'
setwd(workdir)
ncfname <- 'vb_e_t_n-_BR-Sa1_I20TRCLM45CN.clm2.h0.2010-01-01-00000.nc'
ncf <- open.nc(ncfname)
vars <- as.list(read.nc(ncf))
ncf_data <- read.nc(ncfile = ncf)
#ncf_data$LEAF_MR
#ncf_data$XSMRPOOL
#ncf_data$GPP
#ncf_data$NPP

##### build set of datatables, once for each variable of interest

##### equation 1, create datatable from ve_e maintenence respiration files and 
##### standard temperature forcing 
# open all files and initialize matrix
files <- list.files(path = '~/tmp/run/vb_e_t_n-_BR-Sa1_I20TRCLM45CN/run', pattern = '*clm2.h0.*.nc', 
                    full.names = TRUE)

ar <- array(data = NA, c(length(files), 4, length(ncf_data$NPP))) # create an array of the proper length and fill it with NAs

# loop over all files
for(i in 1:length(files)) {
  ncf <- open.nc(files[i]) # open netcdf 
  a <- var.get.nc(nc = ncf, "LEAF_MR") * 3600 * 24 * 365     #gC/m^2/s
  b <- var.get.nc(nc = ncf, "MR") * 3600 * 24 * 365          #gC/m^2
  c <- var.get.nc(nc = ncf, "GPP") * 3600 * 24 * 365         #gC/m^2/s
  d <- var.get.nc(nc = ncf, "NPP") * 3600 *24 * 365          #gC/m^2/s
  ar[i, , ] <- rbind(a, b, c, d)
  #if(i%%50==0){print(i)}            
  #print(m) # for debugging
  close.nc(ncf) # close netcdf after reading
}

ar2d <- apply(ar, c(1, 2), sum)
t <- seq(1, 652, by = 1) # create yearly time variable, step by 1
ar2dt <- cbind(ar2d, t)

vb_e_t <- as.data.table(ar2dt) # convert array to data table
vb_e_t <- rename(vb_e_t, c("V1"="LEAF_MR", "V2"="MR","V3"="GPP", "V4"="NPP", "t"="YEAR"))
vb_e_t <- vb_e_t[, rs_algorithm := "vb_e_t"] # add a column for respiration algorithm

##### equation 2, create datatable from ve_e maintenence respiration files and 
##### -10°C temperature forcing
# open all files and initialize matrix
# open all files and initialize matrix
files <- list.files(path = '~/tmp/run/vb_e_t-_n-_BR-Sa1_I20TRCLM45CN_ad_spinup/run', pattern = '*clm2.h0.*.nc', 
                    full.names = TRUE)

ar <- array(data = NA, c(length(files), 4, length(ncf_data$NPP))) # create an array of the proper length and fill it with NAs

# loop over all files
for(i in 1:length(files)) {
  ncf <- open.nc(files[i]) # open netcdf 
  a <- var.get.nc(nc = ncf, "LEAF_MR") * 3600 * 24 * 365    #gC/m^2/s
  b <- var.get.nc(nc = ncf, "MR") * 3600 * 24 * 365         #gC/m^2
  c <- var.get.nc(nc = ncf, "GPP") * 3600 * 24 * 365        #gC/m^2/s
  d <- var.get.nc(nc = ncf, "NPP") * 3600 *24 * 365         #gC/m^2/s
  ar[i, , ] <- rbind(a, b, c, d)
  #if(i%%50==0){print(i)}            
  #print(m) # for debugging
  close.nc(ncf) # close netcdf after reading
}

ar2d <- apply(ar, c(1, 2), sum)
t <- seq(1, 310, by = 10) # create yearly time variable, step by 1
ar2dt <- cbind(ar2d, t)

vb_e_tminus10 <- as.data.table(ar2dt) # convert array to data table
vb_e_tminus10 <- rename(vb_e_tminus10, c("V1"="LEAF_MR", "V2"="MR","V3"="GPP", "V4"="NPP", "t"="YEAR"))
vb_e_tminus10 <- vb_e_tminus10[, rs_algorithm := "vb_e_tminus10"] # add a column for respiration algorithm


### MAKE FANCY FIGURES

### concatinate the data tables into single data source then draw figures
l <- list(acme, fb_q, vb_q, vb_e)
dt <- rbindlist(l, use.names=TRUE)

p <- ggplot(dt, aes(x = YEAR, y = LEAF_MR, color = rs_algorithm)) + 
  geom_line() + theme(legend.position = "bottom") + 
  scale_color_manual(values=c("black", "red", "green", "blue"))

#, limits = c(1990, 2005)

lmr <- ggplot(dt, aes(x = YEAR, y = LEAF_MR, color = rs_algorithm)) + geom_line()
lmr <- lmr +  scale_x_continuous(name = '')                                                     # hide x-axis name
lmr <- lmr +  scale_y_continuous(expression("LMR (g C m"^-2~yr^-1*")"))     # set y-axis to be 0:400 and label
lmr <- lmr + theme(axis.text.x = element_blank(), axis.ticks = element_blank())                 # hide ledgend, x-axis tick marks
lmr <- lmr + scale_color_manual(values=c("black", "red", "green", "blue"))

mr <- ggplot(dt, aes(x = YEAR, y = MR, color = rs_algorithm)) + geom_line() 
mr <- mr + scale_x_continuous(name = '')
mr <- mr + scale_y_continuous(expression("MR (g C m"^-2~yr^-1*")")) 
mr <- mr + theme(axis.text.x = element_blank(), axis.ticks = element_blank()) 
mr <- mr + scale_color_manual(values=c("black", "red", "green", "blue"))

gpp <- ggplot(dt, aes(x = YEAR, y = GPP, color = rs_algorithm)) + geom_line()  
gpp <- gpp + scale_x_continuous(name = 'Years')        # set x-axis labels to every 100 years
gpp <- gpp + scale_y_continuous(expression("GPP (g C m"^-2~yr^-1*")")) 
gpp <- gpp + scale_color_manual(values=c("black", "red", "green", "blue"))

npp <- ggplot(dt, aes(x = YEAR, y = NPP, color = rs_algorithm)) + geom_line()  
npp <- npp + scale_x_continuous(name = 'Years')       # set x-axis labels to every 100 years
npp <- npp + scale_y_continuous(expression("NPP (g C m"^-2~yr^-1*")")) 
npp <- npp + scale_color_manual(values=c("black", "red", "green", "blue"))

# create custom function to plot single ledgend for all pannels 
grid_arrange_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- grid_arrange_legend(p)

grid.arrange(arrangeGrob(lmr + theme_bw() + theme(legend.position = "none"), 
                         mr + theme_bw() + theme(legend.position = "none"),
                         gpp + theme_bw() + theme(legend.position = "none"), 
                         npp + theme_bw() + theme(legend.position = "none"),
                         legend, heights=c(0.45, 0.45, 0.1), nrow=3), 
             top = "Four Respiration Equations")

