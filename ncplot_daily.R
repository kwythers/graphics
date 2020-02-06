# Load some libraries
#library(rPython)
#library(ncdf4)
library(data.table)
library(plyr)
library(RNetCDF)
library(ggplot2)
library(grid)
library(gridExtra)

# set working directory, read in single file,and place variable names into "vars"
# use the "vars" variable to find what you are looking for
workdir <- '~/tmp/run/acme_t_n-_US-UMB_I20TRCLM45CN/run'
setwd(workdir)
ncfname <- 'acme_t_n-_US-UMB_I20TRCLM45CN.clm2.h0.1982-01-01-00000.nc'
ncf <- open.nc(ncfname)
vars <- as.list(read.nc(ncf))
ncf_data <- read.nc(ncfile = ncf)
#ncf_data$LEAF_MR
#ncf_data$XSMRPOOL
#ncf_data$GPP
#ncf_data$NPP

##### build set of datatables, once for each variable of interest

##### equation 1, create datatable from standard ACME maintenence respiration files
# open all files and initialize matrix

#[1-2]{1}[0-9]{3}(?<!2007)

files <- list.files(path = '~/tmp/run/acme_t_n-_US-UMB_I20TRCLM45CN/run', pattern = '*.h0.[1-2][0-9][0-9][0-9]-01-01-00000.nc', 
                    full.names = TRUE)

#files <- list.files(path = '~/tmp/us-umb/acme_t_n-/lnd/hist', pattern = '*US-UMB_I20TRCLM45CN.clm2.h0.*.nc', full.names = TRUE)

ar <- array(data = NA, c(length(files), 5, length(ncf_data$NPP))) # create an array of the proper length and fill it with NAs

#m <- matrix(NA, length(files), 5)

# loop over all files
for(i in 1:length(files)) {
  ncf <- open.nc(files[i]) # open netcdf 
  a <- var.get.nc(nc = ncf, "LEAF_MR")                  #gC/m^2/s
  b <- var.get.nc(nc = ncf, "MR")                       #gC/m^2
  c <- var.get.nc(nc = ncf, "GPP")                      #gC/m^2/s
  d <- var.get.nc(nc = ncf, "NPP")                      #gC/m^2/s
  t <- var.get.nc(nc = ncf, "mcdate")                   #yyyymmdd
  ar[i, , ] <- rbind(a, b, c, d, t)
  #if(i%%50==0){print(i)}            
  #print(m) # for debugging
  close.nc(ncf) # close netcdf after reading
}

ar2d <- Reduce('rbind', lapply(1:365, function(k) ar[, , k])) 

#t <- seq(0, 800, by = 1) # create time variable, step by 20
acme <- as.data.table(ar2d) # convert array to data table
#acme[, "V5"] <- t # overwrite "time" with t, and rename columns
acme <- rename(acme, c("V1"="LEAF_MR", "V2"="MR","V3"="GPP", "V4"="NPP", "V5"="DATE"))
acme <- acme[, rs_algorithm := "ACME"] # add a column for respiration algorithm
acme <- acme[order(DATE), ]

##### equation 2, create datatable from fixed base and Q10=2 respiration files
# open all files and initialize matrix
files <- list.files(path = '~/tmp/us-umb/fb_q_t_n-/lnd/hist', pattern = '*[1-2][0-9][0-9][0-9]-01-01-00000.nc', 
                    full.names = TRUE)

ar <- array(data = NA, c(length(files), 5, length(ncf_data$NPP))) # create an array of the proper length and fill it with NAs

#m <- matrix(NA, length(files), 5)

# loop over all files
for(i in 1:length(files)) {
  ncf <- open.nc(files[i]) # open netcdf 
  a <- var.get.nc(nc = ncf, "LEAF_MR") * 3600 * 24      #gC/m^2/s
  b <- var.get.nc(nc = ncf, "MR")                       #gC/m^2
  c <- var.get.nc(nc = ncf, "GPP") * 3600 * 24          #gC/m^2/s
  d <- var.get.nc(nc = ncf, "NPP") * 3600 *24           #gC/m^2/s
  t <- var.get.nc(nc = ncf, "mcdate")                   #yyyymmdd
  ar[i, , ] <- rbind(a, b, c, d, t)
  #if(i%%50==0){print(i)}            
  #print(m) # for debugging
  close.nc(ncf) # close netcdf after reading
}

ar2d <- Reduce('rbind', lapply(1:365, function(k) ar[, , k])) 

#t <- seq(0, 800, by = 1) # create time variable, step by 20
fb_q <- as.data.table(ar2d) # convert array to data table
#acme[, "V5"] <- t # overwrite "time" with t, and rename columns
fb_q <- rename(fb_q, c("V1"="LEAF_MR", "V2"="MR","V3"="GPP", "V4"="NPP", "V5"="DATE"))
fb_q <- fb_q[, rs_algorithm := "fb_q"] # add a column for respiration algorithm
fb_q <- fb_q[order(DATE), ]

##### equation 3, create datatable from variable base and Q10=2 maintenence respiration files
# open all files and initialize matrix
files <- list.files(path = '~/tmp/us-umb/vb_q_t_n-/lnd/hist', pattern = '*[1-2][0-9][0-9][0-9]-01-01-00000.nc', 
                    full.names = TRUE)

ar <- array(data = NA, c(length(files), 5, length(ncf_data$NPP))) # create an array of the proper length and fill it with NAs

#m <- matrix(NA, length(files), 5)

# loop over all files
for(i in 1:length(files)) {
  ncf <- open.nc(files[i]) # open netcdf 
  a <- var.get.nc(nc = ncf, "LEAF_MR") * 3600 * 24      #gC/m^2/s
  b <- var.get.nc(nc = ncf, "MR")                       #gC/m^2
  c <- var.get.nc(nc = ncf, "GPP") * 3600 * 24          #gC/m^2/s
  d <- var.get.nc(nc = ncf, "NPP") * 3600 *24           #gC/m^2/s
  t <- var.get.nc(nc = ncf, "mcdate")                   #yyyymmdd
  ar[i, , ] <- rbind(a, b, c, d, t)
  #if(i%%50==0){print(i)}            
  #print(m) # for debugging
  close.nc(ncf) # close netcdf after reading
}

ar2d <- Reduce('rbind', lapply(1:365, function(k) ar[, , k])) 

#t <- seq(0, 800, by = 1) # create time variable, step by 20
vb_q <- as.data.table(ar2d) # convert array to data table
#acme[, "V5"] <- t # overwrite "time" with t, and rename columns
vb_q <- rename(vb_q, c("V1"="LEAF_MR", "V2"="MR","V3"="GPP", "V4"="NPP", "V5"="DATE"))
vb_q <- vb_q[, rs_algorithm := "vb_q"] # add a column for respiration algorithm
vb_q <- vb_q[order(DATE), ]

##### equation 4, create datatable from variable base and exponential (variable Q10) maintenence respiration files
# open all files and initialize matrix
files <- list.files(path = '~/tmp/us-umb/vb_e_t_n-/lnd/hist', pattern = '*[1-2][0-9][0-9][0-9]-01-01-00000.nc', 
                    full.names = TRUE)

ar <- array(data = NA, c(length(files), 5, length(ncf_data$NPP))) # create an array of the proper length and fill it with NAs

#m <- matrix(NA, length(files), 5)

# loop over all files
for(i in 1:length(files)) {
  ncf <- open.nc(files[i]) # open netcdf 
  a <- var.get.nc(nc = ncf, "LEAF_MR") * 3600 * 24      #gC/m^2/s
  b <- var.get.nc(nc = ncf, "MR")                       #gC/m^2
  c <- var.get.nc(nc = ncf, "GPP") * 3600 * 24          #gC/m^2/s
  d <- var.get.nc(nc = ncf, "NPP") * 3600 *24           #gC/m^2/s
  t <- var.get.nc(nc = ncf, "mcdate")                   #yyyymmdd
  ar[i, , ] <- rbind(a, b, c, d, t)
  #if(i%%50==0){print(i)}            
  #print(m) # for debugging
  close.nc(ncf) # close netcdf after reading
}

ar2d <- Reduce('rbind', lapply(1:365, function(k) ar[, , k])) 

#t <- seq(0, 800, by = 1) # create time variable, step by 20
vb_e <- as.data.table(ar2d) # convert array to data table
#acme[, "V5"] <- t # overwrite "time" with t, and rename columns
vb_e <- rename(vb_e, c("V1"="LEAF_MR", "V2"="MR","V3"="GPP", "V4"="NPP", "V5"="DATE"))
vb_e <- vb_e[, rs_algorithm := "vb_e"] # add a column for respiration algorithm
vb_e <- vb_e[order(DATE), ]
### concatinate the data tables into single data source
l <- list(acme, fb_q, vb_q, vb_e)
dt <- rbindlist(l, use.names=TRUE)

### MAKE FANCY FIGURES

p <- ggplot(dt, aes(x = DATE, y = LEAF_MR, color = rs_algorithm)) + 
  geom_line() + theme(legend.position = "bottom") + 
  scale_color_manual(values=c("black", "red", "green", "blue"))

lmr <- ggplot(dt, aes(x = DATE, y = LEAF_MR, color = rs_algorithm)) + geom_line()
lmr <- lmr +  scale_x_continuous(name = '')                                                     # hide x-axis name
lmr <- lmr +  scale_y_continuous(expression("LMR (g C m"^-2~yr^-1*")"), limits = c(0, 400))     # set y-axis to be 0:400 and label
lmr <- lmr + theme(axis.text.x = element_blank(), axis.ticks = element_blank())                 # hide ledgend, x-axis tick marks
lmr <- lmr + scale_color_manual(values=c("black", "red", "green", "blue"))
                               
mr <- ggplot(dt, aes(x = DATE, y = MR, color = rs_algorithm)) + geom_line() 
mr <- mr + scale_x_continuous(name = '')
mr <- mr + scale_y_continuous(expression("MR (g C m"^-2~yr^-1*")"), limits = c(0, 400)) 
mr <- mr + theme(axis.text.x = element_blank(), axis.ticks = element_blank()) 
mr <- mr + scale_color_manual(values=c("black", "red", "green", "blue"))

gpp <- ggplot(dt, aes(x = DATE, y = GPP, color = rs_algorithm)) + geom_line()  
gpp <- gpp + scale_x_continuous(name = 'Years', breaks = c(100, 200, 300, 400, 500, 600, 700, 800), 
                     labels = c("100", "200", "300", "400", "500", "600", "700", "800"))        # set x-axis labels to every 100 years
gpp <- gpp + scale_y_continuous(expression("GPP (g C m"^-2~yr^-1*")")) 
gpp <- gpp + scale_color_manual(values=c("black", "red", "green", "blue"))

npp <- ggplot(dt, aes(x = DATE, y = NPP, color = rs_algorithm)) + geom_line()  
npp <- npp + scale_x_continuous(name = 'Years', breaks = c(100, 200, 300, 400, 500, 600, 700, 800), 
                      labels = c("100", "200", "300", "400", "500", "600", "700", "800"))       # set x-axis labels to every 100 years
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
