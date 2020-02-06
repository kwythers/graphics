# Load some libraries
#library(rPython)
#library(RNetCDF)
library(ncdf4)
library(data.table)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(zoo)
library(scales)

##### Read in csv data from fluxnet directory on google drive

csvfiles <- list.files(path = '~/Google Drive/globland/data/fluxnet/AMF_US-UMB_BASE-BADM_10-1', pattern = 'AMF_USUMB_[2][0][1][0-4]_L2_GF_V010.csv', 
                        full.names = TRUE)
temp <- lapply(csvfiles, fread, skip = 20)
data <- rbindlist(temp)
cols = paste("V", c(1, 4, 39), sep="")
tgpp <- data[, cols, with = F]
tgpp[tgpp == -9999] <- NA
tgpp <- rename(tgpp, c("V1"="YEAR","V4"="DOY", "V39"="GPP.umols"))
tgpp[,`:=`(GPP.grams = GPP.umols * 3600 * 12 * 1E-6)] # convert umols CO2m^-2s^-1 to g C^-2s^-1
key <- c("YEAR", "DOY")
daily.tgpp <- tgpp[ , lapply(.SD, sum), by = key] # aggrate 24 hourly gpp values to a daily sum




##### Read in netcdf data from ACME
# set working directory, read in single file,and place variable names into "vars"
# use the "vars" variable to find what you are looking for
workdir <- '~/tmp/run/acme_t_n-_US-UMB_I20TRCLM45CN/run'
setwd(workdir)
ncfname <- 'acme_t_n-_US-UMB_I20TRCLM45CN.clm2.h0.2000-01-01-00000.nc'
#vars <- as.list(read.nc(ncf))
#ncf_data <- read.nc(ncfile = ncf)

# read individual netcdf variables into arrays
nc <- nc_open(ncfname)

lmr <- ncvar_get(nc, "LEAF_MR")
mr <- ncvar_get(nc, "MR")
gpp <- ncvar_get(nc, "GPP")
npp <- ncvar_get(nc, "NPP")
mcdate <- ncvar_get(nc, "mcdate") #ncdf4

nc_close(ncfname)

##### build set of datatables, once for each variable of interest

##### equation 1, create datatable from standard ACME maintenence respiration files
# open all files and initialize matrix

ncfiles <- list.files(path = '~/tmp/run/acme_t_n-_US-UMB_I20TRCLM45CN/run', pattern = '*.clm2.h0.[2][0][1][0-4]-01-01-00000.nc', 
                    full.names = TRUE)

ar <- array(data = NA, c(length(ncfiles), 5, length(gpp))) # create an array of the proper dimensions and fill it with NAs

# loop over all files
for(i in 1:length(ncfiles)) {
  nc <- nc_open(ncfiles[i])
  a <- ncvar_get(nc, "mcdate")
  b <- ncvar_get(nc, "LEAF_MR") * 3600 * 24
  c <- ncvar_get(nc, "MR") * 3600 * 24
  d <- ncvar_get(nc, "GPP") * 3600 * 24
  e <- ncvar_get(nc, "NPP") * 3600 * 24
  ar[i, , ] <- rbind(a, b, c, d, e)
  # print(ar) # for debugging
  # print(i)
  nc_close(nc)  
}

ar2d <- (Reduce('rbind', lapply(1:365, function(k) ar[, , k]))) 

#t <- seq(0, 800, by = 20) # create time variable, step by 20
acme <- as.data.table(ar2d) # convert matrix to data table
#acme[, "V5"] <- t # overwrite "time" with t, and rename columns
acme <- rename(acme, c("V1"="DATE", "V2"="LEAF_MR","V3"="MR", "V4"="GPP", "V5"="NPP"))
acme <- acme[, rs_algorithm := "ACME"] # add a column for respiration algorithm
acme <- acme[order(DATE)] # order data by date
acme$DATE <- as.Date(as.character(acme$DATE), "%Y%m%d")


# sum(acme$DATE == 20150101) # looks like 365 obs for 2015, removed odd 20150101 netcdf from file list
# sum(acme$DATE == 20140101) # only 1 obs for everything else


##### equation 2, create datatable from variable base and exponential (variable Q10) maintenence respiration files
# open all files and initialize matrix

ncfiles <- list.ncfiles(path = '~/tmp/run/vb_e_t_n-_US-UMB_I20TRCLM45CN/run', pattern = '*.clm2.h0.[2][0][1][0-4]-01-01-00000.nc', 
                    full.names = TRUE)

ar <- array(data = NA, c(length(ncfiles), 5, length(gpp))) # create an array of the proper dimensions and fill it with NAs

# loop over all files
for(i in 1:length(ncfiles)) {
  nc <- nc_open(ncfiles[i])
  a <- ncvar_get(nc, "mcdate")
  b <- ncvar_get(nc, "LEAF_MR") * 3600 * 24
  c <- ncvar_get(nc, "MR") * 3600 * 24
  d <- ncvar_get(nc, "GPP") * 3600 * 24
  e <- ncvar_get(nc, "NPP") * 3600 * 24 
  ar[i, , ] <- rbind(a, b, c, d, e)
  # print(ar) # for debugging
  # print(i)
  nc_close(nc)  
}

ar2d <- (Reduce('rbind', lapply(1:365, function(k) ar[, , k]))) 
# ar2dtest <- (Reduce('rbind', lapply(1:165, function(k) ar[k, , ])))

#t <- seq(0, 800, by = 20) # create time variable, step by 20
vb_e <- as.data.table(ar2d) # convert matrix to data table
#vb_e[, "V5"] <- t # overwrite "time" with t, and rename columns
vb_e <- rename(vb_e, c("V1"="DATE", "V2"="LEAF_MR","V3"="MR", "V4"="GPP", "V5"="NPP"))
vb_e <- vb_e[, rs_algorithm := "vb_e"] # add a column for respiration algorithm
vb_e <- vb_e[order(DATE)] # order data by date
vb_e$DATE <- as.Date(as.character(vb_e$DATE), "%Y%m%d")

### concatinate the data tables into single data source

l <- list(acme, vb_e)
dt <- rbindlist(l, use.names=TRUE)

### MAKE FANCY FIGURES

p <- ggplot(dt, aes(x = DATE, y = LEAF_MR, color = rs_algorithm)) + 
  geom_line() + theme(legend.position = "bottom") + 
  scale_color_manual(values=c("black", "green"))

lmr <- ggplot(dt, aes(x = DATE, y = LEAF_MR, color = rs_algorithm)) + geom_line()
lmr <- lmr + scale_x_date(labels = date_format("%Y"), name = '', limits = as.Date(c("2010-01-01","2014-12-31")))#, limits = as.Date(c("2000-01-01","2012-01-01"))) 
#lmr <- lmr +  scale_x_continuous(name = '')    #limits = c(2010, 2012),                                                  # hide x-axis name
lmr <- lmr +  scale_y_continuous(expression("LMR (g C m"^-2~d^-1*")"), limits = c(0, 5))     # set y-axis to be 0:400 and label
lmr <- lmr + theme(axis.text.x = element_blank(), axis.ticks = element_blank())                 # hide ledgend, x-axis tick marks
lmr <- lmr + scale_color_manual(values=c("black", "green"))
                               
mr <- ggplot(dt, aes(x = DATE, y = MR, color = rs_algorithm)) + geom_line()
mr <- mr  + scale_x_date(labels = date_format("%Y"), name = '', limits = as.Date(c("2010-01-01","2014-12-31")))#, limits = as.Date(c("2000-01-01","2012-01-01"))) 
#mr <- mr + scale_x_continuous(name = '') 
mr <- mr + scale_y_continuous(expression("MR (g C m"^-2~d^-1*")"), limits = c(0, 5)) 
mr <- mr + theme(axis.text.x = element_blank(), axis.ticks = element_blank()) 
mr <- mr + scale_color_manual(values=c("black", "green"))

gpp <- ggplot(dt, aes(x = DATE, y = GPP, color = rs_algorithm)) + geom_line()
gpp <- gpp  + scale_x_date(labels = date_format("%Y"), name = 'Year', limits = as.Date(c("2010-01-01","2014-12-31")))#, limits = as.Date(c("2000-01-01","2012-01-01"))) 
#gpp <- gpp + scale_x_continuous(name = 'Years') #limits = c(2010, 2012), 
gpp <- gpp + scale_y_continuous(expression("GPP (g C m"^-2~d^-1*")")) 
gpp <- gpp + scale_color_manual(values=c("black", "green"))

npp <- ggplot(dt, aes(x = DATE, y = NPP, color = rs_algorithm)) + geom_line()
npp <- npp  + scale_x_date(labels = date_format("%Y"), name = 'Year', limits = as.Date(c("2010-01-01","2014-12-31")))#, limits = as.Date(c("2000-01-01","2012-01-01"))) 
#npp <- npp + scale_x_continuous(name = 'Years') #limits = c("2010", "2012"), 
npp <- npp + scale_y_continuous(expression("NPP (g C m"^-2~d^-1*")")) 
npp <- npp + scale_color_manual(values=c("black", "green"))

# create custom function to plot single ledgend for all pannels 
grid_arrange_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- grid_arrange_legend(p)

grid.arrange(arrangeGrob(lmr + theme_bw() + theme(legend.position = "none"), #, origin = "1970-01-01"
                         mr + theme_bw() + theme(legend.position = "none"),
                         gpp + theme_bw() + theme(legend.position = "none"), 
                         npp + theme_bw() + theme(legend.position = "none"),
                         legend, heights=c(0.45, 0.45, 0.1), nrow=3), 
             top = "Two Respiration Equations")
