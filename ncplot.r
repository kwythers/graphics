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
#workdir <- '~/tmp/us-umb/stand/lnd/hist'
#setwd(workdir)
#ncfname <- 'US-UMB_I1850CLM45CN_ad_spinup.clm2.h0.0701-01-01-00000.nc'
#ncf <- open.nc(ncfname)
#vars <- as.list(read.nc(nc))

##### build set of datatables, once for each variable of interest

##### create datatable from standard maintenence respiration files
# open all files and initialize matrix
files <- list.files(path = '~/tmp/us-umb-n/a_t_n-/lnd/hist', pattern = '*.h0.*.nc', 
                    full.names = TRUE)
m <- matrix(NA, length(files), 5)

# loop over all files
for(i in 1:length(files)) {
  ncf <- open.nc(files[i])
  a <- (var.get.nc(nc = ncf, "LEAF_MR") * 3600 * 24 * 365)
  b <- (var.get.nc(nc = ncf, "MR") * 3600 * 24 * 365)
  c <- (var.get.nc(nc = ncf, "GPP") * 3600 * 24 * 365)
  d <- (var.get.nc(nc = ncf, "NPP") * 3600 * 24 * 365)
  t <- (var.get.nc(nc = ncf, "time") / 365)
  m[i, ] <- t(rbind(a, b, c, d, t))
  #print(m) # for debugging
}

t <- seq(0, 820, by = 20) # create time variable, step by 20
stand <- as.data.table(m) # convert matrix to data table
stand[, "V5"] <- t # overwrite "time" with t, and rename columns
stand <- rename(stand, c("V1"="LEAF_MR", "V2"="MR","V3"="GPP", "V4"="NPP", "V5"="YEARS"))
stand <- stand[, RS := "stand"] # add a column for respiration algorithm

##### create datatable from no acclimation maintenence respiration files
# open all files and initialize matrix
files <- list.files(path = '~/tmp/us-umb-n/mh_t_n-/lnd/hist', pattern = '*.h0.*.nc', 
                    full.names = TRUE)
m <- matrix(NA, length(files), 5)

# loop over all files
for(i in 1:length(files)) {
  ncf <- open.nc(files[i])
  a <- (var.get.nc(nc = ncf, "LEAF_MR") * 3600 * 24 * 365)
  b <- (var.get.nc(nc = ncf, "MR") * 3600 * 24 * 365)
  c <- (var.get.nc(nc = ncf, "GPP") * 3600 * 24 * 365)
  d <- (var.get.nc(nc = ncf, "NPP") * 3600 * 24 * 365)
  t <- (var.get.nc(nc = ncf, "time") / 365)
  m[i, ] <- t(rbind(a, b, c, d, t))
  #print(m) # for debugging
}

t <- seq(0, 820, by = 20) # create time variable, step by 20
noacclim <- as.data.table(m) # convert matrix to data table
noacclim[, "V5"] <- t # overwrite "time" with t, and rename columns
noacclim <- rename(noacclim, c("V1" = "LEAF_MR", "V2"="MR","V3" = "GPP", "V4" = "NPP", "V5" = "YEARS"))
noacclim <- noacclim[, RS := "noacclim"] # add a column for respiration algorithm

##### create datatable from acclimation maintenence respiration files
# open all files and initialize matrix
files <- list.files(path = '~/tmp/us-umb-n/ch_t_n-/lnd/hist', pattern = '*.h0.*.nc', 
                    full.names = TRUE)
m <- matrix(NA, length(files), 5)

# loop over all files
for(i in 1:length(files)) {
  ncf <- open.nc(files[i])
  a <- (var.get.nc(nc = ncf, "LEAF_MR") * 3600 * 24 * 365)
  b <- (var.get.nc(nc = ncf, "MR") * 3600 * 24 * 365)
  c <- (var.get.nc(nc = ncf, "GPP") * 3600 * 24 * 365)
  d <- (var.get.nc(nc = ncf, "NPP") * 3600 * 24 * 365)
  t <- (var.get.nc(nc = ncf, "time") / 365)
  m[i, ] <- t(rbind(a, b, c, d, t))
  #print(m) # for debugging
}

t <- seq(0, 820, by = 20) # create time variable, step by 20
acclim <- as.data.table(m) # convert matrix to data table
acclim[, "V5"] <- t # overwrite "time" with t, and rename columns
acclim <- rename(acclim, c("V1" = "LEAF_MR", "V2" = "MR","V3" = "GPP", "V4" = "NPP", "V5" = "YEARS"))
acclim <- acclim[, RS := "acclim"] # add a column for respiration algorithm

### concatinate the data tables into single data source
l <- list(stand, noacclim, acclim)
dt <- rbindlist(l, use.names=TRUE)

### MAKE FANCY FIGURES

lmr <- ggplot() +
  geom_line(data = stand, aes(x = YEARS, y = LEAF_MR), color = 'black') +
  geom_line(data = noacclim, aes(x = YEARS, y = LEAF_MR), color = 'blue') + 
  geom_line(data = acclim, aes(x = YEARS, y = LEAF_MR), color = 'red') + 
  scale_x_continuous(name = '') +                                                     # hide x-axis name
  scale_y_continuous(expression("LMR (g C m"^-2~yr^-1*")"), limits = c(0, 400)) +   # set y-axis to be 0:400
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())                # hide x-axis tick marks
  #panel.background  = element_rect(fill = "white", colour = NA) 

mr <- ggplot() + 
  geom_line(data = stand, aes(x = YEARS, y = MR), color='black') +
  geom_line(data = noacclim, aes(x = YEARS, y = MR), color='blue') + 
  geom_line(data = acclim, aes(x = YEARS, y = MR), color='red') + 
  scale_x_continuous(name = '') +
  scale_y_continuous(expression("MR (g C m"^-2~yr^-1*")"), limits = c(0, 400)) + 
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) 
  #panel.background  = element_rect(fill = "white", colour = NA) 

gpp <- ggplot() + 
  geom_line(data = stand, aes(x = YEARS, y = GPP), color = 'black') +
  geom_line(data = noacclim, aes(x = YEARS, y = GPP), color = 'blue') + 
  geom_line(data = acclim, aes(x = YEARS, y = GPP), color = 'red') + 
  scale_x_continuous(name = 'Years', breaks = c(100, 200, 300, 400, 500, 600, 700, 800), 
                     labels = c("100", "200", "300", "400", "500", "600", "700", "800")) +  # set x-axis labels to every 100 years
  scale_y_continuous(expression("GPP (g C m"^-2~yr^-1*")")) 
  #panel.background  = element_rect(fill = "white", colour = NA)
  
npp <- ggplot() + 
  geom_line(data = stand, aes(x = YEARS, y = NPP), color = 'black') +
  geom_line(data = noacclim, aes(x = YEARS, y = NPP), color = 'blue') + 
  geom_line(data = acclim, aes(x = YEARS, y = NPP), color = 'red') + 
  scale_x_continuous(name = 'Years', breaks = c(100, 200, 300, 400, 500, 600, 700, 800), 
                     labels = c("100", "200", "300", "400", "500", "600", "700", "800")) +  # set x-axis labels to every 100 years
  scale_y_continuous(expression("NPP (g C m"^-2~yr^-1*")")) 
  #panel.background = element_rect(fill = "white", colour = NA)

# create custom function to plot single ledgend for all pannels 
grid_arrange_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- grid_arrange_legend(lmr)


grid.arrange(lmr, mr, gpp, npp, ncol = 2, nrow = 2, # display all 4 plots as 2 x 2 grid
             top="Three Respiration Equations") 
             #bottom = textGrob("original=black, acclim=blue, acclim at 25°C=red", 
                               #gp = gpar(fontface=3, fontsize=9), 
                               #hjust=1, x=1))

