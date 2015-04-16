# histogram with black outline, white fill
ggplot(dt, aes(x = tabovefill)) + geom_histogram(binwidth = 2, colour = "black", fill = "white")

# histogram with mean line
ggplot(dt, aes(x = tabovefill)) + geom_histogram(binwidth = 2, colour = "black", fill = "white") +
  geom_vline(aes(xintercept = mean(tabovefill, na.rm = TRUE)),   # ignore NA values for mean
             color = "red", linetype = "dashed", size=1)

# histogram overlaid with kernel density curve
ggplot(dt, aes(x = tabovefill)) + 
  geom_histogram(aes(y = ..density..),      # histogram with density instead of count on y-axis
                 binwidth = 2,              # set bin width
                 color = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")  # overlay with transparent density plot

# dual overlaid histograms
ggplot(dt, aes(x = tabovefill, fill = canopy)) + geom_histogram(binwidth = 2, alpha = 0.5, position = "identity")

# dual overlaid density plots
ggplot(dt, aes(x = tabovefill, color = canopy)) + geom_density()

# dual overlaid density plots with semi-transparent fill
ggplot(dt, aes(x = tabovefill, fill = canopy)) + geom_density(alpha = 0.3)

# basic boxplot
ggplot(dt, aes(x = canopy, y = filled_vpd)) + geom_boxplot()

# A basic box with the conditions colored
ggplot(dt, aes(x = canopy, y = tabovefill, fill = canopy)) + geom_boxplot()