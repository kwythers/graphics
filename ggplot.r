##### histogram with black outline, white fill
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


##### basic boxplots
ggplot(dt, aes(x = canopy, y = filled_vpd)) + geom_boxplot()

# basic box with the conditions colored
ggplot(dt, aes(x = canopy, y = tabovefill, fill = canopy)) + geom_boxplot()


##### basic scatter plots
ggplot(dt, aes(x=xvar, y=yvar)) +
  geom_point(shape=1)      # Use hollow circles


ggplot(dt, aes(x=xvar, y=yvar)) +
  geom_abline(intercept = 0, slope = 1) +   # add 1:1 line with a zero intercept 
  geom_point(shape=1)      # Use hollow circles

ggplot(dt, aes(x=xvar, y=yvar)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)

ggplot(dt, aes(x=xvar, y=yvar)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region

ggplot(dt, aes(x=xvar, y=yvar)) + 
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()            # Add a loess smoothed fit curve with confidence region

# more scatter plots
# set color by cond
ggplot(dt, aes(x=xvar, y=yvar, color=cond)) + geom_point(shape=1)

# same, but with different colors and add regression lines
ggplot(dat, aes(x=xvar, y=yvar, color=cond)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region

# extend the regression lines beyond the domain of the data
ggplot(dat, aes(x=xvar, y=yvar, color=cond)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=T) # Extend regression lines

# set shape by cond
ggplot(dt, aes(x=xvar, y=yvar, shape=cond)) + geom_point()

# same, but with different shapes
ggplot(dt, aes(x=xvar, y=yvar, shape=cond)) + geom_point() +
  scale_shape_manual(values=c(1,2))  # Use a hollow circle and triangle