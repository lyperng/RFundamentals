# some potential code for the course that is still in the process of troubleshooting



################### intro plotting test run script ######################
pdf(here('figures','myplots.pdf'))
plot(cars) # simple plot to visualize the data

plot(cars, xlab = "Speed (mph)", ylab = "Stopping Distance (ft)", main = "Relationship between Speed and Stopping Distance") # can add labels

# adjust item sizes
plot(cars, cex = 1.5) # cex is multiplier--points are 1.5x size
plot(cars, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5) # enlarge axis labels, axis titles, plot title

plot(cars, las = 1) # adjust angles of axis labels

#play with fonts
#font_import() # imports fonts from extrafont pkg
#loadfonts(device = "win") # load them into your workspace
fonts() # see list of available fonts so you know the exact syntax
plot(cars, family = 'Century Gothic') # try out some different ones!

plot(cars, col = 'royalblue', pch = 19) # point color and type

lmod1<-lm(dist ~ speed, data = cars) # get lm for trendline
abline(lmod1, col = "magenta", lty = 5) # add trendline-color red linetype 5 (long dashes)

plot(0, 0, xlim=c(0, 25), ylim=c(0, 120), type='n', xlab='Speed', ylab='Distance')

points(cars$speed, cars$dist, col='hotpink')
lines(cars$speed, cars$dist)
lines(c(5,25), c(20,100))

abline(h = mean(cars$dist), col = "darkorchid") # add horizontal line at value of interest
abline(v = mean(cars$speed)-sd(cars$speed), col = "coral1") # add vert line
abline(v = mean(cars$speed)+sd(cars$speed), col = "coral1") # add vert line

hist(cars$speed)
dev.off()

################## two factor ANOVA plot with mtcars ##########
str(mtcars)
# initialize new plot with args color and fill
g5<- ggplot(means, aes(x = Species, y = mean, color = Species, fill = Species)) + 
  # color lines, fill area
  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.04,size=0.5, position = position_dodge(width = 0.9)) +
  labs(x = "Species", y = "Sepal Length", 
       title = "Average Sepal Length by Species with Error Bars")
g5

g6 <- g5 +  
  # Set manual colors 
  scale_fill_manual(values = c("lavender", "aquamarine", "lightpink")) + # color areas
  scale_color_manual(values = c("mediumpurple1", "aquamarine4", "rosybrown3")) + # color lines
  
  # adjust theme for a cleaner plot
  theme_bw() +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank()) #delete minor grid lines
g6




#################### testing base plot ANCOVA plot ######################

# Fit the ANCOVA model
model <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)

# Plot the data and the regression line
plot(Sepal.Length ~ Petal.Length, data = iris, pch = 16, col = c("lavender", "skyblue", "lightpink")[iris$Species])
legend(x= 1, y =7.7, legend = unique(iris$Species), pch = 16, col = c("lavender", "lavender", "lightpink"))

################### monk seal growth rate function #######################

# it runs and mechanically works but something is wrong with the pop growth equations
# population becomes neg rly fast
# i added an ifelse() in the line that pipes pop to pop_data where if it is 0 or less, it puts 0

# maybe save this code for the troubleshooting module and let the students figure it out

library(dplyr)

# function to predict monk seal population growth
monk_seal_growth <- function(start_pop, fishing_mort_pct, shark_mort_pct, natural_mort_pct, birth_rate_pct, logistic_model = FALSE, K = NULL) {
  
  # create empty data frame to store population predictions
  pop_data <- data.frame(year = integer(), pop = numeric(), growth_rate = numeric(), stringsAsFactors = FALSE)
  
  # set starting population
  pop <- start_pop
  
  # loop through years and predict population growth
  for (year in 1:10) {
    
    # calculate total mortality
    total_mort_pct <- fishing_mort_pct + shark_mort_pct + natural_mort_pct
    
    # calculate population growth rate
    if (logistic_model) {
      growth_rate <- (birth_rate_pct * pop * (1 - (pop / K)) - (total_mort_pct / 100) * pop) / 100
    } else {
      growth_rate <- ((birth_rate_pct - total_mort_pct) / 100) * pop
    }
    
    # update population
    pop <- pop + round(growth_rate * pop)
    
    # add data to data frame
    pop_data <- pop_data %>% add_row(year = year, pop = ifelse(pop>0,pop,0), growth_rate = growth_rate)
  }
  
  # write data to csv
  write.csv(pop_data, file = here('outputs',"monk_seal_pop_predictions.csv"), row.names = FALSE)
  
  # return data frame
  return(pop_data)
}

# predict monk seal population growth using an exponential model
monk_seal_growth(start_pop = 100, fishing_mort_pct = 10, shark_mort_pct = 5, natural_mort_pct = 2, birth_rate_pct = 8)

# predict monk seal population growth using a logistic model with carrying capacity of 2000
monk_seal_growth(start_pop = 100, fishing_mort_pct = 10, shark_mort_pct = 5, natural_mort_pct = 2, birth_rate_pct = 8, logistic_model = TRUE, K = 2000)




######### creating sample data for module 5 (simple analyses) & 6 (intro plotting) #########
# want t test, one factor ANOVA, two factor ANOVA, regression, and maybe ANCOVA
# have many groups for one factor ANOVA so that they have to reangle the x axis labels
# set breaks/scales manually

# sample time series dataset
df <- data.frame(date = as.Date("2021-01-01") - 0:99,
                 sales = runif(100, 10, 500) + seq(50, 149)^2)
str(df)

p <- ggplot(df, aes(x=date, y=sales)) +
  geom_line()
p






