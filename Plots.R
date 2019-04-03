### Packages ###
library(ggplot2)
library(plotly)
library(reshape2)
library(gridExtra)

# Fit plane to standardized decadal temps
fit <- lm(StdMin ~ log(Population) + StationDist, decadeAvgs)

# Generate plane of best fit for plotly
x_axis <- seq(min(decadeAvgs$Population), max(decadeAvgs$Population), by = 10000)
y_axis <- seq(min(decadeAvgs$StationDist), max(decadeAvgs$StationDist), by = 1)
plane <- expand.grid(Population = x_axis, StationDist = y_axis)
plane$StdMin <- predict.lm(fit, plane)
plane <- acast(plane, StationDist ~ log(Population), value.var = "StdMin")

# Make 3D Scatter with plane of best fit
plt <- plot_ly(decadeAvgs, 
               x = ~log(Population),
               y = ~StationDist,
               z = ~StdMin,
               type = "scatter3d",
               mode = "markers",
               marker = list(size = 3,
                             color = "#013A9E")) %>%
  add_surface(x = log(x_axis),
              y = y_axis,
              z = plane,
              colorscale = list(c(0,1), c("#8C42F4", "#E69628")),
              colorbar = list(
                title = "Standardized Temperature")) %>%
  layout(title = "Standardized Decadal Minimum Temperature",
         scene = list(
           xaxis = list(title = "log(Metro Population)"),
           yaxis = list(title = "Distance to Center of Metro Area (km)"),
           zaxis = list(title = "Standardized Temperature",
                        range = c(-1.25, 1.25))))

# Plot number of extremely cold days in 3D
cld_fit <- lm(ColdNights ~ log(Population) + StationDist, extremes)
cld_x_axis <- seq(min(extremes$Population), max(extremes$Population), by = 10000)
cld_y_axis <- seq(min(extremes$StationDist), max(extremes$StationDist), by = 1)
cld_plane <- expand.grid(Population = cld_x_axis, StationDist = cld_y_axis)
cld_plane$ColdNights <- predict.lm(cld_fit, cld_plane)
cld_plane <- acast(cld_plane, StationDist ~ log(Population), value.var = "ColdNights")

coldNights3D <- plot_ly(extremes, x = ~log(Population), y = ~StationDist, z = ~ColdNights, 
                        type="scatter3d", mode="markers", marker=list(size=3,color="#2A2A60")) %>% 
  add_surface(x = log(cld_x_axis), y = cld_y_axis, z=cld_plane, 
              colorscale = list(c(0,1), c("#A5EBFF", "#000F64")),
              colorbar = list(
                title = "Extremely Cold Nights per Decade")) %>% 
  layout(title = "Extremely Cold Nights",
         scene = list(
           xaxis = list(title = "log(Metro Population)"),
           yaxis = list(title = "Distance to Center of Metro Area (km)"),
           zaxis = list(
             title = "Extremely Cold Nights per Decade",
             range = c(0, 225))))

# Number of nights and days in top and bottom 1% by distance to metro center
hotNights <- ggplot(extremes, aes(x=StationDist, y=HotNights)) + 
  geom_point(color="#2A2A60") + geom_smooth(method=lm, color="#800000") +
  labs(title = "Extremely Hot Nights per Decade", 
       x = "Distance to Center of Metro Area (km)",
       y = "Number of Nights in Hottest 1% for Metro Area") + ylim(0,225)
coldNights <- ggplot(extremes, aes(x=StationDist, y=ColdNights)) + 
  geom_point(color="#2A2A60") + geom_smooth(method=lm, color="#00FFFF") +
  labs(title = "Extremely Cold Nights per Decade", 
       x = "Distance to Center of Metro Area (km)",
       y = "Number of Nights in Coldest 1% for Metro Area") + ylim(0,225)
hotDays <- ggplot(extremes, aes(x=StationDist, y=HotDays)) +
  geom_point(color="#C88000") + geom_smooth(method=lm, color="#FF0000") +
  labs(title = "Extremely Hot Days per Decade", 
       x = "Distance to Center of Metro Area (km)",
       y = "Number of Days in Hottest 1% for Metro Area") + ylim(0,225)
coldDays <- ggplot(extremes, aes(x=StationDist, y=ColdDays)) +
  geom_point(color="#C88000") + geom_smooth(method=lm, color="#008080") +
  labs(title = "Extremely Cold Days per Decade", 
       x = "Distance to Center of Metro Area (km)",
       y = "Number of Days in Coldest 1% for Metro Area") + ylim(0,225)
extremesPlot <- grid.arrange(hotNights, coldNights, hotDays, coldDays, nrow=2)

# Seasonal changes in UHI effect
lowsPopulation <- ggplot(decadeStats, aes(x=log(Population), y=DecadeStdMonthlyMin)) +
  geom_point(color="#2A2A60", size = 1) + geom_smooth(method=lm, color="#00FFFF") + ylim(-2,2) +
  labs(title = "Decadal Average Standardized Monthly Lows",
       x = "log(Metro Population)",
       y = "Standardized Temperature") + facet_grid(. ~ Season)
lowsDist <- ggplot(decadeStats, aes(x=StationDist, y=DecadeStdMonthlyMin)) +
  geom_point(color="#2A2A60", size = 1) + geom_smooth(method=lm, color="#00FFFF") + ylim(-2,2) +
  labs(title = "Decadal Average Standardized Monthly Lows",
       x = "Distance to Center of Metro Area (km)",
       y = "Standardized Temperature") + facet_grid(. ~ Season)
highsPopulation <- ggplot(decadeStats, aes(x=log(Population), y=DecadeStdMonthlyMax)) +
  geom_point(color="#C88000", size = 1) + geom_smooth(method=lm, color="#FF0000") + ylim(-2,2) +
  labs(title = "Decadal Average Standardized Monthly Highs",
       x = "log(Metro Population)",
       y = "Standardized Temperature") + facet_grid(. ~ Season)
highsDist <- ggplot(decadeStats, aes(x=StationDist, y=DecadeStdMonthlyMax)) +
  geom_point(color="#C88000", size = 1) + geom_smooth(method=lm, color="#FF0000") + ylim(-2,2) +
  labs(title = "Decadal Average Standardized Monthly Highs",
       x = "Distance to Center of Metro Area (km)",
       y = "Standardized Temperature") + facet_grid(. ~ Season)
seasonsPlot <- grid.arrange(lowsPopulation, lowsDist, highsPopulation, highsDist, nrow=2)