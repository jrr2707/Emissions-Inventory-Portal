library(shiny)
library(RPostgres)
library(DBI)
library(RJSONIO)
library(DT)
library(ggplot2)
library(plyr)
library(dplyr)
library(jsonlite)

# create ggplot from given data set
#
# data: data frame
# x: x column
# y: y column
# color: color axis
#
# returns: ggplot object

createPlot <- function(dataFrame, aes, final_max){
    cat(final_max)
    ggplot(dataFrame, aes) +
        geom_col(width = 0.4, fill="red") +
        coord_cartesian(ylim = c(0, final_max)) +
        scale_y_continuous(breaks = seq(0, final_max, by = sequence)) +
        scale_x_continuous(breaks = seq(2010, 2100, by = 1)) +
        xlab("Year") + ylab("Total Emissions (MMBtu)") +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )

}
