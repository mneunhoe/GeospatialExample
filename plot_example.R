###########################
# Geospatial Plot Example
###########################

#install.packages("rgdal") # Uncomment if you still need the rgdal package.

library(rgdal)

MAKE_PAPER <- FALSE # If set to true the plot will be saved as .pdf.

# Load the Community Shapefiles. The file is from gadm.org

gadm <- readRDS("data/DEU_adm4.rds")

# I only need Baden-Württemberg

bw <- gadm[gadm$NAME_1=="Baden-Württemberg",]

# You can remove the unnecessary Communities.

rm(gadm)

# Clean up community names to same format

bw@data$NAME_4[bw@data$NAME_4=="Widdern"] <- "Widdern, Stadt"
bw@data$NAME_4[bw@data$NAME_4=="Hettingen"] <- "Hettingen, Stadt"
bw@data$NAME_4[bw@data$NAME_4=="Langenburg"] <- "Langenburg, Stadt"

# Set baseline color (here white).

myColours <- rep("white", length(bw@data$NAME_4))
myColours[64] <- "white" #pick right Altdorf
myColours[116] <- "white" #pick right Altheim
myColours[370] <- "white" #pick right Dürnau
myColours[403] <- "white" #pick right Talheim

# Get the Dataset with the information you want to plot.

plot_data <- read.csv2("data/plot_data.csv",stringsAsFactors = F)

# Create Selectors for Treatment and Control.

trt_map <- which(sample)[bw@data$NAME_4[sample]%in%as.character(plot_data$community_name)[plot_data$treatment_indicator_Z==1]]
con_map <- which(sample)[bw@data$NAME_4[sample]%in%as.character(plot_data$community_name)[plot_data$treatment_indicator_Z==0]]

# Change colors based on selectors.

myColours <- rep("white", length(bw@data$NAME_4))
myColours[trt_map] <- "#46962b"
myColours[con_map] <- "#ffd500"

if ( MAKE_PDF ) {
  pdf("graphs/map_trt_con.pdf",width=9, height=8)
}

# Set graph parameters as you like.
par(
  family = "serif",
  oma = c(1,1,1,1),
  mar = c(5,5,5,5),
  adj = 0.5
)
# Plot Communities and color them.

plot(bw,col = myColours, border = 'gray80')

# Add a title to the plot.

title("The Treatment- and Control-Communities \n in Baden-Württemberg")

# Add a legend.

legend("bottom", inset=c(0,-0.1), legend=c("Treatment","Control"), fill=c("#46962b","#ffd500"),border="gray80",bty="n",cex=0.8,xpd=TRUE)

if ( MAKE_PDF ) {
  dev.off()
}
