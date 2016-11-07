###########################
# Geospatial Plot Example
###########################

#install.packages("rgdal") # Uncomment if you still need the rgdal package.

library(rgdal)

MAKE_PAPER <- FALSE # If set to TRUE the plot will be saved as .pdf.

pre_data <- read.csv2("data/pre_data.csv", stringsAsFactors = F)

# Select the communities with <= 1500 eligible voters in 2011 and make sample_data and rest_data
sample_data <- pre_data[which(pre_data$eligible_voters_2011<=1500),]

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
myColours[bw@data$NAME_4%in%sample_data$community_name] <- gray.colors(3)[1]
myColours[64] <- "white" #pick right Altdorf
myColours[116] <- "white" #pick right Altheim
myColours[370] <- "white" #pick right Dürnau
myColours[403] <- "white" #pick right Talheim

# Get the Dataset with the information you want to plot.

plot_data <- read.csv2("data/plot_data.csv",stringsAsFactors = F)

# Create Selectors for Treatment and Control.

sample <- myColours==gray.colors(3)[1]
trt_map <- which(sample)[bw@data$NAME_4[sample]%in%as.character(plot_data$community_name)[plot_data$treatment_indicator_Z==1]]
con_map <- which(sample)[bw@data$NAME_4[sample]%in%as.character(plot_data$community_name)[plot_data$treatment_indicator_Z==0]]

# Change colors based on selectors.

myColours <- rep("white", length(bw@data$NAME_4))
myColours[trt_map] <- "#46962b"
myColours[con_map] <- "#ffd500"

if ( MAKE_PDF ) {
  pdf("map_trt_con.pdf",width=9, height=8)
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

###########################################################################################################

###########################################################################################################
# Example with continuous Data and random Communities
###########################################################################################################

# Draw some Communities based on CCA_4

gemeinden <- sample(bw@data$CCA_4,100)

# Make some random continuous indicator variable

continous_value <- rnorm(100, 2, 2)

# Set up Color Palette (from, to)

rbPal <- colorRampPalette(c('lightgrey','black'))

# Make Color vector for the sample communities

Col <- rbPal(20)[as.numeric(cut(continous_value,breaks = 20))]

# Add colors to map plot

myColours <- rep("white", length(bw@data$NAME_4))
myColours[bw@data$CCA_4%in%gemeinden] <- Col

# Plot it.

par(
  family = "serif",
  oma = c(1,1,1,1),
  mar = c(5,5,5,5),
  adj = 0.5
)

plot(bw,col = myColours, border = 'gray80')

# Add a legend with color palette
legend("topleft",title="Arbitraty",legend=seq(min(continous_value), max(continous_value), length.out = 20),col =rbPal(20),pch=20)
