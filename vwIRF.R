# Copyright David García-León (2015)
# Based on the work by Hsiang (2012) and Schönbrodt (2012)
# Visually weighted time paths (also applicable to Impulse Response Functions)

vwIRF <- function(data, title="",method="lineal", shade.alpha=.1, spag=FALSE, spag.color="darkblue", show.median = FALSE, median.col = "white", palette=colorRampPalette(c("yellow","darkblue","red"))(20), ylim=NULL, ...) {

library(reshape2)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(extrafont)

B=ncol(data)        # Number of simulations
slices=nrow(data)   # Number of slices to split the X-axis
dataw<-data
start_year=2010

# Convert spaghettis to long format
dimnames(data) <- lapply(dim(data), function(x) 1:x)
data$index <- seq(start_year,start_year+slices-1,1)
b2 <- melt(data, id.vars="index")
colnames(b2) <- c("x", "B", "value")

# Initialise graph
data2 <- b2[((B/2)*slices+1):((B/2+1)*slices),]
p0 <- ggplot(data2, aes_string(x="Years", y="Values")) + theme_bw()

# Initialise elements with NULL (if they are defined, they are overwritten with something meaningful)
gg.tiles <- gg.spag <- gg.median <- gg.title <- gg.theme <- NULL

# Obtain extreme grid points at which density will be estimated
min_value <- min(dataw, na.rm=TRUE)   
max_value <- max(dataw, na.rm=TRUE)
ylim <- c(min_value,max_value*1.1)

# Vertical cross-sectional density estimate
d2 <- ddply(b2[, c("x", "value")], .(x), function(df) {
  res <- data.frame(density(df$value, na.rm=TRUE, n=slices, from=ylim[1], to=ylim[2])[c("x", "y")])
  colnames(res) <- c("y", "dens")
  return(res)
}, .progress="text")

# Transform densities according to method
if (method=="lineal"){
  d2<-d2
}
else if (method=="squared"){
  d2$dens <- d2$dens^2
}
else if (method=="rootsq"){
  d2$dens <- sqrt(d2$dens)
}

# Obtain bottom and top densities across slices
maxx<-tapply(d2$dens, d2$x, max)
minx<-tapply(d2$dens, d2$x, min)

    # Function to assign extreme densities to grid points
    finddens<-function(temp,tempm,slices){
      res <- 0
      for (i in (1:nrow(temp))) {
         res[i] <- tempm[floor(i/slices)+1]
      }
      temp$tempm <- res
      return(temp)
    }

# Assign bottom and top densities to grid points
d2<-finddens(d2,maxx,slices)
names(d2)[4] <- "maxdens"
d2<-finddens(d2,minx,slices)
names(d2)[5] <- "mindens"

# Rescale densities
d2$dens.scaled <- (d2$dens - d2$mindens)/d2$maxdens 

# Accommodate the fact that the density of the first slice is degenerated in one point
d2$dens.scaled[1]=1
d2$dens.scaled[2:slices]=0
  
# Fill tiles
d2$alpha.factor <- d2$dens.scaled^shade.alpha
gg.tiles <- list(geom_tile(data=d2, aes(x=x, y=y, fill=dens.scaled, alpha=alpha.factor)), scale_fill_gradientn("density.scaled", colours=palette), scale_alpha_continuous(range=c(0.001, 1))) 

# Show spaghettis
if (spag==TRUE) {
gg.spag <- geom_path(data=b2, aes(x=x, y=value, group=B), size=0.3, alpha=0.1, color=spag.color)
}

# Show median
if (show.median==TRUE) {
gg.median <-  geom_path(data=data2, aes(x=x, y=value, alpha=0.7), size=2, linejoin="mitre", color="red")
}

# Theme options
gg.theme <-theme(axis.title=element_text(size=16),
                 panel.grid.major = element_line(size=0.5),
                 text = element_text(family="Lato"),
                 axis.ticks =  element_blank(),
                 legend.position="none"
                  )

gg.elements <- list(gg.median,gg.tiles, gg.spag, gg.title, gg.theme)

return(p0 + gg.elements)
}