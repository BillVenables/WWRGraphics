## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = "",
                      fig.height = 5.5, 
                      fig.width = 7,
                      out.width = "0.8\\textwidth")
library(WWRGraphics)
setHook("plot.new",
        list(las = function() par(las = 1),
             pch = function() par(pch = 20),
             cex.axis = function() par(cex.axis = 0.8)),
        "replace")

## -----------------------------------------------------------------------------
par(mar = c(3,3,3,1), bg = "white")
greenish <- alpha("dark green", 0.25)
blueish <- alpha("blue", 0.5)
pinkish <- alpha("pink", 0.75)
#### 
#### First view, Population
#### 
z <- with(roundTrip, setNames(complex(real = Longitude, 
                                      imaginary = Latitude), 
                              Locality))
plot(z, asp = 1, cex = 0.7, ann = FALSE, axes = FALSE, bty = "n", 
     ylim = c(-45,-9))
axis(1, at = 10*(11:16))
axis(2, at = -10*(5:1))
grid()
lines(Oz, col = greenish)
text(z, labels = names(z), pos = avoid(z), cex = 0.7, offset = 0.25)
circles(Latitude ~ Longitude, roundTrip, radii = sqrt(Population), 
        maxradius = 0.5, fill = pinkish, colour = "hot pink")
title(main = "Population")
#### 
#### Second view
#### 
plot(z, asp = 1, cex = 0.7, ann = FALSE, axes = FALSE, bty = "n", 
     ylim = c(-45,-9))
axis(1, at = 10*(11:16))
axis(2, at = -10*(5:1))
grid()
lines(Oz, col = greenish)
arrows(z, cyc(z), gap = 0.5, col = blueish)
km <- gcd_km(z, cyc(z)) %>% round
text((z + cyc(z))/2, labels = km, col = "red", family = "serif", 
     font = 4, cex = 0.8)
title(main = "Great Circle Distances")

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
zfill <- function(x) gsub(" ", "0", format(x, justify = "right"))
ggOz <- data.frame(Oz) %>% within({
  strip <- paste0("S", zfill(cumsum(is.na(x))))
}) %>% na.omit()
legs <- data.frame(start = z, end = cyc(z), mid = (z + cyc(z))/2, 
                   km = round(gcd_km(z, cyc(z))))
head <- arrow(length = unit(0.0625, "inches"), angle = 15, type = "closed")
####
g0 <- ggplot(ggOz, aes(x, y)) + 
  geom_path(aes(group = strip), colour = greenish) + xlab("") + ylab("") + 
  geom_point(aes(x = Longitude, y = Latitude), size = 0.5,
             data = roundTrip) + coord_equal() + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
####
g0 + geom_point(data = roundTrip, aes(x = Longitude, y = Latitude, 
                                      size = Population),
                fill = pinkish, colour = "hot pink", shape = 21) + 
  scale_size_area(max_size = 12) +
  geom_text_repel(aes(x = Longitude, y = Latitude, label = Locality),
                  size = 3, data = roundTrip) + ggtitle("Population")
####
g0 + geom_segment(data = legs, aes(x = Re(start), y = Im(start),
                                   xend = Re(end), yend = Im(end)),
                  arrow = head, colour = "steel blue", size = 0.25) +
  geom_text(data = legs, aes(x = Re(mid), y = Im(mid), 
                             label = as.character(km)), colour = "red",
            size = 3.5, family = "serif", fontface = "bold.italic") + 
  ggtitle("Great Circle Distances")

