## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "")
library(WWRGraphics)

## ---- fig.show='hold',fig.width=8,fig.height=8,out.width="0.8\\textwidth"-----
lazyData::requireData(doParallel)
par(mar=c(1,1,4,1), mfrow = c(2,2))
pals <- paste0("pal_", cq(desert, green2pink, blue2red, heat_hcl))
foreach(pal = pals, let = letters[1:4]) %do% {
  showColors(get(pal)(150), main = pal)
  text("top right", paste0("(", let, ")"), cex = 1.25, font = 2)
} %>% invisible()

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(head(mtcars, 10))

