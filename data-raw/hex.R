#install.packages("hexSticker")
library(hexSticker)
library(ggplot2)
img <- "data-raw/hexsubplot.png"
out <- "data-raw/rvtable.png"
mult <- 4 # multiplier for larger sticker size, prevents pixelated subplot image text.

sticker(img, 1, 1.05, 0.7, 0.8, "rvtable", p_size = mult * 36, p_y = 1.45, p_color = "navajowhite", h_size = mult * 1.4, h_fill = "gray30",
        h_color = "dodgerblue", url = "leonawicz.github.io/rvtable", u_color = "navajowhite", u_size = mult * 3,
        filename = out)

# overwrite file for larger size
ggsave(out, width = mult*43.9, height = mult*50.8, bg = "transparent", units = "mm")

# store a smaller version (mult = 1) of this higher quality image output at inst/tabr.png
