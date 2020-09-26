#hex sticker designed by Henry Hershey
#licensed to Ben Staton on 2020-09-25

devtools::install_github("GuangchuangYu/hexSticker")
library(hexSticker)

logo <- file.path("/Users/HenryHershey/Pictures/hexlogo.png")
s <- sticker(logo, package="postpack", p_size=13,p_color="#333333",
             p_x=1,p_y=.6,
             s_x=1,s_y=1.25,
             s_width=.9,
             h_fill="#999999", h_color="#333333",h_size=4,
             url = "https://bstaton1.github.io/postpack/",
             u_x = 1,
             u_y = 0.08,
             u_size = 1.4,
             u_color = "#333333",
        filename="postpackhexsticker.png",dpi=900)
print(s)
s
