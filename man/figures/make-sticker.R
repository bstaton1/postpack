# hex sticker designed by Henry Hershey
# licensed to Ben Staton on 2020-09-25

# install.packages("hexSticker")
library(hexSticker)

figs_path = "man/figures"

logo = file.path(figs_path, "hh-logo.png")
s = sticker(logo, package = "postpack",
            p_size = 23,
            p_color = "#505050",
            p_x = 1,
            p_y = 0.65,
            s_x = 1,
            s_y = 1.3,
            s_width = 0.95,
            h_fill = "#E8E8E8",
            h_color = "#505050",
            h_size = 0.75,
            url = "bstaton1.github.io/postpack",
            u_x = 0.97,
            u_y = 0.05,
            u_size = 6,
            u_color = "#505050",
            filename = file.path(figs_path, "logo.png"))
