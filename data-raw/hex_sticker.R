library(cropcircles)
library(tibble)
library(ggplot2)
library(ggpath)
library(showtext)

# choose a font from Google Fonts
font_add_google("Fira Sans", "firasans")
showtext_auto()

library(vital)
fr_sm <- as_vital(demography::fr.mort) |>
  smooth_mortality(Mortality) |>
  dplyr::filter(Sex == "male") |>
  dplyr::select(-Sex)

fr_sm |>
  collapse_ages(max_age = 80) |>
  ggplot(aes(x = Age, y = .smooth, col = Year, group = Year)) +
  geom_line(linewidth = 1) +
  scale_color_gradientn(colours = rainbow(10)) +
  guides(col = "none") +
  theme_void() +
  xlim(-3, 80) +
  scale_y_log10(limits = c(exp(-13), 4))

hex_scatter <- tempfile(fileext = ".png")
ggsave(hex_scatter, height = 6, width = 6)

img_cropped <- hex_crop(
  images = hex_scatter,
  bg_fill = "#666",
  border_colour = "#555",
  border_size = 72
)

ggplot() +
  geom_from_path(aes(0.5, 0.5, path = img_cropped)) +
  annotate("text",
    x = 0.3, y = -0.5, label = "vital", family = "firasans", size = 96, colour = "white",
    angle = 30, hjust = 0, fontface = "bold"
  ) +
  xlim(-1, 2) +
  ylim(-1, 2) +
  theme_void() +
  coord_fixed()

ggsave("./man/figures/vital-hex.png", height = 6, width = 6)
