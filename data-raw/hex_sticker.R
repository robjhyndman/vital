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
  collapse_ages(max_age = 95) |>
  dplyr::select(-Sex)
fr_sm |>
  ggplot(aes(x = Age, y = Mortality, group = Year)) +
  geom_line(linewidth = 2) +
  guides(col = "none") +
  theme_void() +
  xlim(-10, 100) +
  scale_y_log10(limits = c(exp(-13),1))


hex_scatter <- tempfile(fileext = ".png")
ggsave(hex_scatter, height = 6, width = 6)

img_cropped <- hex_crop(
  images = hex_scatter,
  bg_fill = "#234460",
  border_colour = "#57abef",
  border_size = 72
)

ggplot() +
  geom_from_path(aes(0.5, 0.5, path = img_cropped)) +
  annotate("text", x = 0.3, y = -0.5, label = "vital", family = "firasans", size = 96, colour = "white",
           angle = 30, hjust = 0, fontface = "bold") +
  xlim(-1, 2) +
  ylim(-1, 2) +
  theme_void() +
  coord_fixed()

ggsave("./man/figures/vital-hex.png", height = 6, width = 6)
