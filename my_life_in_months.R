library(dplyr)
library(lubridate)
library(forcats)
library(tidyr)
library(waffle)
library(hrbrthemes)
library(extrafont)
library(ggplot2)
library(prismatic)
library(showtext)
library(xkcd)
library(extrafont)

loadfonts(device = "pdf", quiet = TRUE)

## Add the font with the corresponding font faces
font_add("Wild Hazelnut", regular = "Wild Hazelnut.ttf")
font_add("Drunk Handwriting", regular = "Drunk Handwriting.ttf")
font_add("Waltograph", regular = "waltograph42.otf")
font_add("Pacifico", regular = "Pacifico-Regular.ttf")
font_add("Filetto", regular = "filetto_regular.ttf")
## Automatically use showtext to render plots
showtext_auto()
# Prep data ----

birth_year <- 1992
birth_month <- 3
current_year <- year(today())
current_month <- month(today())

life_data <- expand_grid(
  month = month.name,
  year = birth_year:current_year
) %>%
  mutate(month = fct_relevel(month, month.name)) %>%
  arrange(year, month) %>%
  group_by(year) %>%
  mutate(month_number = row_number()) %>%
  ungroup() %>%
  filter(!(year == birth_year & month_number < birth_month)) # %>%
# filter(!(year == current_year & month_number > current_month)) # If you want to exclude after the current month - I didn't, because it looked weird!

# Add "eras" to be coloured
# "era" text can be used for annotation, and the fill colour will colour the waffle chart

eras <- tribble(
  ~year_month, ~era, ~fill_colour,
  "1992,3", "childhood", "#f30189",
  "2007,9", "Fondy", "#f08421",
  "2010,8", "bachelors", "#fdb910",
  "2013,12", "moved to tucson", "#ffeb00",
  "2014,4", "elliotts\non\nCongress", "#74c042",
  "2016,2", "Lab Tech", "#02adf2",
  "2018,1", "Masters", "#5859a9",
  "2019,5", "PhD", "#722c8f"
)

# Darken fill colour to be used for text annotations

eras[["text_colour"]] <- as.character(clr_darken(eras[["fill_colour"]], shift = 0.1))

life_data <- life_data %>%
  rowwise() %>%
  mutate(year_month = paste0(c(year, month_number), collapse = ",")) %>%
  ungroup() %>%
  left_join(eras, by = "year_month") %>%
  fill(era, fill_colour, text_colour) %>%
  mutate(fill_colour = fct_inorder(fill_colour))

# Split life data into list based on era for using labels/colours later on

life_data_list <- split(life_data, life_data$era)

# Make waffle chart! ----

# Base plot

background_colour <- "#FFFFFF"

life_in_months_base <- life_data %>%
  count(fill_colour) %>% ## the count of each era is the number of months in that era
  ggplot() +
  geom_waffle(aes(fill = fill_colour, values = n), color = background_colour, n_rows = 12, size = 1, flip = FALSE) + ## make each row a year/12 months
  coord_equal() +
  scale_x_continuous(limits = c(-0.5, 37.5)) + # The max here will differ based on how old you are! I'm 29 (so there are 30 squares), so ~7.5 more for the additional annotation on the side
  scale_y_continuous(limits = c(-2.5, 14.5)) +
  scale_fill_identity() +
  labs(y = NULL, x = NULL) +
  theme_ipsum(grid = "") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = background_colour, color = background_colour),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# Initial annotations ----

annotation_base_size <- 10 # Use ~10 for exporting at dpi 300, and ~3 for working interactively
annotation_lineheight <- 1
initial_annotations_font_family <- "Wild Hazelnut"


initial_text <- function(x, y, label, size = annotation_base_size, colour = initial_annotations_colour, ...) {
  annotate("text", x = x, y = y, label = label, size = size, family = "Wild Hazelnut", ...)
}

initial_segment <- function(x, xend, y, yend) {
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend))
}

datalines <- data.frame(x1=c(0,0), y1=c(1, 8),
                        xends=c(0,0),
                   yends=c(5, 12))

datalinesage <- data.frame(x1=c(2), y1=c(0),
                        xends=c(4),
                        yends=c(0))


life_in_months_initial_annotations <- life_in_months_base +
  initial_text(x = 0, y = 6.5, label = "1 year", angle = 90) +
  initial_segment(x = -0.25, xend = 0.25, y = .94, yend = .98) +
  xkcdline(aes(x = x1, y = y1, xend = xends, yend = yends), datalines, xjitteramount = .28) +
  initial_segment(x = -0.25, xend = 0.25, y = 12, yend = 12) +
  initial_text(x = 1, y = 14.5, label = "1 square = 1 month", size = annotation_base_size * 0.8, lineheight = annotation_lineheight, hjust = 0.4) +
  geom_curve(aes(x = 0, xend = 1, y = 14, yend = 12), arrow = arrow(length = unit(0.0175, "npc"))) +
  initial_text(x = 0.5, y = 0, label = "age", size = annotation_base_size * 0.8, hjust = 0) +
  xkcdline(aes(x = x1, y = y1, xend = xends, yend=yends), datalinesage, yjitteramount = 0.13) +
  geom_segment(aes(x = 3.88, xend = 4, y = 0, yend = 0), arrow = arrow(length = unit(0.0175, "npc")))


get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

l <- get_png("leopard.png")
t = grid::roundrectGrob()

life_in_months_leopard = life_in_months_initial_annotations +
  annotation_custom(l,
                    xmin = 28,
                    xmax = 32,
                    ymin = 10,
                    ymax = 15)

## life in months colored annotation

life_in_months_titled = life_in_months_leopard +
  annotate("text", x = 29.75, y = 6.5, label = "m", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#f30189") +
  annotate("text", x = 31.25, y = 6.5, label = "y l", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#f08421") +
  annotate("text", x = 33.50, y = 6.5, label = "i", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#ffeb00") +
  annotate("text", x = 34, y = 6.5, label = "f", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#74c042") +
  annotate("text", x = 34.75, y = 6.5, label = "e i", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#02adf2") +
  annotate("text", x =36.75, y = 6.5, label = "n", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#722c8f") +
annotate("text", x = 31, y = 4.5, label = "m", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#f30189")+
  annotate("text", x = 32.50, y = 4.5, label = "o", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#f08421") +
  annotate("text", x = 33.50, y = 4.5, label = "n", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#ffeb00") +
  annotate("text", x = 34.50, y = 4.5, label = "t", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#74c042") +
  annotate("text", x = 35.50, y = 4.5, label = "h", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#02adf2") +
  annotate("text", x =36.55, y = 4.5, label = "s", hjust = 0, family = "Pacifico", fontface = "bold", size = annotation_base_size * 1.75, lineheight = .8, color = "#722c8f")





# "Role" annotations ----

role_annotations_y <- -0.25
roles_size <- annotation_base_size * 1.5


# For annotations: x values are the usually ~midpoint of your age (+1) during that era, give or take for some shifting around to fit labels

life_in_months_role_annotations <- life_in_months_titled +
  annotate("text", x = 8.5, y = 0, label = "childhood", family = "Waltograph",
           size = roles_size *1.2,
           color = "#f30189") +
  annotate("text", x = 17.5, y = 0, label = "Fondy", size = roles_size*.75,
color = "#f08421", family = "Filetto") +
  annotate("text", x = 19, y = role_annotations_y - 1.25, label = "UW Madison",
           color = "#fdb910", family = "Wild Hazelnut", size = roles_size* 0.75) +
  annotate("text", x = 19, y = role_annotations_y - 2.25, label = "(biology)",
           size = roles_size * 0.5,
           color = "#fdb910", family = "Wild Hazelnut") +
  geom_curve(aes(x = 21.5, xend = 22, y = -1, yend = 0.35), curvature = 0.4, arrow = arrow(length = unit(0.0175, "npc")), color = "#fdb910") +
  annotate("text", x = 29.25, y = role_annotations_y, label = "masters",
           family = "mono", color = "#5859a9", size = roles_size* .55) +
  annotate("text", x = 29.25, y = role_annotations_y - 1, label = "(also biology)", colour = "#5859a9", size = roles_size * .35, family = "mono") +
  annotate("text", family = "Drunk Handwriting", size = roles_size*.2, label = "Elliott's\non\nCongress",x = 24.25, y = role_annotations_y-1, color = "#74c042") +
  geom_curve(aes(x = 24, xend = 24, y = -0.5, yend = 0.5), arrow = arrow(length = unit(0.0175, "npc")) ,
             color = "#74c042")


# Location annotations ----

location_annotations_y <- 13



life_in_months_final <- life_in_months_role_annotations +
  annotate("text", x = 8, y = location_annotations_y + 0.1, label = "born + raised in Fond du Lac", family = "Pacifico", size = 8) +
  geom_segment(aes(x = 1, xend = 2, y = 13, yend = 13)) +
  geom_segment(aes(x = 14, xend = 18, y = 13, yend = 13)) +
  geom_segment(aes(x = 1, xend = 1, y = 12.75, yend = 13.25)) +
  geom_segment(aes(x = 18, xend = 18, y = 12.75, yend = 13.25)) +
  annotate("text", x = 21, y = location_annotations_y + 1, label = "moved to Tucson AZ", hjust = 0.75, color = "#ffeb00", family = "Wild Hazelnut", size =10) +
  geom_curve(aes(x = 21.5, xend = 22, y = 13.5, yend = 12.6), curvature = -0.5, arrow = arrow(length = unit(0.0175, "npc")), color = "#ffeb00") +
  annotate("text", x = 25, y = location_annotations_y,
                label = "UArizona", color = "#02adf2", family = "mono", size = 8) +
  annotate("text", x = 30.5, y = 1, label = "PhD", family = "Pacifico", size = 8,
           color = "#722c8f")


# Save final plot ----

ggsave("life_in_months.png", plot = life_in_months_final, device = "png", type = "cairo", width = 25, height = 15, dpi = 300)
