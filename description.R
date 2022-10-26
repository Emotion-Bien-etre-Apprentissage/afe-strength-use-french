# load packages 

library(tidyverse)
library(readxl)
library(gt)

# import
d <- read_excel(path = "data/bdd_master.xlsx")

# Calcul des N - samp

samp <- d |> 
  group_by(bdd) |>
  summarize(n = n(),
            remove = sum(is.na(su_1)),
            remain = sum(!is.na(su_1))
           )

# Préparation du tableau APA style avec gt - apa_table
apa_table <- samp |> 
  gt() |> 
    tab_options(
      table.border.top.color = "white",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(100),
      table.background.color = "white"
    ) |> 
    cols_align(align="center") |> 
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_text(
          align="center"
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) |> 
    tab_header(
      title = md("**Salut**")
    ) |> 
    opt_align_table_header(align = "left") |> 
  cols_label(
    bdd = md("**Base de données**"),
    n = md("**N**"),
    remove = md("**numbers of na's**"),
    remain = md("**Final**")
  )


gtsave(data = apa_table, filename = "ciao.docx")

apa_table

# Calcul des N dans chaque sus_ - samp2

samp2 <- d |> 
  group_by(bdd) |>
  summarize(n = n(),
            remove_su1 = sum(is.na(su_1)),
            remove_su2 = sum(is.na(su_2)),
            remove_su3 = sum(is.na(su_3)),
            
  )
samp2
