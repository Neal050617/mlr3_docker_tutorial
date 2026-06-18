library(tidyverse)
d1 <- tribble(
    ~d, ~name, ~filename,
    "p", "phylum", "phylum.percent.xls",
    "c", "class", "class.percent.xls",
    "o", "order", "order.percent.xls",
    "f", "family", "family.percent.xls",
    "g", "genus", "genus.percent.xls",
    "s", "species", "species.percent.xls"
)

d2 <- lapply(1:nrow(d1), function(i) {
    read_tsv(as.character(d1[i,3])) |>
      mutate(!!as.character(d1[i,2]) := paste0(as.character(d1[i,1]),"__",!!sym(as.character(d1[i,2])))) |>
      rename_at(1,~"id")
})

d3 <- bind_rows(d2)

write_tsv(d3,"alltax.percent.xls")





