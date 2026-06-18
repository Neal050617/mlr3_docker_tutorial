library(tidyverse)
d1 <- tribble(
    ~d, ~name, ~filename,
    "p", "phylum", "phylum.percents.xls",
    "c", "class", "class.percents.xls",
    "o", "order", "order.percents.xls",
    "f", "family", "family.percents.xls",
    "g", "genus", "genus.percents.xls",
    "s", "species", "species.percents.xls"
)

d2 <- lapply(1:nrow(d1), function(i) {
    read_tsv(as.character(d1[i,3])) |>
      #mutate(!!as.character(d1[i,2]) := paste0(as.character(d1[i,1]),"__",!!sym(as.character(d1[i,2])))) |>
      mutate(Taxon = paste0(as.character(d1[i,1]),"__",Taxon)) |>
      rename_at(1,~"id")
})

d3 <- bind_rows(d2) |> filter(!grepl("Incertae_sedis|Fungi_sp", id)) |> filter(!grepl("Incertae_sedis|Fungi_sp", id))

write_tsv(d3,"alltax.percent.xls")





