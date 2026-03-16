# source("testing/render_reports.R")

library(quarto)
library(sf)

for (i in seq_len(nrow(sample_dados))) {
  # row <- sample_dados[1, ]
  row <- sample_dados[i, ]

  quarto_render(
    input = "testing/XX_feature_report.qmd",
    output_file = sprintf("feature_%s.pdf", row$group_id),
    execute_params = list(
      title = paste("Group ID", row$group_id),
      text_blocks = list(
        # paste0("**Surface problem (gradação 0.0 a 1.0):** ", row$surfaceproblem_prop, " (", row$frames, " frames)"),
        paste0("**SQL:** ", row$n_contrib, " | **Cond:** ", row$n_cond, " | **CODLOG:** ", row$codlog, " | **CEP:** ", row$cep),
        paste0("**Logradouro:** ", row$logradouro, ", ", row$lado, ", Altura N° ", as.integer(row$numero)),
        paste0("**Links:**
        [Localização no Google Maps](https://www.google.com/maps?q=",
               st_coordinates(row$geom)[, 2], ",",
               st_coordinates(row$geom)[, 1],
               ") |  [Ver no Google Street View](http://maps.google.com/?cbll=",
        st_coordinates(row$geom)[, 2], ",",
        st_coordinates(row$geom)[, 1],
        "&cbp=12,20.09,,0,5&layer=c)"),
        paste0(" ")
      ),
      imagepath = row$imagepath
    )
  )
}


