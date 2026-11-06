# Usage: Rscript release/render-panel-plots.R ROOT
root <- normalizePath(commandArgs(trailingOnly = TRUE)[[1]], mustWork = TRUE)
.libPaths(c(file.path(root, paste0("lib-", c("interactions", "panelr", "jtools", "current"))), .libPaths()))
library(panelr)
library(interactions)
output <- file.path(root, "visual-review", "panelr")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
wages <- panel_data(WageData, id = id, wave = t)
cross <- wbm(lwage ~ wks | ed | wks * ed, wages, pR2 = FALSE, pvals = FALSE)
within <- wbm(lwage ~ wks * exp, wages, pR2 = FALSE, pvals = FALSE)
scaled <- wbm(lwage ~ wks * exp, wages, scale = TRUE, pR2 = FALSE, pvals = FALSE)
plots <- list(
  cross_level = interact_plot(cross, wks, ed, modx.values = c(9, 12, 16), interval = TRUE),
  within = interact_plot(within, wks, exp, modx.values = c(-2, 0, 2), interval = TRUE),
  original = interact_plot(within, wks, exp, modx.values = c(18, 20, 22),
                            plot.scale = "original", interval = TRUE),
  standardized = interact_plot(scaled, wks, exp, modx.values = c(-2, 0, 2), interval = TRUE),
  slopes = plot(sim_slopes(within, wks, exp, modx.values = c(-2, 0, 2))),
  johnson_neyman = johnson_neyman(within, wks, exp)$plot
)
for (name in names(plots)) {
  ggplot2::ggsave(file.path(output, paste0(name, ".png")), plots[[name]],
                  width = 8, height = 5.5, dpi = 150, bg = "white")
}
saveRDS(plots, file.path(output, "plots.rds"))
cat("Rendered", length(plots), "plots to", output, "\n")
