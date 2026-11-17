################################################################################
## FIGURA - RESPOSTA DO CREDITO LIVRE E DO DIRECIONADO A UM CHOQUE NA SELIC   ##
## Modelo conjunto (script 04), mesmo estilo LaTeX/pgfplots das demais.       ##
################################################################################
suppressMessages({ library(ggplot2); library(gridExtra); library(grid); library(showtext) })
if (!dir.exists("Dados")) stop("Rode a partir da raiz do projeto (abra Monografia.Rproj no RStudio).")
OUT <- "Figuras_Finais"
W <- 6.3; H <- 24; DPI <- 400
LM <- file.path(Sys.getenv("APPDATA"), "TinyTeX/texmf-dist/fonts/opentype/public/lm/")  # Latin Modern do TinyTeX
font_add("LM", regular = paste0(LM, "lmroman10-regular.otf"), italic = paste0(LM, "lmroman10-italic.otf"))
showtext_auto(); showtext_opts(dpi = DPI)
AZUL <- "#0000FF"; BANDA <- "#D9D9FF"; LW <- 0.55
virgula <- function(x) gsub("-", "\u2212", format(x, decimal.mark = ",", big.mark = ".", trim = TRUE))
tema <- theme_bw(base_size = 10, base_family = "LM") +
  theme(panel.grid = element_blank(), panel.border = element_rect(colour = "black", linewidth = 0.3, fill = NA),
        axis.ticks = element_line(colour = "black", linewidth = 0.25), axis.ticks.length = unit(-3, "pt"),
        axis.text.x = element_text(colour = "black", size = 9, margin = margin(t = 5)),
        axis.text.y = element_text(colour = "black", size = 9, margin = margin(r = 5)),
        axis.text.x.top = element_blank(), axis.text.y.right = element_blank(), axis.title = element_text(size = 9.5),
        plot.title = element_text(size = 10, hjust = 0, margin = margin(b = 3)), plot.margin = margin(5, 8, 3, 3))
zero <- geom_hline(yintercept = 0, linewidth = 0.25, linetype = "dashed", colour = "black")
eixo_h <- scale_x_continuous(breaks = seq(0, H, 6), limits = c(0, H), expand = c(0, 0), sec.axis = dup_axis(name = NULL))
irf_plot <- function(d, ylab, titulo, lim) ggplot(d, aes(h)) + geom_ribbon(aes(ymin = lo, ymax = hi), fill = BANDA) + zero +
  geom_line(aes(y = resp), linewidth = LW + 0.1, colour = AZUL) + eixo_h +
  scale_y_continuous(labels = virgula, limits = lim, sec.axis = dup_axis(name = NULL, labels = NULL)) +
  labs(x = NULL, y = ylab, title = titulo) + tema

load("Resultados/robustez_direcionado.RData")
lim <- range(unlist(lapply(list(resC$`Meta -> SaldoReal`, resC$`Meta -> DirReal`), function(d) c(d$lo, d$hi))))
ps <- list(irf_plot(resC$`Meta -> SaldoReal`, "%", "A. Crédito livre", lim),
           irf_plot(resC$`Meta -> DirReal`, "%", "B. Crédito direcionado", lim),
           irf_plot(difC, "p.p.", "C. Diferença", NULL))
g <- arrangeGrob(arrangeGrob(grobs = ps, ncol = 3),
                 textGrob("Meses após o choque", gp = gpar(fontfamily = "LM", fontsize = 9.5)), ncol = 1, heights = c(10, 0.6))
png(file.path(OUT, "latex_fig12_livre_direcionado.png"), width = W, height = 2.4, units = "in", res = DPI, bg = "white")
showtext_begin(); grid.draw(g); showtext_end(); invisible(dev.off())
dir.create(file.path(OUT, "svg"), showWarnings = FALSE)
showtext_opts(dpi = 72)
svg(file.path(OUT, "svg", "latex_fig12_livre_direcionado.svg"), width = W, height = 2.4, bg = "white")
showtext_begin(); grid.draw(g); showtext_end(); invisible(dev.off())
cat("ok\n")
