################################################################################
## IRFs DO MODELO PRINCIPAL EM HORIZONTE LONGO (60 MESES) - SLIDE DE RESERVA  ##
## Mostra que taxas (Selic, spread, cambio) voltam a zero e que variaveis em  ##
## nivel (IPCA, saldo real) tem efeito persistente. Mesmo estilo das figuras. ##
################################################################################
suppressMessages({ library(vars); library(ggplot2); library(gridExtra); library(grid); library(showtext) })
if (!dir.exists("Dados")) stop("Rode a partir da raiz do projeto (abra Monografia.Rproj no RStudio).")
load("Resultados/modelo_final.RData")
y <- m$y; D <- m$D; P <- 4                      # objetos exigidos pelo bootstrap do pacote vars
args <- commandArgs(trailingOnly = TRUE)
H <- if (length(args)) as.integer(args[1]) else 60; set.seed(1234)
o <- irf(v, impulse = "Meta", response = colnames(y), n.ahead = H, ortho = TRUE, boot = TRUE, runs = 5000, ci = 0.95)
get <- function(k) data.frame(h = 0:H, resp = o$irf$Meta[, k], lo = o$Lower$Meta[, k], hi = o$Upper$Meta[, k])

OUT <- "Resultados/Figuras_Diagnostico"; DPI <- 400
LM <- file.path(Sys.getenv("APPDATA"), "TinyTeX/texmf-dist/fonts/opentype/public/lm/")  # Latin Modern do TinyTeX
font_add("LM", regular = paste0(LM, "lmroman10-regular.otf")); showtext_auto(); showtext_opts(dpi = DPI)
AZUL <- "#0000FF"; BANDA <- "#D9D9FF"; LW <- 0.6
virgula <- function(x) gsub("-", "\u2212", format(x, decimal.mark = ",", big.mark = ".", trim = TRUE))
tema <- theme_bw(base_size = 11, base_family = "LM") +
  theme(panel.grid = element_blank(), panel.border = element_rect(colour = "black", linewidth = 0.3, fill = NA),
        axis.ticks = element_line(colour = "black", linewidth = 0.25), axis.ticks.length = unit(-3, "pt"),
        axis.text.x = element_text(colour = "black", size = 9.5, margin = margin(t = 5)),
        axis.text.y = element_text(colour = "black", size = 9.5, margin = margin(r = 5)),
        axis.text.x.top = element_blank(), axis.text.y.right = element_blank(), axis.title = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = 0, margin = margin(b = 3)), plot.margin = margin(5, 10, 3, 3))
painel <- function(k, titulo, ylab) {
  ggplot(get(k), aes(h)) +
    annotate("rect", xmin = 24, xmax = H, ymin = -Inf, ymax = Inf, fill = "#F2F2F2") +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = BANDA) +
    geom_hline(yintercept = 0, linewidth = 0.25, linetype = "dashed") +
    geom_line(aes(y = resp), linewidth = LW, colour = AZUL) +
    scale_x_continuous(breaks = seq(0, H, if (H > 60) 24 else 12), limits = c(0, H), expand = c(0, 0), sec.axis = dup_axis(name = NULL)) +
    scale_y_continuous(labels = virgula, sec.axis = dup_axis(name = NULL, labels = NULL)) +
    labs(x = NULL, y = ylab, title = titulo) + tema
}
ps <- list(painel("Meta", "A. Meta Selic", "p.p."), painel("Spread", "B. Spread do crédito livre", "p.p."),
           painel("Cambio", "C. Câmbio real", "%"), painel("SaldoReal", "D. Saldo real do crédito livre", "%"),
           painel("IPCA", "E. IPCA (nível)", "%"), painel("IBC", "F. IBC-Br", "%"))
g <- arrangeGrob(arrangeGrob(grobs = ps, ncol = 3),
                 textGrob("Meses após o choque (área cinza: além do horizonte de 24 meses usado no trabalho)",
                          gp = gpar(fontfamily = "LM", fontsize = 10)), ncol = 1, heights = c(10, 0.5))
png(file.path(OUT, sprintf("slide_irf_%dmeses.png", H)), width = 10, height = 5.6, units = "in", res = DPI, bg = "white")
showtext_begin(); grid.draw(g); showtext_end(); invisible(dev.off())
for (k in colnames(y)) { d <- get(k); s <- which(d$lo > 0 | d$hi < 0) - 1
  cat(sprintf("%-10s h24 %+.3f h36 %+.3f h60 %+.3f | sig ate 60: %s\n", k, d$resp[25], d$resp[37], d$resp[61],
              if (length(s)) paste(range(s), collapse = "-") else "nenhum")) }
cat("ok\n")
