################################################################################
## FIGURA 1 - MECANISMOS DE TRANSMISSAO DA POLITICA MONETARIA                 ##
## Redesenho do diagrama de Marcatti (2011) no estilo das demais figuras      ##
## (fonte Latin Modern), em PNG 400 dpi e SVG vetorial.                       ##
################################################################################
suppressMessages({ library(ggplot2); library(showtext) })
if (!dir.exists("Dados")) stop("Rode a partir da raiz do projeto (abra Monografia.Rproj no RStudio).")
OUT <- "Figuras_Finais"; W <- 6.3; HT <- 2.9; DPI <- 400
LM <- file.path(Sys.getenv("APPDATA"), "TinyTeX/texmf-dist/fonts/opentype/public/lm/")  # Latin Modern do TinyTeX
font_add("LM", regular = paste0(LM, "lmroman10-regular.otf"))
showtext_auto()

## caixas: centro (x, y), largura, rotulo; altura comum
AH <- 6.4
cx <- data.frame(
  id  = c("juros", "choques", "mercado", "ativos", "expect", "credito", "cambio", "invest", "consumo", "export", "precos", "demanda", "inflacao"),
  x   = c(9, 9, 30, 30, 30, 30, 30, 55, 55, 55, 55, 75, 92),
  y   = c(35, 8, 44, 35, 26, 17, 8, 36, 26, 16, 6.6, 26, 26),
  w   = c(15, 15, 16, 16, 16, 16, 16, 18, 18, 18, 18, 12.5, 11),
  lab = c("Taxa de juros", "Choques\nexternos", "Taxas de\nmercado", "Preço dos\nativos", "Expectativas", "Crédito", "Taxa de\ncâmbio",
          "Investimento\nprivado", "Consumo de\nbens duráveis", "Exportações\nlíquidas", "Preços\nexternos", "Demanda\nagregada", "Inflação"))
cx$x0 <- cx$x - cx$w / 2; cx$x1 <- cx$x + cx$w / 2; cx$y0 <- cx$y - AH / 2; cx$y1 <- cx$y + AH / 2
b <- function(id, lado) { r <- cx[cx$id == id, ]; switch(lado, l = r$x0, r = r$x1, t = r$y1, b = r$y0) }
yb <- function(id) cx$y[cx$id == id]

seta <- arrow(length = unit(3.6, "pt"), type = "closed")
LW <- 0.32
seg <- function(x, y, xend, yend, arr = FALSE) data.frame(x, y, xend, yend, arr)
bus1 <- 19.2; bus2 <- 41.5; bus3 <- 66.3
s <- rbind(
  # taxa de juros -> barramento -> cinco canais
  seg(b("juros", "r"), yb("juros"), bus1, yb("juros")),
  seg(bus1, yb("cambio") + 1.4, bus1, yb("mercado")),
  do.call(rbind, lapply(c("mercado", "ativos", "expect", "credito"), function(k) seg(bus1, yb(k), b(k, "l"), yb(k), TRUE))),
  seg(bus1, yb("cambio") + 1.4, b("cambio", "l"), yb("cambio") + 1.4, TRUE),
  # choques externos -> taxa de juros e -> taxa de cambio
  seg(9, b("choques", "t"), 9, b("juros", "b"), TRUE),
  seg(b("choques", "r"), yb("cambio") - 1.4, b("cambio", "l"), yb("cambio") - 1.4, TRUE),
  # canais -> barramento -> componentes da demanda
  do.call(rbind, lapply(c("mercado", "ativos", "expect", "credito"), function(k) seg(b(k, "r"), yb(k), bus2, yb(k)))),
  seg(b("cambio", "r"), yb("cambio") + 1.4, bus2, yb("cambio") + 1.4),
  seg(bus2, yb("cambio") + 1.4, bus2, yb("mercado")),
  do.call(rbind, lapply(c("invest", "consumo", "export"), function(k) seg(bus2, yb(k), b(k, "l"), yb(k), TRUE))),
  # componentes -> demanda agregada -> inflacao
  do.call(rbind, lapply(c("invest", "consumo", "export"), function(k) seg(b(k, "r"), yb(k), bus3, yb(k)))),
  seg(bus3, yb("export"), bus3, yb("invest")),
  seg(bus3, yb("demanda"), b("demanda", "l"), yb("demanda"), TRUE),
  seg(b("demanda", "r"), yb("demanda"), b("inflacao", "l"), yb("inflacao"), TRUE),
  # cambio -> precos externos -> inflacao
  seg(b("cambio", "r"), yb("cambio") - 1.4, b("precos", "l"), yb("precos"), TRUE),
  seg(b("precos", "r"), yb("precos"), 92, yb("precos")),
  seg(92, yb("precos"), 92, b("inflacao", "b"), TRUE))

g <- ggplot() +
  geom_segment(data = s[!s$arr, ], aes(x, y, xend = xend, yend = yend), linewidth = LW, lineend = "square") +
  geom_segment(data = s[s$arr, ], aes(x, y, xend = xend, yend = yend), linewidth = LW, arrow = seta, linejoin = "mitre") +
  geom_rect(data = cx, aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1), fill = "white", colour = "black", linewidth = LW) +
  geom_text(data = cx, aes(x, y, label = lab), family = "LM", size = 8.3 / .pt, lineheight = 0.88) +
  coord_cartesian(xlim = c(0.8, 98.2), ylim = c(2.6, 48), expand = FALSE) +
  theme_void() + theme(plot.background = element_rect(fill = "white", colour = NA), plot.margin = margin(2, 2, 2, 2))

for (formato in c("png", "svg")) {
  if (formato == "png") png(file.path(OUT, "latex_fig01_mecanismos.png"), width = W, height = HT, units = "in", res = DPI, bg = "white")
  else { dir.create(file.path(OUT, "svg"), showWarnings = FALSE); svg(file.path(OUT, "svg", "latex_fig01_mecanismos.svg"), width = W, height = HT, bg = "white") }
  showtext_opts(dpi = if (formato == "png") DPI else 72)
  showtext_begin(); print(g); showtext_end(); invisible(dev.off())
}
cat("ok\n")
