################################################################################
## FIGURAS NO ESTILO LaTeX/pgfplots                                           ##
## Fonte Latin Modern (a do LaTeX), moldura completa, marcas para dentro nos ##
## quatro lados e cores padrao do pgfplots: blue, red, brown!60!black, black ##
## Banda de confianca em blue!15; faixas sombreadas em gray!15.             ##
################################################################################
suppressMessages({
  library(zoo); library(dplyr); library(tidyr); library(ggplot2)
  library(gridExtra); library(grid); library(showtext)
})
if (!dir.exists("Dados")) stop("Rode a partir da raiz do projeto (abra Monografia.Rproj no RStudio).")
OUT <- "Figuras_Finais"
W <- 6.3; H <- 24; DPI <- 400

## ------------------------------------------------------------------ fonte do LaTeX
LM <- file.path(Sys.getenv("APPDATA"), "TinyTeX/texmf-dist/fonts/opentype/public/lm/")  # Latin Modern do TinyTeX
font_add("LM", regular = paste0(LM, "lmroman10-regular.otf"), italic = paste0(LM, "lmroman10-italic.otf"))
showtext_auto(); showtext_opts(dpi = DPI)

## ------------------------------------------------------------------ cores padrao do pgfplots / xcolor
AZUL <- "#0000FF"; AZUL_CLARO <- "#6699E6"; PRETO <- "#000000"; CINZA <- "#7F7F7F"; CINZA_CLARO <- "#B3B3B3"
BANDA <- "#D9D9FF"     # blue!15
SOMBRA <- "#E6E6E6"    # gray!10

## ------------------------------------------------------------------ tema
virgula <- function(x) gsub("-", "\u2212", format(x, decimal.mark = ",", big.mark = ".", trim = TRUE))
tema <- theme_bw(base_size = 10, base_family = "LM") +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", linewidth = 0.3, fill = NA),
        axis.ticks = element_line(colour = "black", linewidth = 0.25),
        axis.ticks.length = unit(-3, "pt"),
        axis.text.x = element_text(colour = "black", size = 9, margin = margin(t = 5)),
        axis.text.y = element_text(colour = "black", size = 9, margin = margin(r = 5)),
        axis.text.x.top = element_blank(), axis.text.y.right = element_blank(),
        axis.title = element_text(size = 9.5),
        plot.title = element_text(size = 10, hjust = 0, margin = margin(b = 3)),
        legend.position = "inside", legend.position.inside = c(0.98, 0.04), legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", colour = "black", linewidth = 0.25),
        legend.title = element_blank(), legend.text = element_text(size = 8.5),
        legend.key.width = unit(20, "pt"), legend.key.height = unit(10, "pt"),
        legend.margin = margin(2, 5, 2, 4), legend.spacing.y = unit(0, "pt"),
        plot.margin = margin(5, 8, 3, 3))
## cada figura sai em PNG (400 dpi) e em SVG vetorial (texto convertido em contornos pelo showtext)
dir.create(file.path(OUT, "svg"), showWarnings = FALSE)
salva <- function(g, f, h) {
  for (formato in c("png", "svg")) {
    if (formato == "png") png(file.path(OUT, f), width = W, height = h, units = "in", res = DPI, bg = "white")
    else svg(file.path(OUT, "svg", sub("\\.png$", ".svg", f)), width = W, height = h, bg = "white")
    showtext_opts(dpi = if (formato == "png") DPI else 72)
    showtext_begin()
    if (inherits(g, "ggplot")) print(g) else grid.draw(g)
    showtext_end(); invisible(dev.off())
  }
}
LW <- 0.55
zero <- geom_hline(yintercept = 0, linewidth = 0.25, linetype = "dashed", colour = "black")
eixo_y <- function(...) scale_y_continuous(labels = virgula, sec.axis = dup_axis(name = NULL, labels = NULL), ...)
eixo_h <- scale_x_continuous(breaks = seq(0, H, 6), limits = c(0, H), expand = c(0, 0), sec.axis = dup_axis(name = NULL))
eixo_data <- function(anos) scale_x_date(breaks = as.Date(paste0(anos, "-01-01")), date_labels = "%Y", expand = c(0, 0), sec.axis = dup_axis(name = NULL))
sombras <- function(f) {
  df <- data.frame(xmin = f[c(TRUE, FALSE)], xmax = f[c(FALSE, TRUE)])
  geom_rect(data = df, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = SOMBRA)
}

## ------------------------------------------------------------------ Capitulo 3 (dados do SGS em cache)
d3 <- readRDS("Dados/dados_cap3.rds")
rec <- as.Date(c("2008-09-01", "2009-06-01", "2014-01-01", "2016-12-01", "2020-03-01", "2020-09-01"))
aperto <- as.Date(c("2015-01-01", "2016-12-31", "2022-03-01", "2023-08-31"))

## Figura 2
df <- bind_rows(mutate(d3$livre, s = "Crédito livre"), mutate(d3$dir, s = "Crédito direcionado")) %>%
  mutate(s = factor(s, c("Crédito livre", "Crédito direcionado")))
g <- ggplot(df, aes(data, valor, colour = s, linetype = s)) + sombras(rec) + geom_line(linewidth = LW) +
  scale_colour_manual(values = c(AZUL, PRETO)) + scale_linetype_manual(values = c("solid", "solid")) +
  eixo_data(seq(2004, 2026, 4)) + eixo_y() + labs(x = NULL, y = "% do PIB") + tema
salva(g, "latex_fig02_livre_direcionado.png", 3.2)

## Figura 3
g <- ggplot(d3$total, aes(data, valor)) + sombras(rec) + geom_line(linewidth = LW, colour = AZUL) +
  eixo_data(seq(2004, 2026, 4)) + eixo_y() + labs(x = NULL, y = "% do PIB") + tema
salva(g, "latex_fig03_credito_total.png", 3.0)

## Figura 4 (medias moveis de 12 meses)
d2 <- d3$bndes$data
para <- function(s) approx(s$data, s$valor / 1e3, xout = d2, rule = 2)$y
mm12 <- function(x) as.numeric(rollmean(x, 12, fill = NA, align = "right"))
df <- data.frame(data = d2, BNDES = mm12(d3$bndes$valor / 1e3), Habitacional = mm12(para(d3$imob)), Rural = mm12(para(d3$rural))) %>%
  drop_na() %>% pivot_longer(-data, names_to = "s", values_to = "valor") %>% mutate(s = factor(s, c("BNDES", "Habitacional", "Rural")))
g <- ggplot(df, aes(data, valor, colour = s, linetype = s)) + sombras(aperto) + geom_line(linewidth = LW) +
  scale_colour_manual(values = c(AZUL, PRETO, CINZA)) + scale_linetype_manual(values = c("solid", "solid", "solid")) +
  eixo_data(seq(2012, 2026, 2)) + eixo_y() + labs(x = NULL, y = "R$ bilhões") + tema +
  theme(legend.position.inside = c(0.02, 0.97), legend.justification = c(0, 1))
salva(g, "latex_fig04_concessoes.png", 3.2)

## Figura 5
dd <- d3$tx_total$data
ap <- function(s) approx(s$data, s$valor, xout = dd, rule = 2)$y
df <- data.frame(data = dd, Total = d3$tx_total$valor, `Livre – PF` = ap(d3$tx_livre_pf), `Livre – PJ` = ap(d3$tx_livre_pj),
                 `Direcionado – PF` = ap(d3$tx_dir_pf), `Direcionado – PJ` = ap(d3$tx_dir_pj), check.names = FALSE) %>%
  pivot_longer(-data, names_to = "s", values_to = "valor") %>%
  mutate(s = factor(s, c("Total", "Livre – PF", "Livre – PJ", "Direcionado – PF", "Direcionado – PJ")))
g <- ggplot(df, aes(data, valor, colour = s, linetype = s, linewidth = s)) + sombras(aperto) + geom_line() +
  scale_colour_manual(values = c(PRETO, AZUL, AZUL_CLARO, CINZA, CINZA_CLARO)) +
  scale_linetype_manual(values = rep("solid", 5)) +
  scale_linewidth_manual(values = c(0.8, LW, LW, LW, LW)) +
  eixo_data(seq(2012, 2026, 2)) + eixo_y() + labs(x = NULL, y = "% a.a.") + tema +
  theme(legend.position = "bottom", legend.background = element_blank(), legend.box.spacing = unit(4, "pt")) +
  guides(colour = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1), linewidth = guide_legend(nrow = 1))
salva(g, "latex_fig05_taxas_segmento.png", 3.5)

## ------------------------------------------------------------------ Figura 6 - series do modelo
base <- readRDS("Dados/base_canal_credito.rds") %>% filter(datas >= as.Date("2011-03-01"), datas <= as.Date("2025-08-01")) %>%
  dplyr::select(datas, IBC, IPCA, SaldoReal, Meta, Spread, Cambio, idx, saldo_nom) %>% na.omit()
nat <- base %>% transmute(datas,
  `A. IBC-Br (índice, dessazonalizado)` = exp(IBC / 100),
  `B. IPCA (índice, mar./2011 = 100)` = 100 * idx / idx[1],
  `C. Saldo real do crédito livre (R$ tri)` = saldo_nom * idx[n()] / idx / 1e6,
  `D. Meta Selic (% a.a.)` = Meta,
  `E. Spread do crédito livre (p.p.)` = Spread,
  `F. Câmbio real (índice)` = exp(Cambio / 100))
pn <- lapply(names(nat)[-1], function(v)
  ggplot(nat, aes(datas, .data[[v]])) + geom_line(linewidth = 0.45, colour = AZUL) + eixo_data(seq(2012, 2026, 2)) + eixo_y() +
    labs(x = NULL, y = NULL, title = v) + tema + theme(axis.text.x = element_text(size = 8.5, margin = margin(t = 5))))
salva(arrangeGrob(grobs = pn, ncol = 2), "latex_fig06_series_modelo.png", 6.6)

## ------------------------------------------------------------------ Figuras do modelo
load("Resultados/modelo_final.RData")
irf_plot <- function(d, ylab, titulo = NULL, xlab = "Meses após o choque") {
  ggplot(d, aes(h)) + geom_ribbon(aes(ymin = lo, ymax = hi), fill = BANDA) + zero +
    geom_line(aes(y = resp), linewidth = LW + 0.1, colour = AZUL) + eixo_h + eixo_y() +
    labs(x = xlab, y = ylab, title = titulo) + tema
}
salva(irf_plot(res[["Meta -> Spread"]], "Pontos percentuais"), "latex_fig07_irf_spread.png", 3.0)
salva(irf_plot(res[["Meta -> SaldoReal"]], "Variação (%)"), "latex_fig08_irf_saldo.png", 3.0)
salva(irf_plot(res[["SaldoReal -> IBC"]], "Variação (%)"), "latex_fig10_irf_saldo_ibc.png", 3.0)

grade <- function(ps) arrangeGrob(arrangeGrob(grobs = ps, ncol = 2),
                                  textGrob("Meses após o choque", gp = gpar(fontfamily = "LM", fontsize = 9.5)), ncol = 1, heights = c(10, 0.45))

## Figura 9 - projecoes locais (azul) x VAR (vermelho tracejado)
lp_p <- function(lp, var_d, titulo, ylab, legenda = FALSE) {
  g <- ggplot(lp, aes(h)) + geom_ribbon(aes(ymin = lo, ymax = hi), fill = BANDA) + zero +
    geom_line(aes(y = resp, colour = "Projeções locais", linetype = "Projeções locais"), linewidth = LW + 0.1) +
    geom_line(data = var_d, aes(h, resp, colour = "VAR", linetype = "VAR"), linewidth = LW + 0.1) +
    scale_colour_manual(name = NULL, values = c("Projeções locais" = AZUL, "VAR" = PRETO)) +
    scale_linetype_manual(name = NULL, values = c("Projeções locais" = "solid", "VAR" = "solid")) +
    eixo_h + eixo_y() + labs(x = NULL, y = ylab, title = titulo) + tema
  if (legenda) g + theme(legend.position.inside = c(0.03, 0.97), legend.justification = c(0, 1)) else g + theme(legend.position = "none")
}
salva(grade(list(lp_p(lp_meta$Spread, res[["Meta -> Spread"]], "A. Selic \u2192 spread do crédito livre", "p.p.", TRUE),
                 lp_p(lp_meta$SaldoReal, res[["Meta -> SaldoReal"]], "B. Selic \u2192 saldo real do crédito livre", "%"),
                 lp_p(lp_saldo$IBC, res[["SaldoReal -> IBC"]], "C. Saldo real do crédito \u2192 IBC-Br", "%"),
                 lp_p(lp_meta$IPCA, res[["Meta -> IPCA"]], "D. Selic \u2192 IPCA", "%"))), "latex_fig09_lp_vs_var.png", 5.2)

## Figura 12 - respostas complementares
salva(grade(list(irf_plot(res[["Meta -> IBC"]], "%", "A. Selic \u2192 IBC-Br", NULL),
                 irf_plot(res[["Meta -> IPCA"]], "%", "B. Selic \u2192 IPCA", NULL),
                 irf_plot(res[["Meta -> Cambio"]], "%", "C. Selic \u2192 câmbio real", NULL),
                 irf_plot(res[["Spread -> IBC"]], "%", "D. Spread \u2192 IBC-Br", NULL))),
      "latex_fig11_complementares.png", 5.0)

## Figura 11 - decomposicao da variancia (cinza com destaque no canal de credito)
suppressMessages(library(vars))
fv <- fevd(v, n.ahead = H)
nomes <- c(IBC = "IBC-Br", IPCA = "IPCA", SaldoReal = "Saldo real do crédito", Meta = "Meta Selic", Spread = "Spread", Cambio = "Câmbio real")
cores <- setNames(c("#E0E0E0", "#C4C4C4", "#0000FF", "#000000", "#7FA7FF", "#A8A8A8"), nomes)
fevd_p <- function(var, tit) {
  d <- as.data.frame(fv[[var]] * 100); d$h <- 1:H
  d <- tidyr::pivot_longer(d, -h, names_to = "choque", values_to = "p")
  d$choque <- factor(nomes[d$choque], levels = nomes)
  ggplot(d, aes(h, p, fill = choque)) + geom_area(colour = "white", linewidth = 0.15) +
    scale_fill_manual(values = cores, name = NULL) +
    scale_x_continuous(breaks = c(1, 6, 12, 18, 24), expand = c(0, 0), sec.axis = dup_axis(name = NULL)) +
    scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x, "%"), sec.axis = dup_axis(name = NULL, labels = NULL)) +
    labs(x = NULL, y = NULL, title = tit) + tema +
    theme(legend.position = "bottom", legend.background = element_blank(), legend.key.size = unit(9, "pt"),
          legend.text = element_text(size = 8.5, margin = margin(r = 10)), panel.ontop = TRUE, panel.background = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}
pa <- fevd_p("IBC", "A. IBC-Br"); pb <- fevd_p("Spread", "B. Spread do crédito livre")
g0 <- ggplotGrob(pa); leg <- g0$grobs[[which(sapply(g0$grobs, function(x) x$name) == "guide-box")]]
salva(arrangeGrob(arrangeGrob(pa + theme(legend.position = "none"), pb + theme(legend.position = "none"), ncol = 2),
                  textGrob("Horizonte (meses)", gp = gpar(fontfamily = "LM", fontsize = 9.5)), leg,
                  ncol = 1, heights = c(10, 0.55, 1.3)), "latex_fig_fevd.png", 3.9)
cat("ok\n")

## Figura 13 - contraste: credito livre (modelo principal) x credito total (2003-2025)
suppressMessages(library(readxl))
bt <- readRDS("Dados/base_canal_credito.rds") %>%
  left_join(read_excel("Dados/dados_var1.xlsx", sheet = "Sheet1") %>% mutate(datas = as.Date(datas)) %>% transmute(datas, CredTotal = credito_pib), by = "datas") %>%
  filter(datas >= as.Date("2003-01-01"), datas <= as.Date("2025-08-01")) %>% dplyr::select(datas, IBC, IPCA, CredTotal, Meta, Cambio) %>% na.omit()
dd <- bt$datas; w <- function(a, b) as.numeric(dd >= as.Date(a) & dd <= as.Date(b))
S <- sapply(1:11, function(m) as.numeric(as.numeric(format(dd, "%m")) == m) - 1 / 12); colnames(S) <- paste0("saz", 1:11)
E <- cbind(d2008 = w("2008-09-01","2009-06-01"), d2015 = w("2015-01-01","2015-12-01"), d2016 = w("2016-01-01","2016-12-01"),
           d2020 = w("2020-03-01","2020-12-01"), greve18 = w("2018-05-01","2018-07-01"), elei18 = w("2018-09-01","2018-11-01"),
           fisc21 = w("2021-10-01","2021-12-01"))
DT <- cbind(S, E)
vt <- VAR(as.matrix(bt[, -1]), p = 5, type = "const", exogen = DT)
set.seed(1234)
ot <- irf(vt, impulse = "Meta", response = "CredTotal", n.ahead = H, ortho = TRUE, boot = TRUE, runs = 5000, ci = 0.95)
tot <- data.frame(h = 0:H, resp = ot$irf$Meta[, 1], lo = ot$Lower$Meta[, 1], hi = ot$Upper$Meta[, 1])
cat("credito total: sig =", paste(tot$h[tot$lo > 0 | tot$hi < 0], collapse = ","), "| choque Meta:", round(sqrt(summary(vt)$covres["Meta", "Meta"]), 3), "\n")
pA <- irf_plot(res[["Meta -> SaldoReal"]], "%", "A. Crédito livre, saldo real (2011–2025)", NULL)
pB <- irf_plot(tot, "p.p. do PIB", "B. Crédito total, % do PIB (2003–2025)", NULL)
salva(grade(list(pA, pB)), "latex_fig13_livre_total.png", 3.15)
cat("ok13\n")
