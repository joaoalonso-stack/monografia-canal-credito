################################################################################
## COMPLEMENTOS DO MODELO FINAL (Cap. 4 e apendices)                         ##
##  - Figura: series do modelo (2011-2025)                                   ##
##  - Tabela: estatisticas descritivas                                       ##
##  - Tabela: testes de raiz unitaria (ADF e KPSS, nivel e diferenca)        ##
##  - Tabela: posto de cointegracao de Johansen por especificacao            ##
################################################################################

suppressMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(urca)
  library(gridExtra); library(grid); library(gtable)
})
if (!dir.exists("Dados")) stop("Rode a partir da raiz do projeto (abra Monografia.Rproj no RStudio).")
OUT <- "Resultados/Figuras_Diagnostico"

base <- readRDS("Dados/base_canal_credito.rds")
ORD <- c("IBC", "IPCA", "SaldoReal", "Meta", "Spread", "Cambio")
dd <- base %>% filter(datas >= as.Date("2011-03-01")) %>% dplyr::select(datas, all_of(ORD), idx, saldo_nom) %>% na.omit()
y <- as.matrix(dd[, ORD])

## Series em unidades naturais (o modelo usa 100*log das series em indice/valor)
nat <- dd %>% transmute(
  datas,
  `IBC-Br (índice, dessaz.)` = exp(IBC / 100),
  `IPCA (índice, mar/2011 = 100)` = 100 * idx / idx[1],
  `Saldo real do crédito livre (R$ tri de ago/2025)` = saldo_nom * idx[n()] / idx / 1e6,
  `Meta Selic (% a.a.)` = Meta,
  `Spread do crédito livre (p.p.)` = Spread,
  `Câmbio real (índice)` = exp(Cambio / 100))
cat("Saldo real: de", round(nat[[4]][1], 2), "a", round(tail(nat[[4]], 1), 2), "R$ tri\n")

## ---------- Figura: series do modelo ----------
tema <- theme_bw(base_size = 12, base_family = "serif") +
  theme(axis.text = element_text(size = 9.5, color = "black"), panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey88", linewidth = 0.3),
        panel.border = element_rect(color = "black", linewidth = 0.4, fill = NA),
        strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.4),
        strip.text = element_text(face = "bold", size = 9.5, color = "black"))
lv <- names(nat)[-1]
g <- nat %>% pivot_longer(-datas, names_to = "serie", values_to = "valor") %>%
  mutate(serie = factor(serie, levels = lv)) %>%
  ggplot(aes(datas, valor)) + geom_line(linewidth = 0.55) +
  facet_wrap(~serie, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") + labs(x = NULL, y = NULL) + tema
ggsave(file.path(OUT, "final_series_modelo.png"), g, width = 9, height = 8, dpi = 320, bg = "white")

## ---------- Tabelas (padrao booktabs) ----------
save_tab <- function(df, file_out, fontsize = 10.5) {
  tt <- ttheme_minimal(core = list(fg_params = list(fontfamily = "serif", fontsize = fontsize), padding = unit(c(10, 6), "pt")),
                       colhead = list(fg_params = list(fontfamily = "serif", fontsize = fontsize, fontface = "bold"), padding = unit(c(10, 6), "pt")))
  g <- tableGrob(df, rows = NULL, theme = tt)
  for (ci in which(g$layout$name %in% c("core-fg", "colhead-fg") & g$layout$l == 1)) { g$grobs[[ci]]$hjust <- 0; g$grobs[[ci]]$x <- unit(0.02, "npc") }
  hr <- min(g$layout$t[g$layout$name == "colhead-fg"]); lr <- max(g$layout$t); r <- ncol(df)
  g <- gtable_add_grob(g, segmentsGrob(0, 1, 1, 1, gp = gpar(lwd = 1.6)), t = hr, l = 1, r = r, name = "top")
  g <- gtable_add_grob(g, segmentsGrob(0, 0, 1, 0, gp = gpar(lwd = 0.9)), t = hr, l = 1, r = r, name = "mid")
  g <- gtable_add_grob(g, segmentsGrob(0, 0, 1, 0, gp = gpar(lwd = 1.6)), t = lr, l = 1, r = r, name = "bot")
  w <- convertWidth(sum(g$widths), "in", TRUE) + 0.25; h <- convertHeight(sum(g$heights), "in", TRUE) + 0.15
  png(file.path(OUT, file_out), width = w, height = h, units = "in", res = 320, bg = "white"); grid.newpage(); grid.draw(g); dev.off()
}
br <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")

## Estatisticas descritivas
nomes <- c("IBC-Br", "IPCA", "Saldo real do crédito livre", "Meta Selic", "Spread do crédito livre", "Câmbio real")
unid  <- c("índice", "índice (mar/2011 = 100)", "R$ tri de ago/2025", "% a.a.", "p.p.", "índice")
desc <- data.frame(Variável = nomes, Unidade = unid, N = nrow(nat),
                   Média = br(sapply(nat[-1], mean)), `Desv.-Pad.` = br(sapply(nat[-1], sd)),
                   Mínimo = br(sapply(nat[-1], min)), Máximo = br(sapply(nat[-1], max)), check.names = FALSE)
print(desc)
save_tab(desc, "final_tab_descritivas.png", fontsize = 10)

## Raiz unitaria: ADF (defasagens por AIC, max 12) e KPSS (nivel; lags curtos)
rotulo <- c("IBC-Br (log)", "IPCA (log)", "Saldo real (log)", "Meta Selic", "Spread", "Câmbio real (log)")
## nivel: ADF com constante e tendencia; primeira diferenca: ADF com constante
ur <- function(x, tipo) {
  a <- ur.df(x, type = tipo, selectlags = "AIC", lags = 12); k <- ur.kpss(x, type = "mu", lags = "short")
  c(adf = a@teststat[1], kpss = k@teststat)
}
star_adf_t <- function(s) ifelse(s < -3.99, "***", ifelse(s < -3.43, "**", ifelse(s < -3.13, "*", "")))
star_adf <- function(s) ifelse(s < -3.46, "***", ifelse(s < -2.88, "**", ifelse(s < -2.57, "*", "")))
star_kpss <- function(s) ifelse(s > 0.739, "***", ifelse(s > 0.463, "**", ifelse(s > 0.347, "*", "")))
U <- t(sapply(ORD, function(v) c(ur(y[, v], "trend"), ur(diff(y[, v]), "drift"))))
ut <- data.frame(Série = rotulo,
                 `ADF nível` = paste0(br(U[, 1]), star_adf_t(U[, 1])), `KPSS nível` = paste0(br(U[, 2]), star_kpss(U[, 2])),
                 `ADF 1ª dif.` = paste0(br(U[, 3]), star_adf(U[, 3])), `KPSS 1ª dif.` = paste0(br(U[, 4]), star_kpss(U[, 4])),
                 check.names = FALSE)
print(ut)
save_tab(ut, "final_tab_raiz_unitaria.png")

## Johansen: posto selecionado (5%) por defasagens e tratamento das dummies
d <- dd$datas; w <- function(a, b) as.numeric(d >= as.Date(a) & d <= as.Date(b))
E <- cbind(d2015 = w("2015-01-01","2015-12-01"), d2016 = w("2016-01-01","2016-12-01"), d2020 = w("2020-03-01","2020-12-01"),
           greve18 = w("2018-05-01","2018-07-01"), elei18 = w("2018-09-01","2018-11-01"), fisc21 = w("2021-10-01","2021-12-01"))
rank_of <- function(j) { s <- rev(j@teststat); cv <- rev(j@cval[, "5pct"]); r <- 0; for (i in seq_along(s)) { if (s[i] > cv[i]) r <- i else break }; r }
jt <- data.frame()
for (K in 2:6) {
  rr <- sapply(list(E, NULL), function(DV) c(
    rank_of(ca.jo(y, type = "trace", ecdet = "const", K = K, season = 12, dumvar = DV)),
    rank_of(ca.jo(y, type = "eigen", ecdet = "const", K = K, season = 12, dumvar = DV))))
  jt <- rbind(jt, data.frame(Defasagens = K, `Traço, com dummies` = rr[1, 1], `Autovalor, com dummies` = rr[2, 1],
                             `Traço, sem dummies` = rr[1, 2], `Autovalor, sem dummies` = rr[2, 2], check.names = FALSE))
}
print(jt)
save_tab(jt, "final_tab_johansen.png", fontsize = 11)
cat("\nFIM\n")
