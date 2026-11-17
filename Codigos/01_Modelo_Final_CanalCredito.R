################################################################################
## MODELO FINAL - CANAL DE CREDITO (CREDITO LIVRE), 2011-2025                ##
## VAR em niveis com identificacao recursiva CEE (Christiano, Eichenbaum e   ##
## Evans, 1999):                                                             ##
##   bloco lento:  IBC-Br > IPCA > Saldo livre real                          ##
##   politica:     Meta Selic (fim de mes)                                   ##
##   bloco rapido: Spread do credito livre > Cambio real                     ##
## p = 4, dummies sazonais centradas + dummies de eventos, IRFs 24m, 95%.    ##
## Robustez: defasagens, series alternativas, amostra 2003+, projecoes      ##
## locais (Jorda, 2005) com erros Newey-West, contrafactual BGW (1997).     ##
################################################################################

suppressMessages({
  library(readxl); library(dplyr); library(vars); library(ggplot2)
  library(gridExtra); library(grid); library(gtable)
})
if (!dir.exists("Dados")) stop("Rode a partir da raiz do projeto (abra Monografia.Rproj no RStudio).")
set.seed(1234)
RUNS <- 5000; RUNS_ROB <- 1000; H <- 24; P <- 4
OUT <- "Resultados/Figuras_Diagnostico"

## =============================================================
## 1. Dados
## base_canal_credito.rds: series do SGS/BCB ja baixadas
##   IBC   = 100*log(IBC-Br dessaz., SGS 24364)
##   IPCA  = 100*log(indice acumulado do IPCA, SGS 433)
##   SaldoReal = 100*log(saldo livre nominal SGS 20542 / indice IPCA)
##   Saldo = saldo livre % PIB (SGS 20625) | Selic = media mensal (SGS 4189)
##   Meta  = meta Selic no ultimo dia do mes (SGS 432)
##   Spread = spread do credito livre (SGS 20786) | Cambio = 100*log(cambio real)
## =============================================================
base <- readRDS("Dados/base_canal_credito.rds") %>%
  left_join(read_excel("Dados/dados_var1.xlsx", sheet = "Sheet1") %>% mutate(datas = as.Date(datas)) %>%
              transmute(datas, CredTotal = credito_pib), by = "datas")

dummies <- function(d, eventos = TRUE) {
  w <- function(a, b) as.numeric(d >= as.Date(a) & d <= as.Date(b))
  S <- sapply(1:11, function(m) as.numeric(as.numeric(format(d, "%m")) == m) - 1 / 12)
  colnames(S) <- paste0("saz", 1:11)
  if (!eventos) return(S)
  E <- cbind(d2008 = w("2008-09-01","2009-06-01"), d2015 = w("2015-01-01","2015-12-01"),
             d2016 = w("2016-01-01","2016-12-01"), d2020 = w("2020-03-01","2020-12-01"),
             greve18 = w("2018-05-01","2018-07-01"), elei18 = w("2018-09-01","2018-11-01"),
             fisc21 = w("2021-10-01","2021-12-01"))
  cbind(S, E[, colSums(E) > 0, drop = FALSE])
}
monta <- function(ord, ini = "2011-03-01", eventos = TRUE) {
  dd <- base %>% filter(datas >= as.Date(ini)) %>% dplyr::select(datas, all_of(ord)) %>% na.omit()
  list(datas = dd$datas, y = as.matrix(dd[, ord]), D = dummies(dd$datas, eventos))
}

ORD <- c("IBC", "IPCA", "SaldoReal", "Meta", "Spread", "Cambio")
m <- monta(ORD); y <- m$y; D <- m$D
cat("Amostra:", format(min(m$datas)), "a", format(max(m$datas)), "| T =", nrow(y), "\n")

## =============================================================
## 2. Selecao de defasagens e diagnostico
## =============================================================
sel <- VARselect(y, lag.max = 8, type = "const", exogen = D)$selection
cat("\nCriterios de informacao: AIC =", sel[1], "| HQ =", sel[2], "| SC =", sel[3], "\n")
cat("p adotado = 4: menor defasagem sem autocorrelacao residual em nenhuma equacao (Ljung-Box, 12 lags)\n")
diag_tab <- data.frame()
for (p in 2:6) {
  v <- VAR(y, p = p, type = "const", exogen = D)
  lb <- apply(resid(v), 2, function(e) Box.test(e, 12, "Ljung-Box")$p.value)
  es <- serial.test(v, lags.bg = 6, type = "ES")$serial$p.value
  diag_tab <- rbind(diag_tab, data.frame(p, t(lb), ES = es, raiz = max(roots(v))))
}
cat("\nDiagnostico (p-valores; Ljung-Box por equacao e LM Edgerton-Shukur do sistema):\n")
print(round(diag_tab, 3), row.names = FALSE)

v <- VAR(y, p = P, type = "const", exogen = D)
cat("\nModelo principal: p =", P, "| raiz maxima =", round(max(roots(v)), 4),
    "| graus de liberdade por equacao =", nrow(resid(v)) - ncol(Bcoef(v)), "\n")
jb <- normality.test(v, multivariate.only = TRUE)$jb.mul$JB$p.value
arch <- arch.test(v, lags.multi = 6, multivariate.only = TRUE)$arch.mul$p.value
cat("Jarque-Bera multivariado p =", signif(jb, 3), "| ARCH-LM multivariado p =", signif(arch, 3), "\n")
sd_choque <- sqrt(crossprod(resid(v))[4, 4] / nrow(resid(v)))
cat("Tamanho do choque de 1 d.p. na meta Selic:", round(sd_choque, 3), "p.p.\n")

## =============================================================
## 3. IRFs do modelo principal (bootstrap, 5000 replicas, 95%)
## =============================================================
irf_all <- function(mod, imps, runs = RUNS) {   # um unico bootstrap para todos os pares
  irf(mod, impulse = imps, response = colnames(mod$y), n.ahead = H, ortho = TRUE, boot = TRUE, runs = runs, ci = 0.95)
}
irf_get <- function(o, imp, resp) {
  d <- data.frame(h = 0:H, resp = o$irf[[imp]][, resp], lo = o$Lower[[imp]][, resp], hi = o$Upper[[imp]][, resp])
  d$sig <- d$lo > 0 | d$hi < 0; d
}
pares <- list(c("Meta", "Spread"), c("Meta", "SaldoReal"), c("SaldoReal", "IBC"), c("Meta", "IBC"),
              c("Meta", "IPCA"), c("Spread", "IBC"), c("Spread", "SaldoReal"), c("Meta", "Cambio"), c("Cambio", "IBC"))
res <- list()
cat("\nBootstrap (", RUNS, "replicas)...\n")
o_main <- irf_all(v, c("Meta", "SaldoReal", "Spread", "Cambio"))
for (pp in pares) {
  nm <- paste(pp, collapse = " -> ")
  res[[nm]] <- irf_get(o_main, pp[1], pp[2])
  d <- res[[nm]]
  cat(sprintf("%-22s %s | meses significativos: %s\n", nm,
              paste(sprintf("h%d %+.3f", c(0, 3, 6, 12, 18, 24), d$resp[c(0, 3, 6, 12, 18, 24) + 1]), collapse = "  "),
              if (any(d$sig)) paste(d$h[d$sig], collapse = ",") else "nenhum"))
}

## FEVD
fv <- fevd(v, n.ahead = H)
fevd_tab <- round(100 * rbind(IBC = fv$IBC[H, ], SaldoReal = fv$SaldoReal[H, ], Spread = fv$Spread[H, ],
                              IPCA = fv$IPCA[H, ]), 1)
fevd12 <- round(100 * rbind(IBC = fv$IBC[12, ], SaldoReal = fv$SaldoReal[12, ], Spread = fv$Spread[12, ],
                            IPCA = fv$IPCA[12, ]), 1)
cat("\nFEVD (%) em 12 meses:\n"); print(fevd12)
cat("FEVD (%) em 24 meses:\n"); print(fevd_tab)

## =============================================================
## 4. Contrafactual Bernanke, Gertler e Watson (1997)
## Resposta do IBC-Br a um choque de +1 p.p. na meta Selic com e sem a
## resposta endogena do credito (saldo livre real e spread anulados).
## =============================================================
boot_var <- function(mod, yy, DD) {
  B <- Bcoef(mod); p <- mod$p; Tn <- nrow(yy); U <- scale(resid(mod), scale = FALSE)
  ys <- yy; idx <- sample(nrow(U), Tn - p, replace = TRUE)
  for (t in (p + 1):Tn) ys[t, ] <- as.vector(B %*% c(as.vector(t(ys[(t - 1):(t - p), , drop = FALSE])), 1, DD[t, ])) + U[idx[t - p], ]
  colnames(ys) <- colnames(yy); VAR(ys, p = p, type = "const", exogen = DD)
}
theta <- function(mod) {
  Ph <- Phi(mod, nstep = H); L <- t(chol(crossprod(resid(mod)) / nrow(resid(mod))))
  lapply(1:(H + 1), function(h) Ph[, , h] %*% L)
}
bgw <- function(mod, ord, shut, resp = "IBC", pol = "Meta") {
  th <- theta(mod); K <- length(ord); iS <- which(ord == pol); iY <- which(ord == resp)
  e <- matrix(0, K, H + 1); e[iS, 1] <- 1; out <- numeric(H + 1)
  for (h in 0:H) {
    pr <- rowSums(sapply(0:h, function(s) th[[h - s + 1]] %*% e[, s + 1]))
    for (cc in sort(match(shut, ord))) {   # choques compensatorios que zeram a resposta do credito
      a <- pr[cc] / th[[1]][cc, cc]; e[cc, h + 1] <- e[cc, h + 1] - a; pr <- pr - a * th[[1]][, cc]
    }
    out[h + 1] <- pr[iY]
  }
  sc <- th[[1]][iS, iS]
  list(total = sapply(th, function(mm) mm[iY, iS]) / sc, cf = out / sc)
}
cat("\nContrafactual BGW (500 replicas)...\n")
bg <- list()
for (sh in list(c("SaldoReal", "Spread"), "Spread", "SaldoReal")) {
  nm <- paste(sh, collapse = "+")
  r0 <- bgw(v, ORD, sh)
  bb <- t(replicate(500, { r <- bgw(boot_var(v, y, D), ORD, sh); r$total - r$cf }))
  bg[[nm]] <- data.frame(h = 0:H, total = r0$total, cf = r0$cf, contrib = r0$total - r0$cf,
                         lo = apply(bb, 2, quantile, .05), hi = apply(bb, 2, quantile, .95))
  cat(sprintf("Desliga %s (IBC-Br em %%, choque de +1 p.p.):\n", nm))
  for (h in c(6, 12, 18, 24)) with(bg[[nm]][h + 1, ],
    cat(sprintf("  h%-2d total %+.3f | sem credito %+.3f | contribuicao %+.3f [IC90 %+.3f, %+.3f]%s\n",
                h, total, cf, contrib, lo, hi, ifelse(lo > 0 | hi < 0, " *", ""))))
}

## =============================================================
## 5. Projecoes locais (Jorda, 2005) com erros Newey-West
## Choque = inovacao da meta Selic ortogonal ao bloco lento contemporaneo
## (mesma hipotese de identificacao do VAR), controles: p defasagens de
## todas as variaveis + dummies. Resposta acumulada y(t+h) - y(t-1).
## =============================================================
nw_se <- function(X, e, L) {
  XX <- solve(crossprod(X)); g <- X * e; S <- crossprod(g)
  for (l in seq_len(L)) { w <- 1 - l / (L + 1); G <- crossprod(g[-(1:l), , drop = FALSE], g[1:(nrow(g) - l), , drop = FALSE]); S <- S + w * (G + t(G)) }
  sqrt(diag(XX %*% S %*% XX))
}
lp <- function(yy, DD, shock_var, slow, resps, p = P) {
  Tn <- nrow(yy); t0 <- (p + 1):Tn
  lagm <- do.call(cbind, lapply(1:p, function(l) yy[t0 - l, , drop = FALSE]))
  Z <- cbind(1, yy[t0, slow, drop = FALSE], lagm, DD[t0, , drop = FALSE]); Z <- Z[, colSums(abs(Z)) > 0, drop = FALSE]
  shock <- resid(lm(yy[t0, shock_var] ~ Z - 1)); shock <- shock / sd(shock)
  out <- list()
  for (rs in resps) {
    est <- se <- numeric(H + 1)
    for (h in 0:H) {
      ok <- which(t0 + h <= Tn)
      dep <- yy[t0[ok] + h, rs] - yy[t0[ok] - 1, rs]
      X <- cbind(shock = shock[ok], Z[ok, , drop = FALSE])
      X <- X[, qr(X)$pivot[seq_len(qr(X)$rank)], drop = FALSE]
      f <- lm.fit(X, dep)
      est[h + 1] <- f$coefficients["shock"]
      se[h + 1] <- nw_se(X, f$residuals, h + 1)[which(colnames(X) == "shock")]
    }
    out[[rs]] <- data.frame(h = 0:H, resp = est, lo = est - 1.96 * se, hi = est + 1.96 * se)
    out[[rs]]$sig <- out[[rs]]$lo > 0 | out[[rs]]$hi < 0
  }
  out
}
lp_meta  <- lp(y, D, "Meta", c("IBC", "IPCA", "SaldoReal"), c("Spread", "SaldoReal", "IBC", "IPCA"))
lp_saldo <- lp(y, D, "SaldoReal", c("IBC", "IPCA"), "IBC")
cat("\nProjecoes locais (1 d.p., IC 95% Newey-West):\n")
for (nm in names(lp_meta)) { d <- lp_meta[[nm]]
  cat(sprintf("  Meta -> %-9s %s | sig: %s\n", nm, paste(sprintf("h%d %+.3f", c(0, 3, 6, 12, 18, 24), d$resp[c(0, 3, 6, 12, 18, 24) + 1]), collapse = "  "),
              if (any(d$sig)) paste(d$h[d$sig], collapse = ",") else "nenhum")) }
d <- lp_saldo$IBC
cat(sprintf("  SaldoReal -> IBC  %s | sig: %s\n", paste(sprintf("h%d %+.3f", c(0, 3, 6, 12, 18, 24), d$resp[c(0, 3, 6, 12, 18, 24) + 1]), collapse = "  "),
            if (any(d$sig)) paste(d$h[d$sig], collapse = ",") else "nenhum"))

## =============================================================
## 6. Robustez: mesmas perguntas em especificacoes alternativas
## =============================================================
resume <- function(d) {   # sinal e meses com resposta significativa a 95%
  faixa <- function(s) if (!length(s)) NULL else if (min(s) == max(s)) paste(min(s)) else paste0(min(s), "–", max(s))
  pos <- faixa(d$h[d$sig & d$resp > 0]); neg <- faixa(d$h[d$sig & d$resp < 0])
  sig <- c(if (!is.null(pos)) paste("(+)", pos), if (!is.null(neg)) paste("(−)", neg))
  if (length(sig)) paste(sig, collapse = "; ") else "n.s."
}
rob <- function(lab, ord, ini = "2011-03-01", p = P, eventos = TRUE, pol = "Meta", sal = "SaldoReal") {
  mm <- monta(ord, ini, eventos); yy <- mm$y; DD <- mm$D
  assign("DD_rob", DD, envir = globalenv())
  vv <- VAR(yy, p = p, type = "const", exogen = DD_rob)
  oo <- irf_all(vv, intersect(c(pol, sal), ord), RUNS_ROB)
  g <- function(i, r) if (all(c(i, r) %in% ord)) resume(irf_get(oo, i, r)) else "—"
  lb <- min(apply(resid(vv), 2, function(e) Box.test(e, 12, "Ljung-Box")$p.value))
  data.frame(Especificacao = lab, T = nrow(yy), Spread = g(pol, "Spread"), Saldo = g(pol, sal),
             SaldoIBC = g(sal, "IBC"), IPCA = g(pol, "IPCA"), LBmin = sprintf("%.2f", lb))
}
cat("\nRobustez (", RUNS_ROB, "replicas cada)...\n")
rob_tab <- rbind(
  rob("Principal (p = 4)", ORD),
  rob("p = 3", ORD, p = 3),
  rob("p = 6", ORD, p = 6),
  rob("Selic média mensal", c("IBC", "IPCA", "SaldoReal", "Selic", "Spread", "Cambio"), pol = "Selic"),
  rob("Saldo livre em % do PIB", c("IBC", "IPCA", "Saldo", "Meta", "Spread", "Cambio"), sal = "Saldo"),
  rob("Sem dummies de eventos", ORD, eventos = FALSE),
  rob("Crédito total % PIB, 2003–2025", c("IBC", "IPCA", "CredTotal", "Meta", "Cambio"), ini = "2003-01-01", p = 5, sal = "CredTotal")
)
pk <- function(d) resume(d)
rob_tab <- rbind(rob_tab, data.frame(Especificacao = "Projeções locais (Newey-West)", T = nrow(y),
  Spread = pk(lp_meta$Spread), Saldo = pk(lp_meta$SaldoReal), SaldoIBC = pk(lp_saldo$IBC), IPCA = pk(lp_meta$IPCA), LBmin = "—"))
cat("\nTabela de robustez (resposta em 12 e 24 meses; faixas significativas a 95%, por sinal):\n")
print(rob_tab, row.names = FALSE)

## =============================================================
## 7. Figuras (padrao visual da monografia)
## =============================================================
tema <- theme_classic(base_size = 12, base_family = "serif") +
  theme(axis.text = element_text(color = "black", size = 10), axis.title = element_text(size = 11),
        axis.line = element_line(linewidth = 0.4), plot.margin = margin(8, 12, 6, 6),
        strip.background = element_blank(), strip.text = element_text(size = 11))
plot_irf <- function(d, ylab) ggplot(d, aes(h, resp)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.7) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(0, H, 6), expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = "Meses após o choque", y = ylab) + tema
salva <- function(g, f, w = 6, h = 3.6) ggsave(file.path(OUT, f), g, width = w, height = h, dpi = 320, bg = "white")

salva(plot_irf(res[["Meta -> Spread"]], "Spread do crédito livre (p.p.)"), "final_irf_1_selic_spread.png")
salva(plot_irf(res[["Meta -> SaldoReal"]], "Saldo real do crédito livre (%)"), "final_irf_2_selic_saldo.png")
salva(plot_irf(res[["SaldoReal -> IBC"]], "IBC-Br (%)"), "final_irf_3_saldo_ibc.png")

painel <- function(lst, labs) {
  dd <- do.call(rbind, lapply(seq_along(lst), function(i) cbind(lst[[i]][, c("h", "resp", "lo", "hi")], painel = labs[i])))
  dd$painel <- factor(dd$painel, levels = labs)
  ggplot(dd, aes(h, resp)) + geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80") +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) + geom_line(linewidth = 0.8) +
    facet_wrap(~painel, ncol = 2, scales = "free_y") +
    scale_x_continuous(breaks = seq(0, H, 6)) + labs(x = "Meses após o choque", y = NULL) + tema
}
salva(painel(list(res[["Meta -> IBC"]], res[["Meta -> IPCA"]], res[["Spread -> IBC"]], res[["Spread -> SaldoReal"]]),
             c("Selic → IBC-Br (%)", "Selic → IPCA (%)", "Spread → IBC-Br (%)", "Spread → Saldo real (%)")),
      "final_irf_4_complementares.png", w = 7, h = 5.2)
salva(painel(list(lp_meta$Spread, lp_meta$SaldoReal, lp_saldo$IBC, lp_meta$IPCA),
             c("Selic → Spread (p.p.)", "Selic → Saldo real (%)", "Saldo real → IBC-Br (%)", "Selic → IPCA (%)")),
      "final_lp_robustez.png", w = 7, h = 5.2)

b <- bg[["SaldoReal+Spread"]]
bl <- rbind(data.frame(h = b$h, val = b$total, serie = "Resposta total"),
            data.frame(h = b$h, val = b$cf, serie = "Sem resposta do crédito"))
g1 <- ggplot(bl, aes(h, val, linetype = serie)) + geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.5) +
  geom_line(linewidth = 0.9) + scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
  scale_x_continuous(breaks = seq(0, H, 6)) + labs(x = "Meses após o choque", y = "IBC-Br (%)") +
  tema + theme(legend.position = "bottom")
g2 <- ggplot(b, aes(h, contrib)) + geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) + geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(0, H, 6)) + labs(x = "Meses após o choque", y = "Contribuição do crédito (p.p.)") + tema
ggsave(file.path(OUT, "final_bgw_contrafactual.png"), arrangeGrob(g1, g2, ncol = 2), width = 8.5, height = 3.8, dpi = 320, bg = "white")

## =============================================================
## 8. Tabelas (padrao booktabs da monografia)
## =============================================================
save_tab <- function(df, file_out, fontsize = 10.5) {
  tt <- ttheme_minimal(core = list(fg_params = list(fontfamily = "serif", fontsize = fontsize), padding = unit(c(10, 6), "pt")),
                       colhead = list(fg_params = list(fontfamily = "serif", fontsize = fontsize, fontface = "bold"), padding = unit(c(10, 6), "pt")))
  g <- tableGrob(df, rows = NULL, theme = tt)
  for (ci in which(g$layout$name %in% c("core-fg", "colhead-fg") & g$layout$l == 1)) { g$grobs[[ci]]$hjust <- 0; g$grobs[[ci]]$x <- unit(0.02, "npc") }
  hr <- min(g$layout$t[g$layout$name == "colhead-fg"]); lr <- max(g$layout$t); l <- 1; r <- ncol(df)
  g <- gtable_add_grob(g, segmentsGrob(0, 1, 1, 1, gp = gpar(lwd = 1.6)), t = hr, l = l, r = r, name = "top")
  g <- gtable_add_grob(g, segmentsGrob(0, 0, 1, 0, gp = gpar(lwd = 0.9)), t = hr, l = l, r = r, name = "mid")
  g <- gtable_add_grob(g, segmentsGrob(0, 0, 1, 0, gp = gpar(lwd = 1.6)), t = lr, l = l, r = r, name = "bot")
  w <- convertWidth(sum(g$widths), "in", TRUE) + 0.25; h <- convertHeight(sum(g$heights), "in", TRUE) + 0.15
  png(file.path(OUT, file_out), width = w, height = h, units = "in", res = 320, bg = "white"); grid.newpage(); grid.draw(g); dev.off()
}
br <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")

save_tab(data.frame(`Defasagens` = diag_tab$p,
                    `IBC-Br` = br(diag_tab$IBC), IPCA = br(diag_tab$IPCA), `Saldo real` = br(diag_tab$SaldoReal),
                    `Meta Selic` = br(diag_tab$Meta), Spread = br(diag_tab$Spread), `Câmbio` = br(diag_tab$Cambio),
                    `LM sistema` = br(diag_tab$ES, 3), `Raiz máx.` = br(diag_tab$raiz, 3), check.names = FALSE),
         "final_tab_diagnostico.png")

rt <- rob_tab
names(rt) <- c("Especificação", "T", "Selic → Spread", "Selic → Saldo", "Saldo → IBC-Br", "Selic → IPCA", "LB mín.")
rt[] <- lapply(rt, function(x) gsub("([0-9])\\.([0-9])", "\\1,\\2", as.character(x)))
save_tab(rt, "final_tab_robustez.png", fontsize = 10)

fl <- function(M, hz) data.frame(Horizonte = hz, Variável = c("IBC-Br", "Saldo real", "Spread", "IPCA"),
                                 `IBC-Br` = br(M[, 1], 1), IPCA = br(M[, 2], 1), `Saldo real` = br(M[, 3], 1),
                                 `Meta Selic` = br(M[, 4], 1), Spread = br(M[, 5], 1), `Câmbio` = br(M[, 6], 1), check.names = FALSE)
save_tab(rbind(fl(fevd12, "12 meses"), fl(fevd_tab, "24 meses")), "final_tab_fevd.png")

save(m, v, res, bg, lp_meta, lp_saldo, rob_tab, diag_tab, fevd12, fevd_tab, file = "Resultados/modelo_final.RData")
cat("\nFIM\n")
