################################################################################
## ROBUSTEZ - CREDITO LIVRE x CREDITO DIRECIONADO, 2011-2025                  ##
## Mesma especificacao do modelo final (p = 4, dummies sazonais e de eventos, ##
## ordenacao CEE), trocando ou acrescentando o saldo real do credito          ##
## direcionado (SGS 20593 deflacionado pelo IPCA).                           ##
##   B. substituicao: IBC > IPCA > DirReal > Meta > Spread > Cambio            ##
##   C. conjunto:     IBC > IPCA > SaldoReal > DirReal > Meta > Spread > Cambio##
##      (mesmo choque de politica para os dois segmentos; bootstrap conjunto  ##
##      para a diferenca livre - direcionado)                                 ##
##   D. projecoes locais (Jorda, 2005), Newey-West                            ##
################################################################################
suppressMessages({ library(readxl); library(dplyr); library(vars); library(rbcb) })
if (!dir.exists("Dados")) stop("Rode a partir da raiz do projeto (abra Monografia.Rproj no RStudio).")
set.seed(1234)
RUNS <- 5000; RUNS_DIF <- 5000; RUNS_ROB <- 1000; H <- 24; P <- 4

## ------------------------------------------------------------------ dados
cache <- "Dados/sgs20593_saldo_direcionado.rds"
if (!file.exists(cache)) {
  baixa <- function(code) { for (k in 1:6) { x <- tryCatch(get_series(code, start_date = "2007-01-01", end_date = "2025-12-31"), error = function(e) NULL); if (!is.null(x)) return(x); Sys.sleep(8) }; stop("falha SGS ", code) }
  dir <- baixa(20593); liv <- baixa(20542)
  names(dir) <- c("datas", "dir_nom"); names(liv) <- c("datas", "liv_nom")
  saveRDS(list(dir = dir, liv = liv), cache)
}
sg <- readRDS(cache)
base <- readRDS("Dados/base_canal_credito.rds") %>%
  left_join(mutate(sg$dir, datas = as.Date(datas)), by = "datas") %>%
  left_join(mutate(sg$liv, datas = as.Date(datas)), by = "datas") %>%
  mutate(DirReal = 100 * log(dir_nom / idx))
ck <- base %>% filter(!is.na(saldo_nom), !is.na(liv_nom))
cat("Checagem SGS 20542 x saldo livre da base: max dif. relativa =", signif(max(abs(ck$liv_nom / ck$saldo_nom - 1)), 3), "\n")
d3 <- readRDS("Dados/dados_cap3.rds")
base <- base %>% left_join(transmute(d3$dir, datas = as.Date(data), DirPIB = valor), by = "datas")
fim <- base %>% filter(datas == as.Date("2025-08-01"))
cat("Participacao do direcionado no estoque (ago/2025):", round(100 * fim$dir_nom / (fim$dir_nom + fim$liv_nom), 1), "%\n")

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
monta <- function(ord, ini = "2011-03-01", fim = "2025-08-01", eventos = TRUE) {
  dd <- base %>% filter(datas >= as.Date(ini), datas <= as.Date(fim)) %>% dplyr::select(datas, all_of(ord)) %>% na.omit()
  list(datas = dd$datas, y = as.matrix(dd[, ord]), D = dummies(dd$datas, eventos))
}
irf_get <- function(o, imp, resp) {
  d <- data.frame(h = 0:H, resp = o$irf[[imp]][, resp], lo = o$Lower[[imp]][, resp], hi = o$Upper[[imp]][, resp])
  d$sig <- d$lo > 0 | d$hi < 0; d
}
faixa <- function(d) {
  f <- function(s) if (!length(s)) NULL else if (min(s) == max(s)) paste(min(s)) else paste0(min(s), "-", max(s))
  pos <- f(d$h[d$sig & d$resp > 0]); neg <- f(d$h[d$sig & d$resp < 0])
  s <- c(if (!is.null(pos)) paste("(+)", pos), if (!is.null(neg)) paste("(-)", neg))
  if (length(s)) paste(s, collapse = "; ") else "n.s."
}
mostra <- function(nm, d) cat(sprintf("  %-24s %s | sig: %s\n", nm,
  paste(sprintf("h%d %+.3f", c(0, 6, 12, 18, 24), d$resp[c(0, 6, 12, 18, 24) + 1]), collapse = "  "), faixa(d)))
diag_var <- function(v) {
  lb <- apply(resid(v), 2, function(e) Box.test(e, 12, "Ljung-Box")$p.value)
  cat(sprintf("  T = %d | raiz max = %.4f | Ljung-Box(12) min p = %.3f (%s) | LM-ES(6) p = %.3f\n",
              nrow(v$y), max(roots(v)), min(lb), names(which.min(lb)), serial.test(v, lags.bg = 6, type = "ES")$serial$p.value))
}

## ------------------------------------------------------------------ A. principal (referencia)
ORD_A <- c("IBC", "IPCA", "SaldoReal", "Meta", "Spread", "Cambio")
mA <- monta(ORD_A); vA <- VAR(mA$y, p = P, type = "const", exogen = mA$D)
cat("\nA. Modelo principal (credito livre)\n"); diag_var(vA)

## ------------------------------------------------------------------ B. substituicao pelo direcionado
ORD_B <- c("IBC", "IPCA", "DirReal", "Meta", "Spread", "Cambio")
mB <- monta(ORD_B); vB <- VAR(mB$y, p = P, type = "const", exogen = mB$D)
cat("\nB. Credito direcionado no lugar do livre\n"); diag_var(vB)
cat("  choque de 1 d.p. na meta =", round(t(chol(summary(vB)$covres))["Meta", "Meta"], 3), "p.p.\n")
oB <- irf(vB, impulse = c("Meta", "DirReal"), response = ORD_B, n.ahead = H, ortho = TRUE, boot = TRUE, runs = RUNS, ci = 0.95)
resB <- list(`Meta -> DirReal` = irf_get(oB, "Meta", "DirReal"), `Meta -> Spread` = irf_get(oB, "Meta", "Spread"),
             `Meta -> Meta` = irf_get(oB, "Meta", "Meta"), `DirReal -> IBC` = irf_get(oB, "DirReal", "IBC"))
for (nm in names(resB)) mostra(nm, resB[[nm]])
fvB <- fevd(vB, n.ahead = H)
cat(sprintf("  FEVD: Meta explica %.1f%% (12m) e %.1f%% (24m) do saldo direcionado; direcionado explica %.1f%% do IBC-Br (24m)\n",
            100 * fvB$DirReal[12, "Meta"], 100 * fvB$DirReal[24, "Meta"], 100 * fvB$IBC[24, "DirReal"]))

## ------------------------------------------------------------------ C. modelo conjunto
ORD_C <- c("IBC", "IPCA", "SaldoReal", "DirReal", "Meta", "Spread", "Cambio")
mC <- monta(ORD_C); vC <- VAR(mC$y, p = P, type = "const", exogen = mC$D)
cat("\nC. Modelo conjunto (livre e direcionado)\n"); diag_var(vC)
cat("  choque de 1 d.p. na meta =", round(t(chol(summary(vC)$covres))["Meta", "Meta"], 3), "p.p.\n")
set.seed(1234)
oC <- irf(vC, impulse = "Meta", response = ORD_C, n.ahead = H, ortho = TRUE, boot = TRUE, runs = RUNS, ci = 0.95)
resC <- list(`Meta -> SaldoReal` = irf_get(oC, "Meta", "SaldoReal"), `Meta -> DirReal` = irf_get(oC, "Meta", "DirReal"),
             `Meta -> Spread` = irf_get(oC, "Meta", "Spread"), `Meta -> Meta` = irf_get(oC, "Meta", "Meta"))
for (nm in names(resC)) mostra(nm, resC[[nm]])
fvC <- fevd(vC, n.ahead = H)
cat(sprintf("  FEVD 24m: Meta explica %.1f%% do saldo livre e %.1f%% do saldo direcionado\n",
            100 * fvC$SaldoReal[24, "Meta"], 100 * fvC$DirReal[24, "Meta"]))

## diferenca livre - direcionado com bootstrap conjunto (residuos reamostrados)
boot_var <- function(mod, yy, DD) {
  B <- Bcoef(mod); p <- mod$p; Tn <- nrow(yy); U <- scale(resid(mod), scale = FALSE)
  ys <- yy; idx <- sample(nrow(U), Tn - p, replace = TRUE)
  for (t in (p + 1):Tn) ys[t, ] <- as.vector(B %*% c(as.vector(t(ys[(t - 1):(t - p), , drop = FALSE])), 1, DD[t, ])) + U[idx[t - p], ]
  colnames(ys) <- colnames(yy); VAR(ys, p = p, type = "const", exogen = DD)
}
dif_fun <- function(mod) {
  Ph <- Phi(mod, nstep = H); L <- t(chol(summary(mod)$covres)); iS <- which(colnames(mod$y) == "Meta")
  sapply(1:(H + 1), function(h) { r <- Ph[, , h] %*% L[, iS]; r[3] - r[4] })
}
D_C <- mC$D
dif0 <- dif_fun(vC)
cat("\n  Bootstrap da diferenca (", RUNS_DIF, "replicas)...\n")
db <- t(replicate(RUNS_DIF, dif_fun(boot_var(vC, mC$y, D_C))))
difC <- data.frame(h = 0:H, resp = dif0, lo = apply(db, 2, quantile, .025), hi = apply(db, 2, quantile, .975))
difC$sig <- difC$lo > 0 | difC$hi < 0
mostra("Livre - Direcionado", difC)
difC90 <- data.frame(h = 0:H, resp = dif0, lo = apply(db, 2, quantile, .05), hi = apply(db, 2, quantile, .95))
difC90$sig <- difC90$lo > 0 | difC90$hi < 0
mostra("Livre - Direcionado (90%)", difC90)

## ------------------------------------------------------------------ D. projecoes locais
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
cat("\nD. Projecoes locais (1 d.p., IC 95% Newey-West)\n")
lpB <- lp(mB$y, mB$D, "Meta", c("IBC", "IPCA", "DirReal"), "DirReal")
lpC <- lp(mC$y, mC$D, "Meta", c("IBC", "IPCA", "SaldoReal", "DirReal"), c("SaldoReal", "DirReal"))
mostra("LP subst.: Meta -> DirReal", lpB$DirReal)
mostra("LP conj.: Meta -> SaldoReal", lpC$SaldoReal)
mostra("LP conj.: Meta -> DirReal", lpC$DirReal)
save(resB, resC, difC, difC90, lpB, lpC, fvB, fvC, file = "Resultados/robustez_direcionado.RData")

## ------------------------------------------------------------------ E. variacoes
cat("\nE. Variacoes (", RUNS_ROB, "replicas)\n")
rob <- function(lab, ord, resp, p = P, eventos = TRUE, ini = "2011-03-01") {
  mm <- monta(ord, ini = ini, eventos = eventos); assign("DD_rob", mm$D, envir = globalenv())
  vv <- VAR(mm$y, p = p, type = "const", exogen = DD_rob)
  oo <- irf(vv, impulse = "Meta", response = resp, n.ahead = H, ortho = TRUE, boot = TRUE, runs = RUNS_ROB, ci = 0.95)
  r <- irf_get(oo, "Meta", resp); lb <- min(apply(resid(vv), 2, function(e) Box.test(e, 12, "Ljung-Box")$p.value))
  cat(sprintf("  %-38s T=%d  h12 %+.3f  h24 %+.3f  sig: %-14s LBmin %.2f\n", lab, nrow(mm$y), r$resp[13], r$resp[25], faixa(r), lb))
  invisible(r)
}
rob("Direcionado, p = 3", ORD_B, "DirReal", p = 3)
rob("Direcionado, p = 6", ORD_B, "DirReal", p = 6)
rob("Direcionado, sem dummies de eventos", ORD_B, "DirReal", eventos = FALSE)
rob("Livre, sem dummies de eventos", ORD_A, "SaldoReal", eventos = FALSE)
rob("Direcionado % PIB", c("IBC", "IPCA", "DirPIB", "Meta", "Spread", "Cambio"), "DirPIB")
rob("Livre % PIB", c("IBC", "IPCA", "Saldo", "Meta", "Spread", "Cambio"), "Saldo")
rob("Direcionado, sem spread", c("IBC", "IPCA", "DirReal", "Meta", "Cambio"), "DirReal")

cat("\nok\n")
