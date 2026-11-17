################################################################################
## SENSIBILIDADE A ORDENACAO DE CHOLESKY                                      ##
## Mesmo modelo final (p = 4, dummies sazonais e de eventos, 2011-2025),      ##
## trocando apenas a ordem das variaveis. Bootstrap de 5.000 replicas, 95%.   ##
################################################################################
suppressMessages({ library(readxl); library(dplyr); library(vars) })
if (!dir.exists("Dados")) stop("Rode a partir da raiz do projeto (abra Monografia.Rproj no RStudio).")
set.seed(1234)
RUNS <- 5000; H <- 24; P <- 4

base <- readRDS("Dados/base_canal_credito.rds")
dummies <- function(d) {
  w <- function(a, b) as.numeric(d >= as.Date(a) & d <= as.Date(b))
  S <- sapply(1:11, function(m) as.numeric(as.numeric(format(d, "%m")) == m) - 1 / 12)
  colnames(S) <- paste0("saz", 1:11)
  E <- cbind(d2008 = w("2008-09-01","2009-06-01"), d2015 = w("2015-01-01","2015-12-01"),
             d2016 = w("2016-01-01","2016-12-01"), d2020 = w("2020-03-01","2020-12-01"),
             greve18 = w("2018-05-01","2018-07-01"), elei18 = w("2018-09-01","2018-11-01"),
             fisc21 = w("2021-10-01","2021-12-01"))
  cbind(S, E[, colSums(E) > 0, drop = FALSE])
}
faixa <- function(d) {
  f <- function(s) if (!length(s)) NULL else if (min(s) == max(s)) paste(min(s)) else paste0(min(s), "–", max(s))
  pos <- f(d$h[d$sig & d$resp > 0]); neg <- f(d$h[d$sig & d$resp < 0])
  s <- c(if (!is.null(pos)) paste("(+)", pos), if (!is.null(neg)) paste("(−)", neg))
  if (length(s)) paste(s, collapse = "; ") else "n.s."
}
get <- function(o, imp, resp) {
  d <- data.frame(h = 0:H, resp = o$irf[[imp]][, resp], lo = o$Lower[[imp]][, resp], hi = o$Upper[[imp]][, resp])
  d$sig <- d$lo > 0 | d$hi < 0; d
}
ords <- list(
  "Principal"                       = c("IBC", "IPCA", "SaldoReal", "Meta", "Spread", "Cambio"),
  "Saldo depois da Selic"           = c("IBC", "IPCA", "Meta", "SaldoReal", "Spread", "Cambio"),
  "Selic em primeiro lugar"         = c("Meta", "IBC", "IPCA", "SaldoReal", "Spread", "Cambio"),
  "Câmbio antes da Selic"           = c("IBC", "IPCA", "SaldoReal", "Cambio", "Meta", "Spread"),
  "Spread antes da Selic"           = c("IBC", "IPCA", "SaldoReal", "Spread", "Meta", "Cambio"))
dd <- base %>% filter(datas >= as.Date("2011-03-01"), datas <= as.Date("2025-08-01"))
tab <- data.frame()
for (nm in names(ords)) {
  ord <- ords[[nm]]
  m <- dd %>% dplyr::select(datas, all_of(ord)) %>% na.omit()
  y <- as.matrix(m[, ord]); assign("DD_ord", dummies(m$datas), envir = globalenv())
  v <- VAR(y, p = P, type = "const", exogen = DD_ord)
  choque <- t(chol(summary(v)$covres))["Meta", "Meta"]
  o <- irf(v, impulse = c("Meta", "SaldoReal"), response = ord, n.ahead = H, ortho = TRUE, boot = TRUE, runs = RUNS, ci = 0.95)
  sp <- get(o, "Meta", "Spread"); sa <- get(o, "Meta", "SaldoReal"); ib <- get(o, "Meta", "IBC"); si <- get(o, "SaldoReal", "IBC")
  linha <- data.frame(Ordenacao = nm, Choque = round(choque, 2),
                      Spread12 = round(sp$resp[13], 2), SpreadSig = faixa(sp),
                      Saldo24 = round(sa$resp[25], 2), SaldoSig = faixa(sa),
                      IBCSig = faixa(ib), SaldoIBCSig = faixa(si))
  tab <- rbind(tab, linha); print(linha, row.names = FALSE)
}
cat("\nTabela final:\n"); print(tab, row.names = FALSE)
saveRDS(tab, "Resultados/sensibilidade_ordenacao.rds")
cat("ok\n")
