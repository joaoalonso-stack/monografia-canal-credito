# O canal de crédito na transmissão da política monetária no Brasil

**Evidências de um modelo VAR estrutural (2011–2025)**

Monografia de graduação em Ciências Econômicas, Instituto de Economia da UFRJ (novembro de 2026).
Autor: João Victor Alonso Kutianski Santana · Orientador: Prof. Dr. Antonio Luis Licha ·
Coorientadora: Profa. Dra. Viviane Luporini.

📄 **Texto completo:** [`Monografia.pdf`](Monografia.pdf) · 🎤 **Slides da defesa:** [`Apresentacao/apresentacao.pdf`](Apresentacao/apresentacao.pdf)

## Pergunta
Choques na Selic afetam o custo e o volume do crédito com recursos livres? E essas variações chegam à
atividade econômica?

## Método
- **SVAR em nível** com identificação recursiva no estilo Christiano, Eichenbaum e Evans (1999), dados mensais
  de mar/2011 a ago/2025 (T = 174):
  bloco lento (IBC-Br → IPCA → saldo do crédito livre real) → **meta Selic** → bloco rápido (spread do crédito
  livre → câmbio real).
- 4 defasagens (menor ordem sem autocorrelação residual), dummies sazonais e de eventos, IRFs de 24 meses com
  intervalos de 95% por bootstrap (5.000 réplicas) e decomposição da variância (FEVD).
- **Contrafactual** à la Bernanke, Gertler e Watson (1997): desliga as variáveis de crédito para medir quanto do
  efeito da Selic sobre a atividade passa por elas.
- **Robustez:** projeções locais (Jordà, 2005) com erros Newey-West, ordenações alternativas, outras
  defasagens e séries, amostra desde 2003 e o crédito direcionado no lugar do livre.

## Principais resultados
- O aperto monetário **eleva o spread do crédito livre** de forma clara e persistente: para um choque de 1 desvio
  padrão na meta (0,26 p.p.), o spread sobe ~0,38 p.p. em 12 meses, significativo do 4º ao 22º mês.
- O **volume de crédito livre cai**, com menor intensidade: no VAR o efeito não é significativo, nas projeções
  locais é.
- O crédito responde por parcela relevante das flutuações da atividade: o saldo real explica ~11% da variância
  do IBC-Br em 12 meses e ~15% em 24 meses.
- O efeito final da Selic sobre o produto **não é estatisticamente identificável**.
- O **crédito direcionado não responde à Selic**: a segmentação entre crédito livre e direcionado obstrui em
  parte o canal de crédito no Brasil.

## Estrutura do repositório
| Pasta | Conteúdo |
|---|---|
| `Codigos/` | Scripts R numerados na ordem de execução (modelo final, complementos, figuras, robustez) |
| `Dados/` | Séries do SGS/Banco Central já tratadas (`.rds`) e base auxiliar (`.xlsx`) |
| `Resultados/` | Saídas em texto de cada script, objetos estimados e figuras de diagnóstico |
| `Figuras_Finais/` | Figuras do texto em PNG e SVG |
| `Apresentacao/` | Slides da defesa em LaTeX (Beamer) e PDF |

**Fontes dos dados (SGS/BCB):** IBC-Br dessazonalizado (24364), IPCA (433), saldo do crédito livre (20542 e
20625), crédito direcionado (20593), Selic média (4189) e meta (432), spread do crédito livre (20786) e câmbio real.

## Como reproduzir
1. Instale o R (≥ 4.2) e os pacotes:
   ```r
   install.packages(c("readxl", "dplyr", "tidyr", "vars", "urca", "lmtest", "zoo",
                      "ggplot2", "gridExtra", "gtable", "showtext", "rbcb"))
   ```
2. Abra `Monografia.Rproj` no RStudio (ou rode o R a partir da raiz do repositório).
3. Rode os scripts de `Codigos/` em ordem. O `01` estima o modelo final e grava `Resultados/modelo_final_saida.txt`.
   As figuras usam a fonte Latin Modern do TinyTeX (`tinytex::install_tinytex()`).

---

### Abstract
This study investigates the credit channel of monetary policy transmission in Brazil between March 2011 and
August 2025. A structural VAR estimated in levels and identified through a recursive ordering, checked with local
projections, shows that monetary tightening clearly raises the non-earmarked lending spread and reduces its volume
to a lesser extent, while credit accounts for a relevant share of fluctuations in activity. The overall effect of
monetary policy on output is not statistically identifiable, and earmarked credit does not respond to the Selic
rate, suggesting that the segmentation between earmarked and non-earmarked credit partially obstructs the credit
channel in Brazil.
