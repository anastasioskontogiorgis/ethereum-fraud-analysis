# Ethereum Fraud Analytics — Quarto suite

A consolidated statistical investigation of fraudulent behaviour on the Ethereum
blockchain: one dataset, one canonical preparation, a Python exploration track and an
R inference/modelling track. Consolidates four R analyses and one Python notebook
originally produced as separate coursework.

## Render

Prerequisites:

- Quarto >= 1.4 (https://quarto.org/docs/get-started/)
- R packages: `dplyr`, `tidyr`, `ggplot2`, `car`, `broom`, `pROC`, `rstatix`, `knitr`
- Python (for the exploration chapter): `jupyter`, `pandas`, `numpy`, `matplotlib`, `seaborn`

```bash
quarto render          # builds the whole site into docs/
quarto preview         # live-reloading local preview
```

Chapter 01 must render before 03–05 on a fresh checkout (it writes
`data/eth_clean.rds`); `quarto render` respects the chapter order, so a plain
full render just works.

## Publish

GitHub Pages: push the repo, then Settings → Pages → deploy from branch → `/docs`.
`freeze: auto` caches chapter output so unchanged chapters don't re-execute.

## Data

`data/transaction_dataset.csv` — the Kaggle Ethereum Fraud Detection dataset
(vagifa), 9,841 addresses × 50 features, FLAG = known fraud label.
