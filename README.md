# Finite-sample bias-correction factors for the median absolute deviation based on the Harrell-Davis quantile estimator and its trimmed modification

The repository contains auxiliary data for the paper "Finite-sample bias-correction factors for the median absolute deviation based on the Harrell-Davis quantile estimator and its trimmed modification":

* The source code of the paper: `mad-factors.Rmd`, `preamble.tex`, `references.bib`, `*.R`
* Simulation 1 "Evaluating bias-correction factors using the Monte-Carlo method":
  * Source code: `simulation-factors/`
  * The results: `data/sm.csv`, `data/hd.csv`, `data/thd-sqrt.csv`
* Simulation 2 "Statistical efficiency of the median absolute deviation":
  * Source code: `simulation-efficiency.R`
  * The results: `data/efficiency.csv`
* Simulation 3 "Sensitivity to outliers of the median absolute deviation"
  * Source code: `simulation-sensitivity.R`
  * The results: `data/sensitivity-sd.csv`, `data/sensitivity-iqr.csv`, `data/sensitivity-mad.csv`
* The reference implementation of the suggested estimators: `reference-implementation.R`