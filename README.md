# DepAD: Dependency-based Anomaly Detection

This repository contains the R implementation of the **DepAD** framework, accompanying the paper:

> **Dependency-based anomaly detection: A general framework and comprehensive evaluation**
> Sha Lu, Lin Liu, Kui Yu, Thuc Duy Le, Jixue Liu, Jiuyong Li
> *Expert Systems with Applications*, Volume 297, Part A, 2026, 129249
> DOI: [10.1016/j.eswa.2025.129249](https://doi.org/10.1016/j.eswa.2025.129249)

---

## Overview

DepAD is a modular framework for unsupervised anomaly detection based on **dependency deviation**: an object is anomalous if it deviates from the normal variable dependencies learned from data, rather than being in a sparse region of the feature space (the traditional proximity-based view).

The framework consists of three interchangeable components:

1. **Relevant variable selection** — identifies which variables are predictive of each target variable (MI, IEPC, DC, MB via `fast.iamb`, PC via `si.hiton.pc`)
2. **Prediction model** — trains a regression model for each target using its relevant variables (BCART, LASSO, Ridge, Linear, etc.)
3. **Score combination** — aggregates per-variable deviations into a single anomaly score (Sum, Max, GS, Thresh/PS, AZM-PS)

This systematic combination yields 125+ DepAD algorithm variants, all evaluated on 32 real-world benchmark datasets.

---

## Repository Structure

```
.
├── main.R                  # Main entry point — configure and run experiments here
├── depad.R                 # Core DepAD algorithm (GetRelVar, FitDepadPredModel, AlgDepad)
├── functions.R             # Shared utilities: evaluation metrics, I/O, combination methods
├── gen_data.R              # Data sampling and preprocessing
├── real_data.R             # Loaders for all 32 benchmark datasets
├── comp_algorithms.R       # Baseline anomaly detection algorithms (LOF, iForest, ABOD, etc.)
├── utility.R               # Result organisation and LaTeX table generation
├── data/
│   ├── real_data.csv       # Metadata for all benchmark datasets (ID, name, size, anomaly ratio)
│   └── seed_list.txt       # Random seeds used for sampling (ensures reproducibility)
```

---

## Requirements

**R version:** ≥ 4.0

**R packages** (installed automatically on first run):

| Package | Purpose |
|---|---|
| `foreach`, `doParallel`, `doRNG` | Parallel computation |
| `bnlearn` | Markov Blanket / PC-skeleton learning |
| `FSinR` | Feature selection (MI, IEPC, DC) |
| `ipred`, `rpart` | Bagged CART prediction model |
| `glmnet` | LASSO, Ridge, Elastic Net |
| `dbscan` | LOF baseline |
| `IsolationForest` | Isolation Forest baseline (R-Forge) |
| `abodOutlier` | ABOD baseline |
| `HighDimOut` | SOD baseline |
| `e1071` | One-class SVM baseline |
| `PRROC` | ROC-AUC and PR-AUC evaluation |
| `farff` | ARFF file reading |
| `data.table`, `dplyr`, `stringr`, `caret`, `FNN` | Data manipulation |

---

## Setup

### 1. Clone or download this repository

```bash
git clone https://github.com/ShaLu-ML/DepAD.git
cd DepAD
```

### 2. Download the benchmark datasets

The datasets used in the paper are publicly available from the following sources. Download each dataset and place it in a local directory (e.g., `/path/to/datasets/`).

| Dataset | Source |
|---|---|
| WBC, Waveform, Breast Cancer, Letter, Gamma, Wine, Page Blocks, and others | [UCI Machine Learning Repository](https://archive.ics.uci.edu/) |
| Datasets in ARFF format (WBC, etc.) | [ODDS — Outlier Detection DataSets](http://odds.cs.stonybrook.edu/) |
| MNIST | [Yann LeCun's website](http://yann.lecun.com/exdb/mnist/) |

See `data/real_data.csv` for the full list of datasets with expected sizes and anomaly ratios.
See `real_data.R` for the exact file name and subdirectory expected for each dataset ID.

### 3. Configure `main.R`

Open `main.R` and edit the two lines at the top of the **USER SETUP** section:

```r
setwd("/path/to/DepAD")       # directory containing the R scripts
path.data <- "/path/to/datasets"  # directory containing the benchmark datasets
```

---

## Running Experiments

Open `main.R` in R or RStudio. After completing the setup above, source the file:

```r
source("main.R")
```

### Selecting algorithms

Edit the `depad.type` list and `alg.comp` vector in `main.R`:

```r
## DepAD variants — all combinations of the following will be evaluated
depad.type <- list(
  type.rel  = c("mi", "iepc", "dc", "mb_0.01", "pc_0.01"),   # relevant variable methods
  type.pred = c("bcart", "lasso", "linear", "ridge"),          # prediction models
  type.com  = c("Sum", "GS", "azm_ps", "Max", "Thresh")        # combination methods
)
```

**Note on MB and PC implementations:** The paper used the [CausalFS](https://github.com/kuiy/CausalFS) C library (FBED and HITON-PC algorithms) for MB and PC learning. This R implementation uses `bnlearn`'s `fast.iamb` (for `mb`) and `si.hiton.pc` (for `pc`), which are equivalent in principle but may produce slightly different results from those reported in the paper.

```r
## Comparison baselines
alg.comp <- c("lof", "wknn", "iforest", "abod", "sod",
              "combn", "also_m5p",
              "ocsvm_0.01", "ocsvm_0.05", "ocsvm_0.09")
```

### Selecting datasets

```r
at.lst <- data.ids        # run on all 32 datasets
## or select specific dataset IDs:
at.lst <- c(1, 2, 3)     # see data/real_data.csv for ID-name mapping
```

### Workflow control

Set `exe.from` to control which step is (re-)run:

| Value | Behaviour |
|---|---|
| `"auto"` | Skip steps whose results are already saved |
| `"rel"` | Re-run from relevant variable selection |
| `"pred"` | Re-run from prediction model fitting |
| `"comb"` | Re-run from score combination (default) |
| `"eva"` | Re-run evaluation only |

---

---

## Citation

If you use DepAD in your research, please cite:

```bibtex
@article{lu2026depad,
  title   = {Dependency-based anomaly detection: {A} general framework and comprehensive evaluation},
  author  = {Lu, Sha and Liu, Lin and Yu, Kui and Le, Thuc Duy and Liu, Jixue and Li, Jiuyong},
  journal = {Expert Systems with Applications},
  volume  = {297},
  pages   = {129249},
  year    = {2026},
  issn    = {0957-4174},
  doi     = {10.1016/j.eswa.2025.129249},
  url     = {https://www.sciencedirect.com/science/article/pii/S0957417425028659}
}
```

---

## License

This project is licensed under the MIT License — see [LICENSE](LICENSE) for details.

---

## Contact

For questions about the code or data, please contact **Sha Lu** at Sha.Lu@adelaide.edu.au
or open an issue on this repository.
