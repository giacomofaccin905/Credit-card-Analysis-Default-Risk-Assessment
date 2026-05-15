# Machine Learning Models for Credit Card Default Risk Assessment

An end-to-end machine learning project in R focused on predicting credit card default risk using Random Forest and XGBoost models, applied to the Home Credit dataset.

---

##  Project Overview

This project builds and compares multiple classification models to assess the probability of credit card default. The analysis addresses real-world challenges such as **class imbalance**, **feature engineering**, and **threshold optimization** to maximize the detection of high-risk customers while maintaining operational efficiency.

---

##  Dataset

The project merges three data sources from the [Home Credit Default Risk](https://www.kaggle.com/c/home-credit-default-risk) dataset:

| File | Description |
|------|-------------|
| `application_train.csv` | Main application data with demographic and financial features |
| `bureau.csv` | Historical credit bureau data per client |
| `credit_card_balance.csv` | Monthly credit card balance snapshots |
| `HomeCredit_columns_description.csv` | Variable descriptions |

---

##  Methodology

### 1. Data Merging & Feature Engineering
- Aggregated `bureau.csv` by client (`SK_ID_CURR`): total credit, total debt, max/mean overdue days
- Aggregated `credit_card_balance.csv`: mean/max balance, utilization ratio, days past due (DPD)
- Engineered new features: `debt_ratio`, `annuity_ratio`, `age_years`, `employment_years`

### 2. Exploratory Data Analysis (EDA)
- Investigated class imbalance in the target variable (`TARGET`)
- Analysed income distribution using log-scale boxplots

### 3. Handling Class Imbalance
- Created a **balanced training set** by undersampling the majority class (non-default)
- Applied `scale_pos_weight` in XGBoost for weighted learning

### 4. Data Preprocessing & Scaling
- Applied **Yeo-Johnson transformation** (via `caret`) to skewed variables with negative values
- Applied **log1p transformation** to monetary variables: `AMT_INCOME_TOTAL`, `AMT_CREDIT`, `AMT_ANNUITY`, `Total_credit`
- Imputed `NA` and `Inf` values with median

### 5. Models Implemented

| Model | Notes |
|-------|-------|
| Random Forest (RF1, RF2, RF3) | 500 trees, feature importance ranking |
| XGBoost (standard) | Binary logistic, AUC metric |
| XGBoost (weighted) | `scale_pos_weight` to address class imbalance |

---

##  Results

| Model | AUC | Accuracy | Sensitivity | Specificity |
|-------|-----|----------|-------------|-------------|
| Random Forest | ~0.740 | — | High | Lower |
| XGBoost (standard) | ~0.737 | High | ~22% | — |
| XGBoost (weighted) | **~0.763** | **74.7%** | **57.3%** | **76.2%** |

> The **Weighted XGBoost** achieved the best overall trade-off between detecting defaulters (Sensitivity) and minimising false alarms (Specificity).

---

## How to Run

1. Clone this repository
2. Place the CSV data files in the project root directory
3. Open `Credit_card_analysis.Rmd` in RStudio
4. Install required packages:

```r
install.packages(c("dplyr", "ggplot2", "tidyr", "pROC",
                   "caret", "randomForest", "xgboost",
                   "Matrix", "scales"))
```

5. Knit to PDF or run chunks interactively

---

##  Dependencies

- R (≥ 4.0)
- `dplyr`, `tidyr`, `ggplot2`, `scales`
- `caret`, `randomForest`, `xgboost`, `Matrix`
- `pROC`

---

##  Future Improvements

- Advanced threshold tuning using F1-score optimisation
- Ensemble methods combining RF and XGBoost predictions
- SMOTE or other oversampling techniques for class imbalance
- Hyperparameter tuning via cross-validation

---

## 👤 Author

**Giacomo Faccin**  
Master's Degree in Data Analytics for Business and Society — Fintech & Bigtech track  
Ca' Foscari University of Venice

---

##  License

This project is for academic purposes only.
