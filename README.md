# Zero-Dose Small Area Estimation (SAE) in Ethiopia

## Overview

This repository contains the code, documentation, and reproducible analytical workflow for estimating the prevalence of zero-dose children at woreda level in Ethiopia using Bayesian Small Area Estimation (SAE).

The project combines survey data, spatial information, and statistical modelling to produce policy-relevant estimates for immunization planning and decision-making.

---

## Objectives

- Estimate woreda-level zero-dose prevalence.
- Improve estimates for areas with small or no survey samples.
- Produce high-quality maps and uncertainty estimates.
- Support evidence-based immunization policy and programme planning.

---

## Repository Structure

```
zero_dose/
│
├── data/
│   ├── raw/              # Original datasets (NOT tracked by Git)
│   ├── processed/        # Cleaned datasets (NOT tracked by Git)
│   ├── interim/          # Intermediate datasets (NOT tracked)
│   └── example/          # Small example datasets (optional)
│
├── R/                    # Reusable R functions
├── scripts/              # Analysis scripts
├── reports/              # Quarto reports
├── outputs/              # Tables and results (ignored)
├── figures/              # Maps and figures (ignored)
├── docs/                 # Documentation
│
├── zero_dose.Rproj
├── renv.lock
├── .gitignore
└── README.md
```

---

## Data

This repository **does not include the original survey or administrative datasets**.

Examples include:

- Ethiopia DHS datasets
- DHIS2 extracts
- WorldPop datasets
- Administrative boundary files
- Other restricted datasets

These files are excluded from GitHub using `.gitignore` because of licensing, confidentiality, and file size restrictions.

After obtaining the required datasets, place them in the following folders:

```
data/raw/
data/processed/
```

---

## Software Requirements

- R (latest stable version)
- RStudio
- GitHub Desktop (recommended)
- Git

---

## Reproducibility

This project uses **renv** to manage package versions.

To restore the project environment:

```r
renv::restore()
```

---

## Git Workflow

### Before starting work

1. Fetch latest changes.
2. Pull latest changes.
3. Switch to your working branch.

### During work

- Make small commits.
- Write meaningful commit messages.

Example:

```
Add BYM2 spatial model
```

or

```
Update pooled mapping workflow
```

### After finishing

- Commit changes.
- Push to GitHub.

---

## Important Notes

Do **NOT** commit:

- Raw survey data
- Processed confidential data
- DHS datasets
- DHIS2 exports
- Large spatial datasets
- Personal information

Only commit:

- R scripts
- Quarto documents
- Documentation
- Functions
- Project configuration files

---

## Project Status

Current components include:

- Data preparation
- Direct weighted estimates
- Bayesian Small Area Estimation (INLA-BYM2)
- Model validation
- Uncertainty assessment
- Woreda-level mapping
- Policy-oriented reporting

---

## Authors

Zero-Dose Small Area Estimation Team

Ethiopian Public Health Institute (EPHI)

National Disease Modelling Centre (NDMC)

