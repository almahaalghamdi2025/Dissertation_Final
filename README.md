# Dissertation Code – Slum Demolitions & Housing Prices (Makkah)

This repository contains the R scripts used in the dissertation:

**"Between Demolition & Development: Slum Demolitions' Impact on Housing Prices in the Makkah Region"**  
*Almaha Alghamdi, August 2025*

---

## Description

The analysis investigates the impact of large-scale slum demolitions in Makkah on regional housing prices.

- The **base method** used is the **Synthetic Control Method (SCM)**.
- **Difference-in-Differences (DiD)** and **Synthetic DiD (SDID)** are included as **robustness checks**.
- The goal of this repository is to ensure **transparency** and support **authenticity verification** of the dissertation’s findings.

---

## Files Included

| File | Description |
|------|-------------|
| `Synth_Control_Thesis.R` | Synthetic Control Model (Base Model) |
| `Diff_in_Diff_Thesis.R` | DiD model for robustness |
| `Synth_DiD_Thesis.R` | Synthetic DiD robustness check |
| `README.md` | Repository description (this file) |

---

## Data Availability

The datasets used are publicly available and sourced from official Saudi government and statistical platforms.

- **Transactional data** is sourced from [AqarSaas](https://www.aqarsaas.sa), which provides access to real estate records.
- However, **historical datasets** on AqarSaas are available **only through subscription** and will **not be visible by default** to public users.
- Other datasets (e.g., employment, tourism, population) are available through platforms such as the **General Authority for Statistics (GaStat)** and **Ministry of Tourism**.

As a result, no raw datasets are included in this repository.

---

## Reproducibility Notice

Due to data sharing limitations, full replication is not possible directly from this repository.

However:
- The **code is complete** and structured in a way that allows replication once data is accessed.
- The repository serves to **verify the authenticity** of the methods and analytical approach used in the dissertation.

---
