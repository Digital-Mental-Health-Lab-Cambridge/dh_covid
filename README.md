# Children's social connections, internet usage and mental health during the COVID-19 pandemic in the Global South: evidence from *Disrupting Harm*

This repository contains the supplementary data for the paper "Children's social connections, internet usage and mental health during the COVID-19 pandemic in the Global South: evidence from *Disrupting Harm*" (preprint coming soon).

Please review the licensing information at [LICENCE](https://github.com/Digital-Mental-Health-Lab-Cambridge/dh_covid/blob/main/LICENCE) before reusing any software in this repository.

## Data source

The data used in this study are controlled by UNICEF Innocenti – Global Office of Research and Foresight and are not publicly available.

## Data analysis

The data analysis in this study uses R version 4.3.1 in the `renv` environment present in the repository, via the `targets` pipeline. 

## Supplementary information

The `report` Quarto files are rendered automatically as part of the `targets` pipeline, and the output `.html` files are also provided in the repository. To view these, you should download both the `.html` file and the associated `_files` subdirectory to ensure that graphics are rendered correctly.

You can also view the reports on [confirmatory factor analysis](https://digital-mental-health-lab-cambridge.github.io/dh_covid/cfa_report/), [descriptive analyses](https://digital-mental-health-lab-cambridge.github.io/dh_covid/descriptives_report/), [multiple imputation](https://digital-mental-health-lab-cambridge.github.io/dh_covid/mice_report/), [estimated model coefficients](https://digital-mental-health-lab-cambridge.github.io/dh_covid/models_report/), [missingness](https://digital-mental-health-lab-cambridge.github.io/dh_covid/NA_report/) and [sensitivity analyses](https://digital-mental-health-lab-cambridge.github.io/dh_covid/sensitivity_report/) online.

## Results dashboard

In addition to the supplementary information available in the `report` files, you can also inspect the [results dashboard](https://digital-mental-health-lab-cambridge.github.io/dh_covid/results_dashboard/) (please allow some time for the app to load). This shows all the coefficients of interest obtained from the regression models in graphical form. Please note that there were convergence issues with many of the weighted models with self-harm or suicidal ideation as the outcome.
