# CWAS_Intro
Week 1 repository for introductory CWAS project to explore the assumptions of correlating diseases with Type II diabetes.
ICD9 codes were pulled from [https://phewas.mc.vanderbilt.edu/](https://phewas.mc.vanderbilt.edu/)

`cleanUp.py` fills in empty values from `Phewas_rawcode.txt` and outputs clean data into `Phewas_code.txt`. `phewas.R` contains functions that simulate a hypothetical cohort of Type II diabetics and the prevalence of common comorbidities, as well as computes the odds-ratio between case and control for each ICD9 code.
