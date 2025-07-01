# Gender and Disciplines in Grant Peer Review

This repository provides the implementation codes for the descriptive and regression
analyses of the SNSF grant peer review reports conducted in the following research paper:

**Gender and Discipline Shape Length, Content and Tone of Grant Peer Review Reports**

## Abstract

Peer review by experts is central to the evaluation of grant proposals, but little is
known about how gender and disciplinary differences shape the content and tone of
grant peer review reports. We analyzed 39,280 review reports submitted to the
Swiss National Science Foundation between 2016 and 2023, covering 11,385 proposals
for project funding across 21 disciplines from the Social Sciences and Humanities (SSH),
Life Sciences (LS), and Mathematics, Informatics, Natural Sciences, and Technology (MINT).
Using supervised machine learning, we classified over 1.3 million sentences by evaluation
criteria and sentiment. Reviews in SSH were significantly longer and more critical,
with less focus on the applicant’s track record, while those in MINT were more concise
and positive, with a higher focus on the track record, as compared to those in LS.
Compared to male reviewers, female reviewers write longer reviews that more closely
align with the evaluation criteria and express more positive sentiments. Female
applicants tend to receive reviews with slightly more positive sentiment than male
applicants. Gender and disciplinary culture influence how grant proposals are
reviewed--shaping the tone, length, and focus of peer review reports. These differences
have important implications for fairness and consistency in research funding.

Authors: [Stefan Müller](https://orcid.org/0000-0002-6315-4125)
[Gabriel Okasa](https://orcid.org/0000-0002-3573-7227),
[Michaela Strinzel](https://orcid.org/0000-0003-3181-0623),
[Anne Jorstad](https://orcid.org/0000-0002-6438-1979),
[Katrin Milzow](https://orcid.org/0009-0002-8959-2534), and
[Matthias Egger](https://orcid.org/0000-0001-7462-5132)

## Code

The code scripts are located in the `code` subfolder, which consists of three
`R` scripts that contain the implementation of the following analyses:

- `01_descriptive_plots_tables.R`: script for descriptive analyses, plots and tables
- `02_regression_models_mixed-effects.R`: script for regression analyses using mixed-effects models
- `03_regression_models_partially_linear.R`: script for regression analysis using partially linear models

The subfolder `data` serves as a placeholder as due to data protection laws, the data cannot be shared.

Similarly, the subfolder `output` is a placeholder for storage of the model outputs, plots and tables.

## Replication

To clone the repository run:

```
git clone https://github.com/snsf-data/ml-gender-disciplines-peer-review.git
```

The implementation relies on `R` version 4.2.1 or higher.

## Resources

- [Hugging Face Models](https://huggingface.co/snsf-data)
- [Methodological Paper](https://arxiv.org/abs/2411.16662)
- [Data Management Plan](https://doi.org/10.46446/DMP-peer-review-assessment-ML)
- [Annotation Codebook](https://doi.org/10.46446/Codebook-peer-review-assessment-ML)

## Contact

For general inquiries about the research paper and the codes, please contact [stefan.mueller@ucd.ie](mailto:stefan.mueller@ucd.ie).

## License

MIT © snsf-data
