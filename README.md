# Chain Event Graphs
[![Language](https://img.shields.io/badge/language-R_(4.1.0%2B)-ffa033.svg?style=flat-square)](https://www.r-project.org)
[![Language](https://img.shields.io/badge/language-Python_3-54a4ff.svg?style=flat-square)](https://www.python.org)
[![License](https://img.shields.io/badge/license-MIT-bb86f7.svg?style=flat-square)](https://opensource.org/licenses/MIT)

This repository contains scripts supporting my research about Chain Event Graphs conducted under the Undergraduate Research Support Scheme at the University of Warwick in the summer 2021. The final report detailing work available at: [link](https://urss.warwick.ac.uk/items/show/129).

### Abstract
Chain Event Graphs (CEGs) are a family of graphical sta-tistical models derived from well-known probability trees. They form ageneralisation  of  Bayesian  Networks,  providing  an  explicit  representa-tion of context-specific conditional dependencies within their topology.This report demonstrates on a real cohort study how CEGs enable usto depict various hypotheses about the data generation mechanisms. Weargue that CEGs, in contrast to the standard framework of generalisedlinear models, can exhibit dependencies between multiple variables in amuch  more  intuitive  way.  We  also  present  how  CEGs  can  be  used  forstatistical inference with incomplete data set, identifying if the data aremissing at random and extracting further conclusions from the patternsof missingness. We additionally discuss the problem of data discretisa-tion and propose a method for supervised discretisation without leavingthe framework of tree-based models.

<p align="center">
  <img src="figures/readme.png" alt="ceg" width="800"/>
</p>


### Contents

```
├── chain-event-graphs
│   ├── R
│   ├── python
│   ├── chain-event-graphs-report
|   ├── README.md
│   └── data
```

- `R` - contains the R scripts for data processing, EDA and the `stagedtrees` submodule with custom algorithms for stage partitioning
- `python` - contains the python scripts used for fitting the CEGs and generating the final graphs for the report
- `chain-event-graphs-report` - submodule with the final .tex report 
- `ceg-workshops` - submodule with the workshops for CEG Conference, Uniersity of Warwick, September 2022
- `data` - contains raw data (content git ignored, data are safeguarded)




