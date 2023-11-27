# GC News: Content Analysis with regard to Populism of German News Media during the Covid-19 pandemic

This repository holds the data, code, and results for a master thesis with the Hochschule für Politik at the Technical University of Munich. It includes all ground truth labels and a fully labeled version of the _GC News_ data. To comply with copyright laws, the journalistic contents were removed from the _GC News_ data and are available for replication upon request. The repository further includes the _Python_ and _R_ code used for model creation, fine-tuning, and evaluation, as well as for regression analysis and data visualization. 

## Data

A labeled version of the entire corpus of German news paragraphs is included in `data/gcnews/gc_news_labeled.csv`. To comply with copyright laws, the journalistic contents were removed from this file. With the labels and other meta information, this file can be used to reproduce the regression and data analysis conducted in this study. A second `data/gcnews/gc_news_unlabeled_sample1000.csv` file contains 1000 random sample paragraphs from the _GC News_ dataset and can be used to understand, track, and reproduce the labeling process. 

The `data/ground_truth/labeled_data.csv` and `data/ground_truth/labeled_data_unbalanced.csv` files contain a stratified and a balanced version of the ground truth labels that were used to train, fine-tune, and evaluate the five implemented classification methods. 

The `data/external` data includes the daily 7-day incidence and fatality numbers of Covid-19 in Germany, as recorded by the Robert Koch Institute. The data was initially published in
- Robert Koch-Institut (2023): 7-Tage-Inzidenz der COVID-19-Fälle in Deutschland, Berlin: Zenodo. DOI: 10.5281/zenodo.10207072
- Robert Koch-Institut (2023): COVID-19-Todesfälle in Deutschland, Berlin: Zenodo. DOI: 10.5281/zenodo.10207073

## Text Classification

### Dictionaries

This publication applies three dictionaries via the [_multidictR_](https://github.com/jogrue/multidictR) and [_popdictR_](https://github.com/jogrue/popdictR) packages. Together with Gründl`s dictionary on populism, these were published in 
- Gründl, J. (2022). Populist ideas on social media: A dictionary-based measurement of populist communication. New Media & Society, 24(6), 1481–1499. https://doi.org/10.1177/1461444820976970

The `thiele_covid_terms.csv` and `thiele_econ_terms.csv` dictionaries were originally published @
- Thiele, D. (2022). Pandemic Populism? How Covid-19 Triggered Populist Facebook User Comments in Germany and Austria. Politics and Governance, 10(1), 185–196. https://doi.org/10.17645/pag.v10i1.4712

### Compressor-based Classification

The code in the `text_classification/gzip` folder implements a compressor-based _k_-NN-classifier and relies on Jiang _et al._`s [npc_gzip](https://github.com/bazingagin/npc_gzip) library. The Python codebase required for reproduction is available on pypi.org via
```
pip install npc-gzip
```
and was published in 
- Jiang, Z., Yang, M., Tsirlin, M., Tang, R., Dai, Y., & Lin, J. (2023). “Low-Resource” Text Classification: A Parameter-Free Classification Method with Compressors. Findings of the Association for Computational Linguistics: ACL 2023, 6810–6828. https://aclanthology.org/2023.findings-acl.426

### SetFit Classifier
The exact model specifications and usage instructions for the _SetFit_ classifier are available at https://huggingface.co/baunef/PopFit. The respective _Jupyter_ notebook `text_classification/SetFit/setfit_inference.ipynb` downloads the model from HuggingFace and can be used for inference. The `text_classification/SetFit/setfit_training.ipynb` notebook was deployed via Google Colaboratory and trains a SetFit classifier. 

### PopBERT
The `text_classification/zero_shot/zero_shot.ipynb` notebook contains an inference pipeline for BERT language models and is set for testing and inference with the PopBERT classifier (https://huggingface.co/luerhard/PopBERT). 

## Regression and Visualization

the `regression` and `visualization` directories contain _R_ files that produce all figures and results reported in the study. The figures can be viewed in the `figures` directory. 


 
 



## Citing GC News

If you use parts of this repository in your research, please consider citing
