# viscol

## Master thesis credit

- Thesis title: "Visualising English Collocational Patterns in an Educational Web Application: Comparison of Native and Learner Corpora"
- Author: Koji Okumura (Master's Programme of Digital Humanities, Faculty of Engineering Science, KU Leuven, Belgium)
- Supervisor: Professor Dirk Speelman

## Project description
- This master thesis project has developed an educational web application "VisCol", which visualises English verb-noun collocations by comparing native and learner corpora.

- Corpora used in this project
  - Native corpus: Corpus of Contemporary American English (COCA): sample data (approx. 11,500,000 words)
    - COCA data is available at https://www.corpusdata.org/formats.asp.
  - Learner corpus: International Corpus of Learner English (ICLE): L1 Japanese data (approx. 227,000 words)
    - ICLE data is only accessible with license. Thus, ICLE corpus file is not uploaded on GitHub.

- Online outputs (Appendix A)
  - VisCol (web application): https://kojiokumura.shinyapps.io/viscol/
  - VisCol video tutorial: https://youtu.be/Xa7s1_bdFYg
  - GitHub repository: https://github.com/Koji1227/viscol.git

## Repository structure

### `1_data_cleansing` folder
I converted the ICLE plain text data into a vertical corpus by adding POS tags and lemmas. POS tagging was done on the web interface manually and lemmatisation was done by Python. ICLE corpus is not uploaded here.
- `grouping.ipynb`: It groupings and connects multiple txt files before POS tagging
- `lemmatization.ipynb`: After POS tagging, it adds lemmas to the corpus
- `word_counting.ipynb`: It counts the number of words in corpora.

### `2_visualisation` folder
I conducted collocation analysis of the corpora and developed a web application to visualise collocations.

- `corp` subfolder: COCA vartical corpus data (as txt files)
- `scripts` subfolder
  - `retrieve_data.R`: It converts corpus data from txt files to tsv/rds files
  - `freqlist.R`: It generates frequency tables.
  - `cooclist.R`: It generates collocation tables.
  - `network.R`: It visualises coolocations as a network.
  - `list_comparison.R`: It visualises collocation as lists.
  - `concordance.R`: It generates a concordance.
  - `app.R`: It develops a web application VisCol (local version).
- `data` subfolder
   - `corp_coca_sample.rds`: Formatted COCA vertical corpus
   - `corp_coca_short.rds`: Approx. one million lines at the beginning of `corp_coca_sample.rds` (for deployed version of VisCol)
   - `freq_verb.rds`: Verb frequency table for both corpora
   - `freq_noun.rds`: Noun frequency table for both corpora
   - `col_verb_coca.rds`: Verb collocation table in COCA
   - `col_verb_icle.rds`: Verb collocation table in ICLE (L1 Japanese)
   - `col_noun_coca.rds`: Noun collocation table in COCA
   - `col_noun_icle.rds`: Noun collocation table in ICLE (L1 Japanese)

### `3_shiny` folder
I deployed VisCol to Shiny App so that everyone can access the tool.
- `app.R`: It develops a web application VisCol (online version), `network.R` + `list_comparison.R` + `concordance.R` + `app.R` in one file

### `4_thesis_fig` folder
I created some figures for the explanation in the thesis.
- `thesis_fig.ipynb`: It creates some figures for the thesis.
- `survey-multi.csv`: Results of the user testing survey (the part of multiple-choice questions)
- `coordinate-plane.jpg`: Fig 3.1
- `c-net.jpg`: Fig 3.2
- `c-list.jpg`: Fig 3.3
- `survey-results.jpg`: Fig 5.1