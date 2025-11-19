# Data for Differential Diagnosis for Schizophrenia

A subset of the COBRE dataset has been retrieved, by querying
SchizConnect for 105 patients with neurological and clinical symptoms,
collecting also their corresponding diagnosis.

## Usage

``` r
cobre61
```

## Format

A matrix with 105 rows and 61 columns. Column names are related to
different scales for depression and Schizophrenia:

- COSAS_n:

  The *Simpson-Angus Scale*, 7 items to evaluate Parkinsonism-like
  alterations, related to schizophrenia, in an individual.

- FIPAN_n:

  The *Positive and Negative Syndrome Scale*, a set of 29 attributes
  measuring different aspects and symptoms in schizophrenia.

- FICAL_n:

  The *Calgary Depression Scale for Schizophrenia*, 9 items (attributes)
  assessing the level of depression in schizophrenia, differentiating
  between positive and negative aspects of the disease.

- SCIDII_n:

  The *Structured Clinical Interview for DSM-III-R Personality
  Disorders*, with 14 variables related to the presence of signs
  affecting personality.

- dx_ss:

  if `TRUE`, the diagnosis is strict schizophrenia.

- dx_other:

  it `TRUE`, the diagnosis is other than schizophrenia, including
  schizoaffective, bipolar disorder and major depression.

In summary, the dataset consists in the previous 59 attributes related
to signs or symptoms, and 2 attributes related to diagnosis (these
diagnoses are mutually exclusive, thus only one of them is assigned to
each patient). This makes a dataset with 105 objects (patients) and 61
attributes to explore. The symptom attributes are multi-valued.

Thus, according to the specific scales used, all attributes are fuzzy
and graded. For a given attribute (symptom), the available grades range
from *absent* to *extreme*, with *minimal*, *mild*, *moderate*,
*moderate severe* and *severe* in between.

These fuzzy attributes are mapped to values in the interval \[0, 1\].

## Source

Aine, C. J., Bockholt, H. J., Bustillo, J. R., Cañive, J. M., Caprihan,
A., Gasparovic, C., ... & Liu, J. (2017). Multimodal neuroimaging in
schizophrenia: description and dissemination. Neuroinformatics, 15(4),
343-364. <https://pubmed.ncbi.nlm.nih.gov/26142271/>
