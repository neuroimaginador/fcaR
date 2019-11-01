#' Data for Differential Diagnosis for Schizophrenia
#'
#' A subset of the COBRE dataset has been retrieved, by querying SchizConnect for 105 patients with neurological and clinical symptoms, collecting also their corresponding diagnosis.
#'
#' @format
#' A matrix with 105 rows and 32 columns. Column names are related to different scales for depression and Schizophrenia:
#' \describe{
#'   \item{COSAS_n}{The _Simpson-Angus Scale_, 7 items to evaluate Parkinsonism-like alterations, related to schizophrenia, in an individual.}
#'   \item{FICAL_n}{The _Calgary Depression Scale for Schizophrenia_, 9 items (attributes) assessing the level of depression in schizophrenia, differentiating between positive and negative aspects of the disease.}
#'   \item{SCIDII_n}{The _Structured Clinical Interview for DSM-III-R Personality Disorders_, with 14 variables related to the presence of signs affecting personality.}
#'   \item{dx_ss}{if \code{TRUE}, the diagnosis is strict schizophrenia.}
#'   \item{dx_other}{it \code{TRUE}, the diagnosis is other than schizophrenia, including schizoaffective, bipolar disorder and major depression.}
#' }
#'
#' In summary, the dataset consists in the previous 30 attributes related to signs or symptoms, and 2 attributes related to diagnosis (these diagnoses are mutually exclusive, thus only one of them is assigned to each patient). This makes a dataset with 105 objects (patients) and 32 attributes to explore. The symptom attributes are multi-valued.
#'
#' Thus, according to the specific scales used, all attributes are fuzzy and graded. For a given attribute (symptom), the available grades range from _absent_ to _extreme_, with _minimal_, _mild_, _moderate_, _moderate severe_ and _severe_ in between.
#'
#' These fuzzy attributes are mapped to values in the interval \[0, 1\].
#'
#' @source
#' Aine, C. J., Bockholt, H. J., Bustillo, J. R., Cañive, J. M., Caprihan, A., Gasparovic, C., ... & Liu, J. (2017). Multimodal neuroimaging in schizophrenia: description and dissemination. Neuroinformatics, 15(4), 343-364.
#' \url{http://schizconnect.org/}
"cobre32"

#' Data for Differential Diagnosis for Schizophrenia
#'
#' A subset of the COBRE dataset has been retrieved, by querying SchizConnect for 105 patients with neurological and clinical symptoms, collecting also their corresponding diagnosis.
#'
#' @format
#' A matrix with 105 rows and 61 columns. Column names are related to different scales for depression and Schizophrenia:
#' \describe{
#'   \item{COSAS_n}{The _Simpson-Angus Scale_, 7 items to evaluate Parkinsonism-like alterations, related to schizophrenia, in an individual.}
#'   \item{FIPAN_n}{The _Positive and Negative Syndrome Scale_, a set of 29 attributes measuring different aspects and symptoms in schizophrenia.}
#'   \item{FICAL_n}{ The _Calgary Depression Scale for Schizophrenia_, 9 items (attributes) assessing the level of depression in schizophrenia, differentiating between positive and negative aspects of the disease.}
#'   \item{SCIDII_n}{The _Structured Clinical Interview for DSM-III-R Personality Disorders_, with 14 variables related to the presence of signs affecting personality.}
#'   \item{dx_ss}{if \code{TRUE}, the diagnosis is strict schizophrenia.}
#'   \item{dx_other}{it \code{TRUE}, the diagnosis is other than schizophrenia, including schizoaffective, bipolar disorder and major depression.}
#' }
#'
#' In summary, the dataset consists in the previous 59 attributes related to signs or symptoms, and 2 attributes related to diagnosis (these diagnoses are mutually exclusive, thus only one of them is assigned to each patient). This makes a dataset with 105 objects (patients) and 61 attributes to explore. The symptom attributes are multi-valued.
#'
#' Thus, according to the specific scales used, all attributes are fuzzy and graded. For a given attribute (symptom), the available grades range from _absent_ to _extreme_, with _minimal_, _mild_, _moderate_, _moderate severe_ and _severe_ in between.
#'
#' These fuzzy attributes are mapped to values in the interval \[0, 1\].
#'
#' @source
#' Aine, C. J., Bockholt, H. J., Bustillo, J. R., Cañive, J. M., Caprihan, A., Gasparovic, C., ... & Liu, J. (2017). Multimodal neuroimaging in schizophrenia: description and dissemination. Neuroinformatics, 15(4), 343-364.
#' \url{http://schizconnect.org/}
"cobre61"
