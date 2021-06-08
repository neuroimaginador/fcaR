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
#' \url{https://pubmed.ncbi.nlm.nih.gov/26142271/}
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
#' \url{https://pubmed.ncbi.nlm.nih.gov/26142271/}
"cobre61"

#' Data for Tourist Destination in Las Vegas
#'
#' The dataset \code{vegas} is the binary translation of the Las Vegas Strip dataset (@moro2017stripping), which records more than 500 TripAdvisor reviews of hotels in Las Vegas Strip. The uninformative attributes (such as the user continent or the weekday of the review) are removed.
#'
#' @format
#' A matrix with 504 rows and 25 binary columns. Column names are related to different features of the hotels:
#' \describe{
#'   \item{Period of Stay}{4 categories are present in the original data, which produces as many binary variables: \code{Period of stay=Dec-Feb}, \code{Period of stay=Mar-May}, \code{Period of stay=Jun-Aug} and \code{Period of stay=Sep-Nov}.}
#'   \item{Traveler type}{Five binary categories are created from the original data: \code{Traveler type=Business}, \code{Traveler type=Couples}, \code{Traveler type=Families}, \code{Traveler type=Friends} and \code{Traveler type=Solo}.}
#'   \item{Pool, Gym, Tennis court, Spa, Casino, Free internet}{Binary variables for the services offered by each destination hotel}
#'   \item{Stars}{Five binary variables are created, according to the number of stars of the hotel, \code{Stars=3}, \code{Stars=3.5}, \code{Stars=4}, \code{Stars=4.5} and \code{Stars=5}.}
#'   \item{Score}{The score assigned in the review, from \code{Score=1} to \code{Score=5}.}
#' }
#'
#' @source
#' Moro, S., Rita, P., & Coelho, J. (2017). Stripping customers' feedback on hotels through data mining: The case of Las Vegas Strip. Tourism Management Perspectives, 23, 41-52.
"vegas"

#' Planets data
#'
#' This dataset records some properties of the planets in our solar system.
#'
#' @format
#' A matrix with 9 rows (the planets) and 7 columns, representing additional features of the planets:
#' \describe{
#'    \item{small}{1 if the planet is small, 0 otherwise.}
#'    \item{medium}{1 if the planet is medium-sized, 0 otherwise.}
#'    \item{large}{1 if the planet is large, 0 otherwise.}
#'    \item{near}{1 if the planet belongs in the inner solar system, 0 otherwise.}
#'    \item{far}{1 if the planet belongs in the outer solar system, 0 otherwise.}
#'    \item{moon}{1 if the planet has a natural moon, 0 otherwise.}
#'    \item{no_moon}{1 if the planet has no moon, 0 otherwise.}
#' }
#'
#' @source
#' Wille R (1982). “Restructuring Lattice Theory: An Approach Based on Hierarchies of Concepts.” In Ordered Sets, pp. 445–470. Springer.
"planets"
