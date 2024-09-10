get_metadata <- function(ID) {

  meta <- yaml::read_yaml("https://fcarepository.org/contexts.yaml")

  if (ID %in% names(meta)) {

    return(meta[[ID]])

  }

  return(NULL)

}

