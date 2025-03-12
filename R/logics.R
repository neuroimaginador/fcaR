get_implication <- function(name) {

  if (name == "Zadeh") {

    return(implication_Zadeh);

  }
  if (name == "Lukasiewicz") {

    return(implication_Lukasiewicz);

  }

  if (name == "Godel") {

    return(implication_Godel);

  }

  if (name == "Product") {

    return(implication_Product);

  }

  return(NULL);
}

get_tnorm <- function(name) {

  if (name == "Zadeh") {

    return(tnorm_Zadeh);

  }
  if (name == "Lukasiewicz") {

    return(tnorm_Lukasiewicz);

  }

  if (name == "Godel") {

    return(tnorm_Godel);

  }

  if (name == "Product") {

    return(tnorm_Product);

  }

  return(NULL);

}
