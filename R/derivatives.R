.intent <- function(A, I) {

  # Names of objects and attributes
  attributes <- colnames(I)
  objects <- rownames(I)

  # Membership of each object in fuzzy set A
  ms_objects <- .get_object_memberships(A, I)

  # Use the derivation expression
  ms <- sapply(attributes,

               # The minimum of (A(g) -> I(g, m))
               function(m) {

                 min(
                   .I.(
                     ms_objects,
                     I[ , m]
                   )
                 )

               }

  )

  # Return a fuzzy set of the attributes,
  # with the computed memberships
  gset(support = attributes, memberships = ms)

}

.extent <- function(B, I) {

  # Names of objects and attributes
  attributes <- colnames(I)
  objects <- rownames(I)

  # Memberships of the attributes in fuzzy set B
  # Just assign 0 to all attributes in B if it's empty
  ms_attr <- .get_attribute_memberships(B, I)

  # Compute the memberships of the derived extention
  ms <- sapply(objects,

               # The minimum of (B(m) -> I(g, m))
               function(g) {

                 min(
                   .I.(
                     ms_attr,
                     I[g, ]
                   )
                 )

               }

  )

  # Return a fuzzy set of the objects with the
  # computed attributes
  gset(support = objects, memberships = ms)

}

.closure <- function(B, I) {

  # Double derivative starting from a fuzzy set of attributes.
  .intent(.extent(B, I), I)

}
