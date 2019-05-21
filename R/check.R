.check_concepts <- function(L, I) {

  all(sapply(L,
             function(p)
               p[[2]] == .closure(p[[2]], I)))

}
