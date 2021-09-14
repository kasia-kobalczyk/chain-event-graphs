#' # missingnes data
#' mod_df <- df[, c("miss1", "miss2", "miss3", "miss4")] %>% na.omit()
#' 
#' object <- full(mod_df)
#' 
#' #' Rename stages of a full model.
#' #' 
#' #' Returns an object with unique stage numbering across all variables
#' #'
#' rename_stages <- function(object) {
#'   check_sevt(object)
#'   vars <- sevt_varnames(object)
#'   counter <- length(object$tree[[vars[1]]])
#'   for (v in vars[-1]) {
#'     stages <- object$stages[[v]]
#'     object$stages[[v]] <- as.character(seq(counter + 1, counter + length(stages)))
#'     counter <- counter + length(stages)
#'   }
#'   object
#' }
#' 
#' #' Join two stages on two different variable levels
#' #'
#' #' Warning: All stages must have unique names across the variables
#' join_multi_stages2 <- function(object, s1, s2) {
#'   check_sevt(object)
#'   s1 <- as.character(s1)
#'   s2 <- as.character(s2)
#'   if (s1 == s2) {
#'     stop("Stages are the same. Cannot join")
#'   }
#' 
#'   for
#'   
#'   
#'   st1 <- object$stages[[v1]]
#'   st2 <- object$stages[[v2]]
#'   object$stages[[v1]][st1 == s1] <- s1
#'   object$stages[[v2]][st2 == s2] <- s1
#'   if (!is.null(object$prob)) {
#'     p1 <- object$prob[[v1]][[s1]]
#'     p2 <- object$prob[[v2]][[s2]]
#'     n1 <- attr(p1, "n")
#'     n2 <- attr(p2, "n")
#'     if (is.null(n1)) n1 <- 1
#'     if (is.null(n2)) n2 <- 1
#'     if (is.null(object$lambda)) {
#'       object$lambda <- 0
#'     }
#'     ct1 <-
#'       ifelse(is.na(p1), 0, p1) * (n1 + object$lambda * k1) - object$lambda
#'     ct2 <-
#'       ifelse(is.na(p2), 0, p2) * (n2 + object$lambda * k1) - object$lambda
#'     dll <-
#'       sum(ct2[ct2 > 0] * log(p2[ct2 > 0])) +
#'       sum(ct1[ct1 > 0] * log(p1[ct1 > 0]))
#' 
#'     object$prob[[v1]][[s1]] <- ct2 + ct1 + object$lambda
#'     object$prob[[v2]][[s1]] <- ct2 + ct1 + object$lambda
#'     attr(object$prob[[v1]][[s1]], "n") <- n1 + n2
#'     attr(object$prob[[v2]][[s1]], "n") <- n1 + n2
#'     object$prob[[v1]][[s1]] <-
#'       object$prob[[v1]][[s1]] / sum(object$prob[[v1]][[s1]])
#'     object$prob[[v2]][[s1]] <-
#'       object$prob[[v2]][[s1]] / sum(object$prob[[v2]][[s1]])
#'     
#'     object$prob[[v1]][[s2]] <- NULL ## delete one of the two
#'     object$prob[[v2]][[s2]] <- NULL
#'     if (!is.null(object$ll)) {
#'       ## update log likelihood
#'       ct1 <- ct1 + ct2
#'       object$ll <-
#'         object$ll - dll + sum(ct1[ct1 > 0] *
#'                                 log(object$prob[[v1]][[s1]][ct1 > 0]))
#'       attr(object$ll, "df") <-
#'         attr(object$ll, "df") - length(object$prob[[v1]][[s1]]) - length(object$prob[[v2]][[s1]]) + 1
#'     }
#'   }
#'   return(object)
#' }
