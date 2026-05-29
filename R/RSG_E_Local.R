# RSG_E_Local.R

# 국지적 공간 노출지수 (Local Spatial Exposure Index)

#' @export
RSG_E_Local <- function(data = NULL, group_a, group_b, total,
                        bandwidth, dist_matrix = NULL, verbose = FALSE) {

  a_pop <- .rsg_resolve_pop(data, group_a)
  b_pop <- .rsg_resolve_pop(data, group_b)
  t_pop <- .rsg_resolve_pop(data, total)
  n     <- .rsg_validate_lengths(a_pop, b_pop, t_pop)

  Total_A <- sum(a_pop, na.rm = TRUE)
  if (Total_A == 0) {
    return(invisible(list(index = rep(NA_real_, n), summary = "기준 집단 인구 0")))
  }

  d_mat <- .rsg_dist_matrix(data, dist_matrix, n)
  W     <- .rsg_gaussian_weights(d_mat, bandwidth)
  L_b   <- .rsg_local_intensity(W, b_pop)
  L_t   <- .rsg_local_intensity(W, t_pop)

  local_prop <- ifelse(L_t > 0, L_b / L_t, 0)
  local_exp  <- (a_pop / Total_A) * local_prop

  if (verbose) print(head(local_exp))

  invisible(list(index = local_exp))
}
