# RSG_NSI.R
#
# Feitosa et al. (2007) Spatial Neighbourhood Sorting Index
# Feitosa, F.F. et al. (2007). International Journal of Geographical Information Science, 21(3), 299-323.
#
# 인자:
#   data        sf 객체 또는 NULL
#   group_a     집단 A 열이름 또는 숫자벡터 (기준집단)
#   group_b     집단 B 열이름 또는 숫자벡터 (비교집단)
#   total       전체 인구 열이름 또는 숫자벡터 (NULL이면 group_a + group_b)
#   bandwidth   커널 반경 (단일값 또는 벡터, 논문 권장: 여러 bandwidth 사용)
#   dist_matrix 거리 행렬 (NULL이면 자동 계산)
#   verbose     TRUE이면 콘솔 출력
#
# 반환: list(index = bw별 NSI벡터, aspatial = 비공간 NSI, summary = 상태메시지)

#' @export
RSG_NSI <- function(data = NULL, group_a, group_b, total = NULL,
                    bandwidth, dist_matrix = NULL, verbose = FALSE) {

  a_pop <- .rsg_resolve_pop(data, group_a)
  b_pop <- .rsg_resolve_pop(data, group_b)
  t_pop <- if (is.null(total)) a_pop + b_pop else .rsg_resolve_pop(data, total)
  n     <- .rsg_validate_lengths(a_pop, b_pop, t_pop)

  T_total <- sum(t_pop)
  if (T_total == 0) return(invisible(list(index = NA, aspatial = NA, summary = "전체 인구 0")))

  # 비공간 NSI (aspatial)
  t_m2    <- sum(b_pop) / T_total
  X_bar   <- t_m2
  X_j     <- ifelse(t_pop > 0, b_pop / t_pop, 0)
  s2_btn  <- sum(t_pop * (X_j - X_bar)^2) / T_total
  s2_tot  <- (1 - t_m2) * (0 - X_bar)^2 + t_m2 * (1 - X_bar)^2
  nsi_asp <- if (s2_tot > 0) s2_btn / s2_tot else NA

  # 공간 NSI — bandwidth별 계산
  d_mat  <- .rsg_dist_matrix(data, dist_matrix, n)
  bw_vec <- as.numeric(bandwidth)

  nsi_sp <- sapply(bw_vec, function(bw) {
    W          <- exp(-(d_mat^2) / (2 * bw^2))
    L_a        <- .rsg_local_intensity(W, a_pop)
    L_b        <- .rsg_local_intensity(W, b_pop)
    L_t        <- .rsg_local_intensity(W, t_pop)
    X_hat_j    <- ifelse(L_t > 0, L_b / L_t, 0)
    X_hat      <- sum(L_t * X_hat_j) / sum(L_t)
    s2_between <- sum(L_t * (X_hat_j - X_hat)^2) / sum(L_t)
    s2_total   <- (sum(L_a)/sum(L_t)) * (0 - X_hat)^2 + (sum(L_b)/sum(L_t)) * (1 - X_hat)^2
    if (s2_total == 0) return(NA_real_)
    s2_between / s2_total
  })
  names(nsi_sp) <- paste0("bw=", bw_vec)

  if (verbose) {
    cat(sprintf("Aspatial NSI: %.4f\n", nsi_asp))
    for (i in seq_along(bw_vec))
      cat(sprintf("bw=%-6.2f NSI: %.4f\n", bw_vec[i], nsi_sp[i]))
  }

  invisible(list(index = nsi_sp, aspatial = nsi_asp))
}
