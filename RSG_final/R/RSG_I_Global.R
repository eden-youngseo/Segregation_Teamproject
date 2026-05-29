# RSG_I_Global.R
#
# 전역 공간 고립지수 (Global Spatial Isolation Index)
# Feitosa et al. (2007). Int. J. Geographical Information Science, 21(3), 299-323.
#
# 인자:
#   data        sf 객체 또는 NULL
#   group_a     기준 집단 열이름 또는 숫자벡터
#   total       전체 인구 열이름 또는 숫자벡터
#   bandwidth   Gaussian 커널 반경 (양수)
#   dist_matrix 거리 행렬 (NULL이면 자동 계산)
#   zone_names  구역 이름 벡터 (NULL이면 자동)
#   verbose     TRUE이면 콘솔 출력
#
# 반환: list(index = 고립지수값 0~1, summary = 상태메시지)

#' @export
RSG_I_Global <- function(data = NULL, group_a, total, bandwidth, dist_matrix = NULL, zone_names = NULL, verbose = FALSE) {

  a_pop <- .rsg_resolve_pop(data, group_a)
  t_pop <- .rsg_resolve_pop(data, total)
  n     <- .rsg_validate_lengths(a_pop, t_pop)

  Total_A <- sum(a_pop)
  if (Total_A == 0) return(invisible(list(index = NA, summary = "기준 집단 인구 0")))

  d_mat <- .rsg_dist_matrix(data, dist_matrix, n)
  W     <- .rsg_gaussian_weights(d_mat, bandwidth)
  L_a   <- .rsg_local_intensity(W, a_pop)
  L_t   <- .rsg_local_intensity(W, t_pop)

  local_prop <- ifelse(L_t > 0, L_a / L_t, 0)
  isolation_val <- sum((a_pop / Total_A) * local_prop, na.rm = TRUE)

  if (verbose) cat(sprintf("Global Isolation: %.4f\n", isolation_val))

  invisible(list(index = isolation_val))
}
