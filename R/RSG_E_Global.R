# RSG_E_Global.R
#
# 전역 공간 노출지수 (Global Spatial Exposure Index)
#
# Feitosa et al. (2007) 기반 공간 노출지수
# 집단 A가 주변 공간에서 집단 B에 얼마나 노출되는지를 도시 전체 수준에서 계산
#
# 인자:
#   data          sf 객체 또는 NULL
#   group_a       집단 A 열 이름 또는 숫자벡터
#   group_b       집단 B 열 이름 또는 숫자벡터
#   total         전체 인구 열 이름 또는 숫자벡터
#   bandwidth     Gaussian 커널 반경 (양수)
#   dist_matrix   거리 행렬 (NULL이면 자동 계산)
#   zone_names    구역 이름 벡터 (NULL이면 자동)
#   verbose       TRUE이면 결과값을 콘솔에 출력
#
# 반환:
#   list(index = Global_Exposure)

#' @export
RSG_E_Global <- function(data = NULL, group_a, group_b, total,
                         bandwidth, dist_matrix = NULL,
                         zone_names = NULL, verbose = FALSE) {

  a_pop <- .rsg_resolve_pop(data, group_a)
  b_pop <- .rsg_resolve_pop(data, group_b)
  t_pop <- .rsg_resolve_pop(data, total)
  n     <- .rsg_validate_lengths(a_pop, b_pop, t_pop)

  Total_A <- sum(a_pop, na.rm = TRUE)
  if (Total_A == 0) {
    return(invisible(list(index = NA_real_, summary = "기준 집단 인구 0")))
  }

  d_mat <- .rsg_dist_matrix(data, dist_matrix, n)
  W     <- .rsg_gaussian_weights(d_mat, bandwidth)
  L_b   <- .rsg_local_intensity(W, b_pop)
  L_t   <- .rsg_local_intensity(W, t_pop)

  local_prop <- ifelse(L_t > 0, L_b / L_t, 0)
  exposure_val <- sum((a_pop / Total_A) * local_prop, na.rm = TRUE)

  if (verbose) cat(sprintf("Global Exposure: %.4f\n", exposure_val))

  invisible(list(index = exposure_val))
}
