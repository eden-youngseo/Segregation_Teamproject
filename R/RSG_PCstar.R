# RSG_PCstar.R
#
# Morgan의 PC* 공간 노출 지수
# Morgan, B.S. (1983). Area, 15(3), 211-216.
#
# 인자:
#   data        sf 객체 또는 NULL
#   group_a     집단 A 열이름 또는 숫자벡터
#   group_b     집단 B 열이름 또는 숫자벡터
#   total       전체 인구 열이름 또는 숫자벡터
#   dist_matrix 거리 행렬 (NULL이면 자동 계산)
#   a_param     감쇄함수 절편 (log10 스케일)
#   b_param     감쇄함수 기울기
#   m           감쇄함수 지수 (권장 범위: 0 < m ≤ 0.5)
#   verbose     TRUE이면 콘솔 출력
#
# 반환: list(index = PC*값)

#' @export
RSG_PCstar <- function(data = NULL, group_a, group_b, total, dist_matrix = NULL,
                       a_param, b_param, m, verbose = FALSE) {

  a_pop <- .rsg_resolve_pop(data, group_a)
  b_pop <- .rsg_resolve_pop(data, group_b)
  t_pop <- .rsg_resolve_pop(data, total)
  n     <- .rsg_validate_lengths(a_pop, b_pop, t_pop)

  Total_A <- sum(a_pop)
  if (Total_A == 0) return(invisible(list(index = NA_real_, summary = "기준 집단 인구 0")))

  d_mat <- .rsg_dist_matrix(data, dist_matrix, n)
  W     <- 10^(a_param - b_param * (d_mat^m))

  L_b <- .rsg_local_intensity(W, b_pop)
  L_t <- .rsg_local_intensity(W, t_pop)

  local_prop <- ifelse(L_t > 0, L_b / L_t, 0)
  pcstar_val <- sum((a_pop / Total_A) * local_prop, na.rm = TRUE)

  if (verbose) cat(sprintf("Morgan's PC*: %.4f\n", pcstar_val))

  invisible(list(index = pcstar_val))
}
