# RSG_D_Local.R
#
# 국지적 공간 상이지수 (Local Spatial Dissimilarity Index)
# Feitosa et al. (2007). Int. J. Geographical Information Science, 21(3), 299-323. 식 6
# RSG_D_Global의 구역별 분해값
#
# 인자:
#   data        sf 객체 또는 NULL
#   group_a     집단 A 열이름 또는 숫자벡터
#   group_b     집단 B 열이름 또는 숫자벡터
#   total       전체 인구 열이름 또는 숫자벡터
#   bandwidth   Gaussian 커널 반경 (양수)
#   dist_matrix 거리 행렬 (NULL이면 자동 계산)
#   verbose     TRUE이면 결과 앞 6행 콘솔 출력
#
# 반환: list(index = data.frame(Zone, Local_Dissimilarity), summary = 상태메시지)

#' @export
RSG_D_Local <- function(data = NULL, group_a, group_b, total, bandwidth, dist_matrix = NULL, verbose = FALSE) {

  a_pop <- .rsg_resolve_pop(data, group_a)
  b_pop <- .rsg_resolve_pop(data, group_b)
  t_pop <- .rsg_resolve_pop(data, total)
  n     <- .rsg_validate_lengths(a_pop, b_pop, t_pop)

  Total_A <- sum(a_pop)
  Total_B <- sum(b_pop)
  Total_T <- sum(t_pop)

  if (Total_A == 0 || Total_B == 0 || Total_T == 0) {
    return(invisible(list(index = rep(NA, n), summary = "기준 집단 또는 전체 인구가 0입니다.")))
  }

  P_A <- Total_A / Total_T
  P_B <- Total_B / Total_T

  d_mat <- .rsg_dist_matrix(data, dist_matrix, n)
  W     <- .rsg_gaussian_weights(d_mat, bandwidth)
  L_a   <- .rsg_local_intensity(W, a_pop)
  L_t   <- .rsg_local_intensity(W, t_pop)

  local_prop <- ifelse(L_t > 0, L_a / L_t, 0)

  # Feitosa(2007) 국지적 상이지수 (식 6) 2개 집단 최적화 연산
  local_d <- abs(local_prop - P_A) / (2 * P_A * P_B)


  if (verbose) print(head(local_d))

  invisible(list(index = local_d))
}
