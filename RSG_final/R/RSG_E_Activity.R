# RSG_E_Activity.R
#
# 활동공간 기반 노출지수 (Activity Space-Bounded Exposure Index)
# Wong, D. & Shaw, S.L. (2011). Journal of Geographical Systems, 13(2), 127-145.
#
# 인자:
#   data        sf 객체 또는 NULL
#   group_a     집단 A 열이름 또는 숫자벡터 (노출 주체)
#   group_b     집단 B 열이름 또는 숫자벡터 (노출 대상)
#   total       전체 인구 열이름 또는 숫자벡터
#   od_matrix   n×n OD 이동량 행렬 ([i,j] = 구역 i→j 이동 수)
#   verbose     TRUE이면 콘솔 출력
#
# 반환: list(index = 전역 노출지수, local = 구역별 기여값 벡터)

#' @export
RSG_E_Activity <- function(data = NULL, group_a, group_b, total,
                           od_matrix, verbose = FALSE) {

  a_pop <- .rsg_resolve_pop(data, group_a)
  b_pop <- .rsg_resolve_pop(data, group_b)
  t_pop <- .rsg_resolve_pop(data, total)
  n     <- .rsg_validate_lengths(a_pop, b_pop, t_pop)

  Total_A <- sum(a_pop, na.rm = TRUE)
  if (Total_A == 0) return(invisible(list(index = NA_real_, summary = "기준 집단 인구 0")))

  phi <- .rsg_validate_od(od_matrix, n)
  W   <- .rsg_normalize_od(phi)

  b_ratio    <- ifelse(t_pop > 0, b_pop / t_pop, 0)
  local_exp  <- .rsg_local_intensity(W, b_ratio)
  global_exp <- sum((a_pop / Total_A) * local_exp, na.rm = TRUE)

  if (verbose) cat(sprintf("Activity Space Exposure: %.4f\n", global_exp))

  invisible(list(index = global_exp, local = local_exp))
}
