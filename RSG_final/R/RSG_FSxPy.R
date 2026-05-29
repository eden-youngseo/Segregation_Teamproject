# RSG_FSxPy.R
#
# 흐름 기반 공간 노출지수 (Flow-based Spatial Exposure Index)
# Liu, Q., Liu, M., & Ye, X. (2021). Cartography and Geographic Information Science,
# 48(6), 530-545. DOI: 10.1080/15230406.2021.1965915
#
# 인자:
#   data        sf 객체 또는 NULL
#   group_a     집단 a 열이름 또는 숫자벡터 (노출 주체)
#   group_b     집단 b 열이름 또는 숫자벡터 (노출 대상)
#   total       전체 인구 열이름 또는 숫자벡터
#   od_matrix   n×n OD 이동량 행렬 ([i,j] = 구역 i→j 이동 수)
#   alpha       최상위 계층 자기혼합비율 하한 (기본값 0.3)
#   beta        최하위 계층 자기혼합비율 상한 (기본값 1.0)
#   max_level   최대 계층 수 (기본값 6L)
#   hierarchy   사전 계산된 계층 벡터 (NULL이면 자동)
#   verbose     TRUE이면 콘솔 출력
#
# 반환: list(index = 전역 FSxPy, local = 구역별 기여값 벡터)


# Lorenz curve 기반 계층 수준 자동 계산 (1 = 최상위/가장 활성, max_level = 최하위)
.rsg_lorenz_hierarchy <- function(outflow_totals, max_level = 6L) {
  n         <- length(outflow_totals)
  h         <- rep(max_level, n)
  remaining <- seq_len(n)
  level     <- 1L

  while (level < max_level && length(remaining) > 1L) {
    flows <- outflow_totals[remaining]
    ord   <- order(flows)
    cum_n <- seq_along(ord) / length(ord)
    cum_f <- cumsum(flows[ord]) / sum(flows[ord])

    nr    <- length(ord)
    slope <- (cum_f[nr] - cum_f[nr - 1L]) / (cum_n[nr] - cum_n[nr - 1L])
    if (slope <= 1) break

    x_thr     <- 1 - 1 / slope
    cum_rank  <- rank(flows, ties.method = "first") / length(flows)
    hot_local <- which(cum_rank > x_thr)
    if (length(hot_local) == 0L) break

    h[remaining[hot_local]] <- level
    remaining <- remaining[-hot_local]
    level     <- level + 1L
  }
  h
}


# OD 행렬 → ω 가중치 행렬 구성 (식3: 대각 = alpha + (h-1)*(beta-alpha)/(max_level-1))
.rsg_build_omega <- function(phi, h, alpha, beta, max_level) {
  n       <- nrow(phi)
  phi_adj <- phi
  diag(phi_adj) <- 0

  row_sums  <- rowSums(phi_adj)
  zero_rows <- row_sums == 0
  if (any(zero_rows))
    warning(sprintf("구역 %s의 총 유출량이 0입니다. 균등 가중치로 대체합니다.",
                    paste(which(zero_rows), collapse = ", ")))

  denom <- ifelse(zero_rows, n, row_sums)
  omega <- phi_adj / denom
  if (any(zero_rows)) omega[zero_rows, ] <- 1 / n

  omega_ii <- alpha + (h - 1L) * (beta - alpha) / (max_level - 1L)

  for (i in seq_len(n)) {
    off_sum <- 1 - omega[i, i]
    if (off_sum > 0)
      omega[i, -i] <- omega[i, -i] / off_sum * (1 - omega_ii[i])
    omega[i, i] <- omega_ii[i]
  }
  omega
}


#' @export
RSG_FSxPy <- function(data      = NULL,
                      group_a,
                      group_b,
                      total,
                      od_matrix,
                      alpha     = 0.3,
                      beta      = 1.0,
                      max_level = 6L,
                      hierarchy = NULL,
                      verbose   = FALSE) {

  pop_x  <- .rsg_resolve_pop(data, group_a)
  pop_y  <- .rsg_resolve_pop(data, group_b)
  pop_t  <- .rsg_resolve_pop(data, total)
  n      <- .rsg_validate_lengths(pop_x, pop_y, pop_t)

  Total_X <- sum(pop_x, na.rm = TRUE)
  if (Total_X == 0) return(invisible(list(index = NA_real_, summary = "기준 집단 인구 0")))

  phi <- .rsg_validate_od(od_matrix, n)

  if (!is.numeric(alpha) || !is.numeric(beta) || alpha < 0 || beta > 1 || alpha > beta)
    stop("alpha, beta는 0 ≤ alpha ≤ beta ≤ 1 범위여야 합니다.")

  max_level <- as.integer(max_level)
  if (max_level < 2L) stop("max_level은 2 이상이어야 합니다.")

  if (is.null(hierarchy)) {
    phi_nodiag <- phi; diag(phi_nodiag) <- 0
    hierarchy  <- .rsg_lorenz_hierarchy(rowSums(phi_nodiag), max_level)
  } else {
    hierarchy <- as.integer(hierarchy)
    if (length(hierarchy) != n) stop("hierarchy 길이가 구역 수와 다릅니다.")
  }

  omega   <- .rsg_build_omega(phi, hierarchy, alpha, beta, max_level)
  mixed_y <- .rsg_local_intensity(omega, pop_y)
  mixed_t <- .rsg_local_intensity(omega, pop_t)

  valid   <- mixed_t > 0
  local_v <- rep(NA_real_, n)
  local_v[valid] <- (pop_x[valid] / Total_X) * (mixed_y[valid] / mixed_t[valid])

  fsxpy_val <- sum(local_v, na.rm = TRUE)

  if (verbose) cat(sprintf("FSxPy: %.4f\n", fsxpy_val))

  invisible(list(index = fsxpy_val, local = local_v))
}
