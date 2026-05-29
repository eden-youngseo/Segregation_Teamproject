# RSG_S.R
#
# Surface-based S Index — O'Sullivan & Wong (2007)
# O'Sullivan, D. & Wong, D.W.S. (2007). Geographical Analysis, 39(2), 147-168.
#
# 인자:
#   data        sf 객체 또는 NULL
#   group_a     집단 A 열이름 또는 숫자벡터
#   group_b     집단 B 열이름 또는 숫자벡터
#   bandwidth   커널 반경 (단일값 또는 벡터, 논문 권장: 6개 bandwidth 사용)
#   dist_matrix 거리 행렬 (NULL이면 자동 계산)
#   verbose     TRUE이면 콘솔 출력
#
# 반환: list(index = bw별 S벡터, aspatial = 비공간 D, summary = 상태메시지)

#' @export
RSG_S <- function(data = NULL, group_a, group_b,
                  bandwidth, dist_matrix = NULL, verbose = FALSE) {

  a_pop <- .rsg_resolve_pop(data, group_a)
  b_pop <- .rsg_resolve_pop(data, group_b)
  n     <- .rsg_validate_lengths(a_pop, b_pop)

  A_total <- sum(a_pop)
  B_total <- sum(b_pop)
  if (A_total == 0 || B_total == 0)
    return(invisible(list(index = NA, aspatial = NA, summary = "집단 인구 0")))

  # 인구 비율 — 식(7): pWi = wi/W, pBi = bi/B
  p_A <- a_pop / A_total
  p_B <- b_pop / B_total

  # 비공간 기준값: aspatial D — 식(1)
  D_asp <- 0.5 * sum(abs(p_A - p_B))

  # 거리 행렬
  d_mat  <- .rsg_dist_matrix(data, dist_matrix, n)
  bw_vec <- as.numeric(bandwidth)

  s_sp <- sapply(bw_vec, function(bw) {
    K       <- exp(-0.5 * (d_mat / bw)^2)
    pA_surf <- as.numeric(K %*% p_A) / n
    pB_surf <- as.numeric(K %*% p_B) / n
    # S = 1 - V_intersection / V_union — 식(9)
    1 - sum(pmin(pA_surf, pB_surf)) / sum(pmax(pA_surf, pB_surf))
  })
  names(s_sp) <- paste0("bw=", bw_vec)

  if (verbose) {
    cat(sprintf("Aspatial D: %.4f\n", D_asp))
    for (i in seq_along(bw_vec))
      cat(sprintf("bw=%-6.2f S: %.4f\n", bw_vec[i], s_sp[i]))
  }

  invisible(list(index = s_sp, aspatial = D_asp))
}
