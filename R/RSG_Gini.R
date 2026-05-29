# RSG_Gini.R
#
# 공간 Gini 분리지수 (Spatial Gini Index of Segregation)
# Dawkins, C.J. (2004). Urban Studies, 41(4), 833-851.
#
# 인자:
#   data         sf 객체 또는 NULL
#   group_a      집단 A 열이름 또는 숫자벡터 (소수집단 W)
#   group_b      집단 B 열이름 또는 숫자벡터 (다수집단 B)
#   dist_matrix  거리 행렬 (NULL이면 data로부터 자동 계산)
#   pattern_type 공간 reranking 기준: "nearest" (최근접) 또는 "kth" (k번째 근접)
#   k            pattern_type = "kth"일 때 k값 (기본 1)
#   verbose      TRUE이면 콘솔 출력
#
# 반환: list(index = GST 표준화 공간 Gini, aspatial = G0 비공간 Gini,
#            GS = 공간 Gini, ES = 잔차(G0 - GS), summary = 상태메시지)


#' @export
RSG_Gini <- function(data = NULL, group_a, group_b,
                     dist_matrix = NULL,
                     pattern_type = "nearest", k = 1,
                     verbose = FALSE) {

  a_pop <- .rsg_resolve_pop(data, group_a)
  b_pop <- .rsg_resolve_pop(data, group_b)
  n     <- .rsg_validate_lengths(a_pop, b_pop)

  A_total <- sum(a_pop)
  B_total <- sum(b_pop)
  if (A_total == 0 || B_total == 0)
    return(invisible(list(index = NA_real_, summary = "집단 인구 0")))

  # 인구 비중 — 식(W*, B*)
  w_share <- a_pop / A_total
  b_share <- b_pop / B_total

  # B/W 비율 내림차순 정렬
  ratio <- ifelse(w_share == 0,
                  ifelse(b_share == 0, 0, .Machine$double.xmax),
                  b_share / w_share)
  ord    <- order(ratio, decreasing = TRUE)
  w_star <- w_share[ord]
  b_star <- b_share[ord]

  # Silber sign 행렬 G (대각 0, 상삼각 -1, 하삼각 +1)
  G_mat <- matrix(0, n, n)
  G_mat[upper.tri(G_mat)] <- -1
  G_mat[lower.tri(G_mat)] <-  1

  # 비공간 Gini (G0) — 식: [W*]' G [B*]
  G0 <- as.numeric(t(w_star) %*% G_mat %*% b_star)

  # 공간 reranking — dist_matrix 기반
  d_mat <- .rsg_dist_matrix(data, dist_matrix, n)
  d_ord <- d_mat[ord, ord]
  diag(d_ord) <- Inf

  if (pattern_type == "nearest") {
    rerank_idx <- apply(d_ord, 1, which.min)
  } else if (pattern_type == "kth") {
    if (k < 1 || k > n - 1) stop("k는 1 ≤ k ≤ n-1 범위여야 합니다.")
    rerank_idx <- apply(d_ord, 1, function(d) order(d)[k])
  } else {
    stop("pattern_type은 'nearest' 또는 'kth'여야 합니다.")
  }

  # 공간 reranked 벡터 → GS, ES, GST
  w_re <- w_star[rerank_idx]
  b_re <- b_star[rerank_idx]
  GS   <- as.numeric(t(w_re) %*% G_mat %*% b_re)
  ES   <- G0 - GS
  GST  <- if (abs(G0) > .Machine$double.eps) GS / G0 else NA_real_

  if (verbose) {
    cat(sprintf("Aspatial Gini (G0)        : %.4f\n", G0))
    cat(sprintf("Spatial Gini (GS)         : %.4f\n", GS))
    cat(sprintf("Residual ES = G0 - GS     : %.4f\n", ES))
    cat(sprintf("Standardized GST = GS/G0  : %.4f\n", GST))
  }

  invisible(list(index = GST, aspatial = G0, GS = GS, ES = ES,
                 summary = sprintf("pattern_type=%s", pattern_type)))
}
