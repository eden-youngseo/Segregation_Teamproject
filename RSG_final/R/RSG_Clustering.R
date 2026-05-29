# RSG_Clustering.R
#
# 둘레 기반 클러스터링 지수 (Perimeter-based Clustering Index)
# Lee, C.M. & Culhane, D.P. (1998). Environment and Planning B, 25(3), 327-343.
#
# 인자:
#   data            sf 폴리곤 객체 또는 NULL (boundary_matrix 직접 입력 시 NULL 허용)
#   group_a         관심 집단 열이름 또는 숫자벡터
#   total           전체 인구 열이름 또는 숫자벡터
#   boundary_matrix n×n 공유 경계선 길이 행렬 (NULL이면 data로부터 자동 계산)
#   verbose         TRUE이면 콘솔 출력
#
# 반환: list(index = I^c, LQ = 입지계수, l = 이진 지시자, summary = 상태메시지)

.rsg_boundary_matrix <- function(data, boundary_matrix, n) {
  if (!is.null(boundary_matrix)) {
    if (!is.matrix(boundary_matrix) || nrow(boundary_matrix) != n || ncol(boundary_matrix) != n)
      stop("boundary_matrix는 n×n 정방행렬이어야 합니다.")
    return(boundary_matrix)
  }
  if (is.null(data) || !inherits(data, "sf"))
    stop("boundary_matrix를 자동 계산하려면 sf 폴리곤 객체(data)가 필요합니다.")

  require(sf)

  b_mat <- matrix(0, n, n)
  geom  <- sf::st_geometry(data)

  # (1) 희소 인접 후보만 추출 (평면 인덱스 기반, O(n log n))
  touches <- sf::st_touches(geom)
  if (length(touches) == 0L || sum(lengths(touches)) == 0L) return(b_mat)

  # (2) 상삼각 페어 (i < j) 를 벡터로 일괄 구성 — 이중 for 루프 제거
  i_idx <- rep.int(seq_len(n), lengths(touches))
  j_idx <- unlist(touches, use.names = FALSE)
  keep  <- i_idx < j_idx
  i_idx <- i_idx[keep]
  j_idx <- j_idx[keep]
  if (length(i_idx) == 0L) return(b_mat)

  # (3) 폴리곤 → 경계선 일괄 변환 (라인×라인 교차로 비용 절감)
  bnd <- sf::st_boundary(geom)

  # (4) 페어 단위 st_intersection 일괄 처리
  #     bnd[k] 형태로 sfc 길이-1 부분집합을 넘겨 sfc 타입을 유지
  shared_len <- suppressWarnings(mapply(
    function(i, j) {
      inter <- sf::st_intersection(bnd[i], bnd[j])
      if (length(inter) == 0L) return(0)
      as.numeric(sum(sf::st_length(inter)))
    },
    i_idx, j_idx, USE.NAMES = FALSE
  ))
  shared_len[is.na(shared_len) | shared_len < 0] <- 0

  # (5) 행렬 인덱싱으로 대칭 채우기 (벡터 할당 1회)
  pos <- shared_len > 0
  if (any(pos)) {
    b_mat[cbind(i_idx[pos], j_idx[pos])] <- shared_len[pos]
    b_mat[cbind(j_idx[pos], i_idx[pos])] <- shared_len[pos]
  }
  b_mat
}


#' @export
RSG_Clustering <- function(data = NULL, group_a, total,
                           boundary_matrix = NULL,
                           verbose = FALSE) {

  a_pop <- .rsg_resolve_pop(data, group_a)
  t_pop <- .rsg_resolve_pop(data, total)
  n     <- .rsg_validate_lengths(a_pop, t_pop)

  X <- sum(a_pop, na.rm = TRUE)
  P <- sum(t_pop, na.rm = TRUE)
  if (X == 0 || P == 0)
    return(invisible(list(index = NA_real_, summary = "집단 또는 전체 인구 0")))

  # 입지계수 LQ → 이진 지시자 l_i
  LQ <- (a_pop / t_pop) / (X / P)
  LQ[is.nan(LQ) | is.na(LQ)] <- 0
  l_i <- as.integer(LQ >= 1)

  # 공유 경계선 길이 행렬
  b_mat <- .rsg_boundary_matrix(data, boundary_matrix, n)

  # I^c = 1 - sum_ij |l_i - l_j| * b_ij / sum_ij b_ij
  diff_mat <- abs(outer(l_i, l_i, "-"))
  num <- sum(diff_mat * b_mat)
  den <- sum(b_mat)
  Ic  <- if (den > 0) 1 - num / den else NA_real_

  if (verbose) cat(sprintf("Clustering Index (I^c): %.4f\n", Ic))

  invisible(list(index = Ic, LQ = LQ, l = l_i,
                 summary = sprintf("고농도 셀 %d / %d", sum(l_i), n)))
}
