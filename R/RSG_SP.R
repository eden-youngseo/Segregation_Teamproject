# RSG_SP.R
#
# 공간 근접성 지수 (Spatial Proximity Index)
# White (1983)의 Spatial Proximity Index
#
# 인자:
#   data            sf 객체 또는 NULL
#   group_a         집단 A 열 이름 또는 숫자벡터
#   group_b         집단 B 열 이름 또는 숫자벡터
#   total           전체 인구 열 이름 또는 숫자벡터 (NULL이면 A+B)
#   proximity       거리 변환 방식: "exp", "exp2", "distance", "inverse_square"
#   dist_matrix     거리 행렬 (NULL이면 자동 계산)
#   self_distance   자기거리 처리: "zero" 또는 "area"
#   verbose         TRUE이면 결과값 출력
#
# 반환:
#   list(index = Spatial_Proximity, components = 중간 계산값)

#' @export
RSG_SP <- function(data = NULL, group_a, group_b, total = NULL,
                   proximity = "exp", dist_matrix = NULL,
                   self_distance = "zero", verbose = FALSE) {

  a_pop <- .rsg_resolve_pop(data, group_a)
  b_pop <- .rsg_resolve_pop(data, group_b)

  if (is.null(total)) {
    t_pop <- a_pop + b_pop
  } else {
    t_pop <- .rsg_resolve_pop(data, total)
  }

  n <- .rsg_validate_lengths(a_pop, b_pop, t_pop)

  N1 <- sum(a_pop, na.rm = TRUE)
  N2 <- sum(b_pop, na.rm = TRUE)
  N  <- sum(t_pop, na.rm = TRUE)

  if (N1 == 0 || N2 == 0 || N == 0) {
    return(invisible(list(index = NA_real_, summary = "집단 인구 0")))
  }

  d_mat <- .rsg_dist_matrix(data, dist_matrix, n)
  d_mat <- as.matrix(d_mat)

  self_distance <- match.arg(self_distance, c("zero", "area"))

  if (self_distance == "zero") {
    diag(d_mat) <- 0
  }

  if (self_distance == "area") {
    diag(d_mat) <- 0

    if (!is.null(data) && inherits(data, "sf")) {
      geom_type <- as.character(sf::st_geometry_type(data))
      is_polygon <- grepl("POLYGON", geom_type)

      if (all(is_polygon)) {
        diag(d_mat) <- 0.6 * sqrt(as.numeric(sf::st_area(data)))
      }
    }
  }

  proximity <- match.arg(proximity, c("exp", "exp2", "distance", "inverse_square"))

  F <- switch(
    proximity,
    exp = exp(-d_mat),
    exp2 = exp(-2 * d_mat),
    distance = d_mat,
    inverse_square = {
      if (any(d_mat == 0, na.rm = TRUE)) {
        stop("inverse_square는 거리가 0이면 계산할 수 없습니다. self_distance = 'area' 또는 다른 proximity를 사용하세요.")
      }
      1 / (d_mat^2)
    }
  )

  P11 <- sum((a_pop %o% a_pop) * F, na.rm = TRUE) / (N1^2)
  P22 <- sum((b_pop %o% b_pop) * F, na.rm = TRUE) / (N2^2)
  P12 <- sum((a_pop %o% b_pop) * F, na.rm = TRUE) / (N1 * N2)
  P00 <- sum((t_pop %o% t_pop) * F, na.rm = TRUE) / (N^2)

  if (P00 == 0 || is.na(P00)) {
    return(invisible(list(index = NA_real_, summary = "P00 계산 불가")))
  }

  sp_value <- (N1 * P11 + N2 * P22) / ((N1 + N2) * P00)

  if (proximity == "distance") {
    sp_value <- 1 / sp_value
  }

  components <- list(
    P00 = P00,
    P11 = P11,
    P22 = P22,
    P12 = P12,
    N1 = N1,
    N2 = N2
  )

  if (verbose) cat(sprintf("Spatial Proximity Index: %.4f\n", sp_value))

  invisible(list(index = sp_value, components = components))
}
