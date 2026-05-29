# 열 이름이나 숫자 벡터를 파싱하여 순수 숫자 벡터로 반환
.rsg_resolve_pop <- function(data, col) {
  if (is.numeric(col)) {
    return(col)
  } else if (!is.null(data) && is.character(col) && col %in% names(data)) {
    # sf 객체인 경우 geometry 제외하고 데이터만 추출
    if (inherits(data, "sf")) {
      return(as.numeric(sf::st_drop_geometry(data)[[col]]))
    }
    return(as.numeric(data[[col]]))
  } else {
    stop("입력값이 올바르지 않거나 데이터에 해당 열이 없습니다.")
  }
}

# 벡터 길이 일치 확인 및 N 반환
.rsg_validate_lengths <- function(...) {
  vecs <- list(...)
  lens <- sapply(vecs, length)
  if (length(unique(lens)) != 1) {
    stop("입력된 인구 데이터 벡터들의 길이가 일치하지 않습니다.")
  }
  return(lens[1])
}

# 구역 이름 자동 생성
.rsg_zone_names <- function(data, n, zone_names = NULL) {
  if (!is.null(zone_names)) return(zone_names)
  return(paste0("Zone_", seq_len(n)))
}

# OD 이동량 행렬 검증 및 행렬 변환 (크기·음수 확인 후 matrix 반환)
.rsg_validate_od <- function(od_matrix, n) {
  phi <- as.matrix(od_matrix)
  if (nrow(phi) != n || ncol(phi) != n)
    stop(sprintf("od_matrix는 %d×%d 행렬이어야 합니다. 현재: %d×%d",
                 n, n, nrow(phi), ncol(phi)))
  if (any(phi < 0, na.rm = TRUE))
    stop("od_matrix에 음수값이 있습니다.")
  phi
}