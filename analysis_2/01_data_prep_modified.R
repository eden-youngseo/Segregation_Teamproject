library(sf)
library(dplyr)

setwd("C:/Users/NA/Desktop/RSG_final/analysis_2")

# ── 경로 설정 ─────────────────────────────────────────
path_elderly <- "독거노인_현황_성별_동별_2024.csv"

path_pop     <- "등록인구_연령별_동별_2024.csv"

path_shp     <- "행정구역.shp"

# ── 1. 독거노인 현황 ────────────────────────────────────────────────────────────
# 헤더 5행 건너뛰고 읽기
# 컬럼: 시도 | 구 | 동 | 전체(계/남/여) | 기초수급(계/남/여) | 차상위(계/남/여) | 일반(계/남/여)
raw_elderly <- read.csv(path_elderly, header = FALSE, skip = 5,
                        fileEncoding = "UTF-8-BOM",
                        col.names = c("시도","구","동",
                                      "전체_계","전체_남","전체_여",
                                      "기초수급_계","기초수급_남","기초수급_여",
                                      "차상위_계","차상위_남","차상위_여",
                                      "일반_계","일반_남","일반_여"))

# 실제 행정동만 남기기 (소계 행 제거)
elderly <- raw_elderly %>%
  filter(동 != "소계", 구 != "소계") %>%
  # 구 이름 정규화
  mutate(구 = ifelse(구 == "동대문", "동대문구", 구)) %>%
  # 동 이름 오타 수정: 정능 → 정릉
  mutate(
    동 = gsub("정능", "정릉", 동),
    동 = gsub("·", ".", 동)
  ) %>%
  
  # "-" → 0 변환 후 numeric
  mutate(across(전체_계:일반_여, ~ as.numeric(ifelse(. == "-", "0", .)))) %>%
  # 강동구 상일1동+상일2동 → 상일동으로 합산
  mutate(동 = ifelse(구 == "강동구" & 동 %in% c("상일1동","상일2동"), "상일동", 동)) %>%
  group_by(구, 동) %>%
  summarise(across(전체_계:일반_여, sum, na.rm = TRUE), .groups = "drop") %>%
  # 분석 집단 설정
  # 1) 저소득 독거노인 = 기초수급 독거노인
  # 2) 독거_비교집단 = 차상위 독거노인 + 일반 독거노인
  # ※ 일반인구_비교집단은 총인구 자료와 조인한 뒤 계산함
  mutate(
    저소득 = 기초수급_계,
    독거_비교집단 = 차상위_계 + 일반_계,
    독거_전체 = 전체_계
  ) %>%
  select(구, 동, 저소득, 독거_비교집단, 독거_전체)

cat("독거노인 행정동 수:", nrow(elderly), "\n")

# ── 2. 등록인구 (전체 주민 — 1단계 고립지수 분모) ──────────────────────────────
pop_raw <- read.csv(
  path_pop,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

pop <- pop_raw %>%
  filter(행정동 != "소계", 시군구 != "합계") %>%
  transmute(
    구 = as.character(시군구),
    동 = as.character(행정동),
    총인구 = as.numeric(gsub(",", "", 합계))
  ) %>%
  mutate(
    동 = gsub("정능", "정릉", 동),
    동 = gsub("·", ".", 동),
    동 = ifelse(구 == "강동구" & 동 %in% c("상일1동", "상일2동"), "상일동", 동)
  ) %>%
  group_by(구, 동) %>%
  summarise(총인구 = sum(총인구, na.rm = TRUE), .groups = "drop")

cat("등록인구 행정동 수:", nrow(pop), "\n")

# ── 3. shapefile 로드 ───────────────────────────────────────────────────────────
shp <- st_read(path_shp, quiet = TRUE) %>%
  rename(구 = SIGUNGU_NM, 동 = ADM_NM) %>%
  select(구, 동, geometry)

# 동 이름 정규화: shapefile의 가운뎃점(·) → 마침표(.)
shp$동 <- gsub("·", ".", shp$동)

cat("shapefile 행정동 수:", nrow(shp), "\n")

# ── 4. 조인 ─────────────────────────────────────────────────────────────────────
# 독거노인 + 등록인구
# 두 가지 비교집단 생성
# 1) 일반인구_비교집단 = 전체 인구 - 저소득 독거노인
# 2) 독거_비교집단 = 차상위 독거노인 + 일반 독거노인
data_joined <- elderly %>%
  left_join(pop, by = c("구", "동")) %>%
  mutate(
    일반인구_비교집단 = 총인구 - 저소득
  )

# 계산 결과 확인: 일반인구_비교집단이 음수이면 원자료 또는 조인 문제 확인 필요
cat("일반인구_비교집단 음수 개수:", sum(data_joined$일반인구_비교집단 < 0, na.rm = TRUE), "\n")

if (sum(data_joined$일반인구_비교집단 < 0, na.rm = TRUE) > 0) {
  cat("일반인구_비교집단 음수 행정동 목록:\n")
  print(data_joined %>%
          filter(일반인구_비교집단 < 0) %>%
          select(구, 동, 총인구, 저소득, 일반인구_비교집단))
}

# shapefile과 조인
data_sf <- shp %>%
  left_join(data_joined, by = c("구", "동"))

# 조인 결과 확인
n_missing <- sum(is.na(data_sf$저소득))
cat("조인 후 결측 행정동 수:", n_missing, "\n")

if (n_missing > 0) {
  cat("결측 행정동 목록:\n")
  print(data_sf[is.na(data_sf$저소득), c("구","동")])
}

cat("\n--- 데이터 요약 ---\n")
cat("서울 전체 저소득 독거노인(기초수급):", sum(data_sf$저소득, na.rm=TRUE), "명\n")
cat("서울 전체 일반인구 비교집단(전체인구-저소득 독거노인):", sum(data_sf$일반인구_비교집단, na.rm=TRUE), "명\n")
cat("서울 전체 독거 비교집단(차상위+일반 독거노인):", sum(data_sf$독거_비교집단, na.rm=TRUE), "명\n")
cat("서울 전체 독거노인:", sum(data_sf$독거_전체, na.rm=TRUE), "명\n")
cat("서울 전체 총인구:", sum(data_sf$총인구, na.rm=TRUE), "명\n")

saveRDS(data_sf, "data_sf.rds")
cat("\ndata_sf.rds 저장 완료\n")
