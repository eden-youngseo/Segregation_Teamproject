library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(RColorBrewer)

FONT <- "Apple SD Gothic Neo"
setwd("C:/Users/NA/Desktop/RSG_final/analysis_2")

# ── 1. 시설 데이터 로드 ────────────────────────────────────────────────────────
path_fac <- "노인여가+복지시설_동별_20260527210745.xlsx"


raw_fac <- read_excel(path_fac, sheet = "데이터",
                      col_names = FALSE, .name_repair = "minimal", skip = 3)
names(raw_fac) <- c("drop","구","동","시설합계","복지관수","복지관종사자","경로당수","노인교실수")

raw_fac2 <- raw_fac %>%
  select(-drop, -복지관종사자) %>%
  mutate(구 = ifelse(구 == "소계", NA, 구)) %>%
  tidyr::fill(구, .direction = "down") %>%
  filter(!is.na(구))

# 구 소계 (동 단위 없는 4개 구 → 구 단위 요약점에 사용)
subtotals <- raw_fac2 %>%
  filter(동 == "소계") %>%
  transmute(구,
            구_시설합계 = as.numeric(시설합계),
            구_경로당   = as.numeric(경로당수),
            구_노인교실 = as.numeric(노인교실수))

# 동 단위 행
dong_fac <- raw_fac2 %>%
  filter(동 != "소계") %>%
  mutate(동 = gsub("·", ".", 동)) %>%
  mutate(across(시설합계:노인교실수,
                ~ as.numeric(ifelse(. %in% c("-","..."), NA_character_, .))))

# 동 단위 데이터 없는 구 확인
dot_구 <- dong_fac %>% filter(is.na(시설합계)) %>% pull(구) %>% unique()
cat("동 단위 데이터 없는 구 (구 단위로만 표시):", paste(dot_구, collapse=", "), "\n")

# 동 단위 실측값만 사용
fac_dong_valid <- dong_fac %>%
  filter(!is.na(시설합계)) %>%
  select(구, 동, 시설합계, 경로당수, 노인교실수)

# ── 2. 분리 측도 결과 로드 ─────────────────────────────────────────────────────
results <- readRDS("results.rds")
data_sf  <- results$data_sf %>% st_set_crs(5179) %>% filter(!is.na(저소득))

seg_df <- data_sf %>%
  st_drop_geometry() %>%
  mutate(Local_Isolation     = results$I_local$index,
         Local_Dissimilarity = results$D_elderly_local$index) %>%
  select(구, 동, 저소득, 일반인구_비교집단, 독거_비교집단, 독거_전체, 총인구,
         Local_Isolation, Local_Dissimilarity)

# ── 3. 동 단위 조인 (353개 동만) ──────────────────────────────────────────────
joined <- seg_df %>%
  filter(!(구 %in% dot_구)) %>%
  left_join(fac_dong_valid, by = c("구", "동")) %>%
  mutate(시설_per1000 = ifelse(저소득 > 0, 시설합계 / 저소득 * 1000, NA_real_))

n_miss <- sum(is.na(joined$시설합계))
cat("동 단위 조인 후 결측:", n_miss, "개\n")
if (n_miss > 0) print(joined %>% filter(is.na(시설합계)) %>% select(구, 동))

cat(sprintf("\n분석 대상: %d개 동 (동 단위 실측)\n", nrow(joined)))
cat(sprintf("제외: %s (%d개 동, 구 단위로 별도 표시)\n",
            paste(dot_구, collapse="·"), nrow(seg_df) - nrow(joined)))

cat("\n--- 시설_per1000 기술통계 (동 단위 실측 기준) ---\n")
print(summary(joined$시설_per1000))

# ── 4. 구 단위 요약점 (4개 구 → 산점도에 별도 마커로 표시) ───────────────────
구_summary <- seg_df %>%
  filter(구 %in% dot_구) %>%
  group_by(구) %>%
  summarise(
    구_저소득합         = sum(저소득),
    iso_avg             = weighted.mean(Local_Isolation,     w = 저소득 + 1),
    diss_avg            = weighted.mean(Local_Dissimilarity, w = 저소득 + 1),
    .groups = "drop"
  ) %>%
  left_join(subtotals, by = "구") %>%
  mutate(시설_per1000 = 구_시설합계 / 구_저소득합 * 1000)

cat("\n[구 단위 요약 (동 단위 데이터 없는 4개 구)]\n")
print(구_summary %>% select(구, 구_저소득합, 구_시설합계, 시설_per1000))

# ── 5. 사분면 분류 (353개 동만) ───────────────────────────────────────────────
iso_med  <- median(joined$Local_Isolation,     na.rm = TRUE)
diss_med <- median(joined$Local_Dissimilarity, na.rm = TRUE)
fac_med  <- median(joined$시설_per1000,         na.rm = TRUE)

cat(sprintf("\n중앙값 — 고립지수: %.5f | 상이지수: %.4f | 시설밀도: %.1f (개/1천명)\n",
            iso_med, diss_med, fac_med))

joined <- joined %>%
  mutate(
    iso_quad = case_when(
      Local_Isolation > iso_med  & 시설_per1000 < fac_med ~ "① 고위험\n(고고립·저시설)",
      Local_Isolation > iso_med  & 시설_per1000 >= fac_med ~ "② 시설 충분",
      Local_Isolation <= iso_med & 시설_per1000 < fac_med ~ "③ 저고립·저시설",
      Local_Isolation <= iso_med & 시설_per1000 >= fac_med ~ "④ 양호",
      TRUE ~ NA_character_
    ),
    diss_quad = case_when(
      Local_Dissimilarity > diss_med & 시설_per1000 < fac_med ~ "① 고위험\n(고분리·저시설)",
      Local_Dissimilarity > diss_med & 시설_per1000 >= fac_med ~ "② 시설 충분",
      Local_Dissimilarity <= diss_med & 시설_per1000 < fac_med ~ "③ 저분리·저시설",
      Local_Dissimilarity <= diss_med & 시설_per1000 >= fac_med ~ "④ 양호",
      TRUE ~ NA_character_
    )
  )

cat("\n[고립 × 시설 사분면 (동 단위 실측 353개 동)]\n")
print(table(joined$iso_quad, useNA="ifany"))
cat("\n[분리 × 시설 사분면 (동 단위 실측 353개 동)]\n")
print(table(joined$diss_quad, useNA="ifany"))

# ── 6. 고위험 행정동 목록 ─────────────────────────────────────────────────────
priority_iso <- joined %>%
  filter(grepl("고위험", iso_quad)) %>%
  arrange(desc(Local_Isolation)) %>%
  mutate(구동 = paste0(구, " ", 동)) %>%
  select(구동, 저소득, Local_Isolation, 시설합계, 시설_per1000) %>%
  head(15)

priority_diss <- joined %>%
  filter(grepl("고위험", diss_quad)) %>%
  arrange(desc(Local_Dissimilarity)) %>%
  mutate(구동 = paste0(구, " ", 동)) %>%
  select(구동, 저소득, Local_Dissimilarity, 시설합계, 시설_per1000) %>%
  head(15)

cat("\n[고위험 — 고고립 + 저시설 상위 15 (실측 동만)]\n"); print(priority_iso)
cat("\n[고위험 — 고분리 + 저시설 상위 15 (실측 동만)]\n"); print(priority_diss)

# ── 7. 시각화 ────────────────────────────────────────────────────────────────
quad_pal <- c(
  "① 고위험\n(고고립·저시설)" = "#D73027",
  "② 시설 충분"               = "#4575B4",
  "③ 저고립·저시설"           = "#FDAE61",
  "④ 양호"                    = "#ABD9E9"
)
quad_pal_d <- c(
  "① 고위험\n(고분리·저시설)" = "#D73027",
  "② 시설 충분"               = "#4575B4",
  "③ 저분리·저시설"           = "#FDAE61",
  "④ 양호"                    = "#ABD9E9"
)

base_theme <- function() {
  theme_minimal(base_size = 11, base_family = FONT) +
  theme(plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_text(color = "grey40", size = 8.5),
        legend.position  = "bottom",
        legend.title     = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = "white", color = NA),
        plot.margin      = margin(10, 12, 10, 10))
}

y_max <- quantile(joined$시설_per1000, 0.97, na.rm = TRUE)

label_iso  <- joined %>% filter(grepl("고위험", iso_quad)) %>%
  arrange(desc(Local_Isolation)) %>% head(8) %>%
  mutate(label = paste0(구, "\n", 동))
label_diss <- joined %>% filter(grepl("고위험", diss_quad)) %>%
  arrange(desc(Local_Dissimilarity)) %>% head(8) %>%
  mutate(label = paste0(구, "\n", 동))

# Plot A: 고립 × 시설
p_iso_fac <- ggplot(joined %>% filter(!is.na(iso_quad)),
                    aes(x = Local_Isolation, y = 시설_per1000, color = iso_quad)) +
  geom_vline(xintercept = iso_med,  linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_hline(yintercept = fac_med,  linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_point(alpha = 0.7, size = 1.8, shape = 16) +
  # 구 단위 요약점 (다이아몬드, 회색)
  geom_point(data = 구_summary,
             aes(x = iso_avg, y = 시설_per1000),
             color = "grey30", shape = 18, size = 4, inherit.aes = FALSE) +
  geom_text(data = 구_summary,
            aes(x = iso_avg, y = 시설_per1000, label = 구),
            color = "grey30", size = 2.5, hjust = -0.15, family = FONT,
            inherit.aes = FALSE) +
  geom_text(data = label_iso, aes(label = label),
            size = 2.4, color = "#D73027", hjust = -0.1, lineheight = 0.85, family = FONT) +
  scale_color_manual(values = quad_pal) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(title    = "1단계: 사회적 고립 × 노인여가복지시설 밀도",
       subtitle = "동 단위 실측 353개 동 | ◆=구 단위 평균 (중랑·노원·서초·강동구, 동 단위 데이터 없음)",
       x = "Local Isolation Index",
       y = "시설합계 (개/저소득 독거노인 1,000명)") +
  base_theme()

ggsave("scatter_isolation_facility.png", p_iso_fac, width = 9, height = 7, dpi = 200)
cat("scatter_isolation_facility.png 저장\n")

# Plot B: 분리 × 시설
p_diss_fac <- ggplot(joined %>% filter(!is.na(diss_quad)),
                     aes(x = Local_Dissimilarity, y = 시설_per1000, color = diss_quad)) +
  geom_vline(xintercept = diss_med, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_hline(yintercept = fac_med,  linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_point(alpha = 0.7, size = 1.8, shape = 16) +
  geom_point(data = 구_summary,
             aes(x = diss_avg, y = 시설_per1000),
             color = "grey30", shape = 18, size = 4, inherit.aes = FALSE) +
  geom_text(data = 구_summary,
            aes(x = diss_avg, y = 시설_per1000, label = 구),
            color = "grey30", size = 2.5, hjust = -0.15, family = FONT,
            inherit.aes = FALSE) +
  geom_text(data = label_diss, aes(label = label),
            size = 2.4, color = "#D73027", hjust = -0.1, lineheight = 0.85, family = FONT) +
  scale_color_manual(values = quad_pal_d) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(title    = "2단계: 저소득 독거노인 vs 비교 독거노인 분리 × 노인여가복지시설 밀도",
       subtitle = "동 단위 실측 353개 동 | ◆=구 단위 평균 (중랑·노원·서초·강동구, 동 단위 데이터 없음)",
       x = "Local Dissimilarity Index",
       y = "시설합계 (개/저소득 독거노인 1,000명)") +
  base_theme()

ggsave("scatter_dissimilarity_facility.png", p_diss_fac, width = 9, height = 7, dpi = 200)
cat("scatter_dissimilarity_facility.png 저장\n")

# Plot C: 고위험 행정동 바 차트 (동 단위 실측만)
top_risk <- joined %>%
  filter(grepl("고위험", iso_quad)) %>%
  arrange(desc(Local_Isolation)) %>% head(15) %>%
  mutate(구동 = factor(paste0(구, " ", 동), levels = rev(paste0(구, " ", 동))))

p_bar <- ggplot(top_risk, aes(x = 구동, y = 저소득)) +
  geom_col(fill = "#D73027", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = paste0("시설 ", round(시설합계), "개 (",
                                round(시설_per1000, 1), "/천명)")),
            hjust = -0.05, size = 2.8, family = FONT, color = "grey30") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(title    = "고위험 행정동 (고고립 + 저시설) 상위 15",
       subtitle = "동 단위 실측 353개 동 기준 | 막대: 저소득 독거노인 수 | 라벨: 시설 수·밀도",
       x = NULL, y = "저소득 독거노인 수 (명)") +
  theme_minimal(base_size = 11, base_family = FONT) +
  theme(plot.title        = element_text(face = "bold"),
        plot.subtitle     = element_text(color = "grey40", size = 8.5),
        panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background   = element_rect(fill = "white", color = NA))

ggsave("bar_high_risk_districts.png", p_bar, width = 9, height = 7, dpi = 200)
cat("bar_high_risk_districts.png 저장\n")

# ── 8. 결과 저장 ──────────────────────────────────────────────────────────────
saveRDS(list(joined = joined, 구_summary = 구_summary), "joined_facility.rds")
cat("\njoined_facility.rds 저장 완료\n")
cat("\n=== 분석 범위 ===\n")
cat("동 단위 실측:", nrow(joined), "개 동 (사분면 분류 적용)\n")
cat("구 단위 표시:", paste(dot_구, collapse="·"), "→ 산점도에 ◆ 마커로 별도 표시\n")
