# Supplementary exercises on diet and shopping.

rm(list = ls())

library(data.table)
library(fixest)

source('src/lib/helpers.R')

options(width = 200, scipen = 999)
hq <- sprintf(
    paste0(
        "read_parquet('%s/analysis_hh_quarter/*/*.parquet', ",
        'hive_partitioning = true)'
    ),
    wp2_paths()$final
)

iv <- function(dd, ys, lab = NULL) {
    m <- feols(
        as.formula(
            sprintf(
                'c(%s) ~ 1 | yq | infl_exposure ~ z',
                paste(ys, collapse = ', ')
            )
        ),
        dd,
        cluster = ~state,
        notes = FALSE
    )
    if (length(ys) == 1) m <- list(m)
    rbindlist(
        lapply(
            seq_along(ys),
            function(j) {
                ct <- coeftable(m[[j]])
                data.table(
                    group = lab,
                    outcome = ys[j],
                    b = ct[1, 1],
                    se = ct[1, 2],
                    p = 2 * pt(abs(ct[1, 3]), 48, lower.tail = FALSE),
                    n = nobs(m[[j]])
                )
            }
        )
    )
}


# 0. Changes in components and shopping margins (t vs t-4) ---------------------

vars <- c(
    'z_fiber', 'z_protein', 'z_fv', 'z_sugar', 'z_satfat', 'z_sodium',
    'sh_deal', 'sh_pl', 'sh_ch_grocery', 'sh_ch_discount', 'sh_ch_club',
    'sh_ch_dollar', 'n_trips_food'
)
sel <- paste(vars, collapse = ', ')
ch <- wp2_sql(
    sprintf(
        "WITH h AS (SELECT household_code, yq, %s FROM %s)
   SELECT a.household_code, a.yq, %s
   FROM h a
   JOIN h b
       ON a.household_code = b.household_code AND b.yq = a.yq - 10",
        sel,
        hq,
        paste(sprintf('a.%1$s - b.%1$s AS d_%1$s', vars), collapse = ', ')
    )
)
d <- f1_gate(
    c(
        'd_lkpu',
        'd_lspend',
        'd_lkcal',
        'd_ed',
        'd_hs',
        'kcal_100g_food_b'
    )
)
d <- merge(d, ch, by = c('household_code', 'yq'), all.x = TRUE)
rm(ch)
gc()
cat(
    'gate rows:',
    nrow(d),
    '| with component changes:',
    d[!is.na(d_z_fiber), .N],
    '\n'
)
d[, d_n_trips_food := as.numeric(d_n_trips_food)]

# (1) Components: changes in item z-scores by nutrient (higher values mean
# relatively more of that nutrient)
comp <- c(
    d_z_fiber = 'Fiber per 1,000 kcal', d_z_protein = 'Protein per 1,000 kcal',
    d_z_fv = 'Fruit and vegetable spend share',
    d_z_sugar = 'Sugar per 1,000 kcal',
    d_z_satfat = 'Saturated fat per 1,000 kcal',
    d_z_sodium = 'Sodium per 1,000 kcal'
)
rc <- iv(d, names(comp), 'components')
rc[, `:=`(b100 = 100 * b, se100 = 100 * se)]
cat('\n(1) health-score components, 2SLS x100 (item SD units)\n')
print(rc)

# (2) Margins: shares in percentage points (x100), trips in levels
for (v in c(
    'd_sh_deal', 'd_sh_pl', 'd_sh_ch_grocery', 'd_sh_ch_discount',
    'd_sh_ch_club', 'd_sh_ch_dollar'
)) {
    set(d, j = v, value = 100 * d[[v]])
}
marg <- c(
    d_sh_deal = 'Deal (promotion) share of spend, pp',
    d_sh_pl = 'Private-label share of spend, pp',
    d_sh_ch_grocery = 'Grocery-store share, pp',
    d_sh_ch_discount = 'Discount-store share, pp',
    d_sh_ch_club = 'Club-store share, pp',
    d_sh_ch_dollar = 'Dollar-store share, pp',
    d_n_trips_food = 'Food shopping trips per quarter'
)
rm_ <- iv(d, names(marg), 'margins')
cat('\n(2) shopping margins, 2SLS per pp of exposure\n')
print(rm_)


# (3) equivalence: two one-sided tests at stated margins (state clusters) ------

hd <- iv(d, c('d_lkcal', 'd_ed', 'd_hs'), 'headline')
mean_ed <- mean(d$kcal_100g_food_b, na.rm = TRUE)
mg <- c(d_lkcal = 0.25, d_ed = 0.001 * mean_ed, d_hs = 0.01)
hd[, margin := mg[outcome]]
hd[, `:=`(lo90 = b - qt(.95, 48) * se, hi90 = b + qt(.95, 48) * se)]
hd[, p_tost := pmax(
    pt((b + margin) / se, 48, lower.tail = FALSE),
    pt((b - margin) / se, 48)
)]
cat(
    '\n(3) equivalence (TOST); energy-density margin = 0.1% of base mean',
    mean_ed,
    '\n'
)
print(hd, digits = 6)
# Benchmark: estimated calorie elasticities for staple foods run from -0.2 to
# -0.5 (Gibson and Rozelle 2011), so a fall of 0.5 percent per point of a
# uniform basket price rise is at the upper end of that range.
t_meta <- (hd[outcome == 'd_lkcal', b] + 0.5) / hd[outcome == 'd_lkcal', se]
say(
    'test of calorie response = -0.5: t = %.2f, p = %.2e',
    t_meta,
    2 * pt(abs(t_meta), 48, lower.tail = FALSE)
)


# (4) habit: share of base-quarter spend on UPCs also bought at t-8 ------------

hu <- "read_parquet('%s/intermediate/hh_upc_q/panel_year=%d.parquet')"
yrs <- sort(unique(d$year))
hab_f <- file.path(wp2_paths()$data, 'intermediate', 'habit_share.parquet')
if (file.exists(hab_f)) {
    hab <- wp2_sql(sprintf("SELECT * FROM read_parquet('%s')", hab_f))
} else {
    hab <- rbindlist(
        lapply(
            yrs,
            function(y) {
                b <- y - 1L
                p <- y - 2L # base year and the year before it
                if (p < 2004) {
                    return(NULL)
                }
                wp2_sql(
                    sprintf(
                        "WITH b AS (
                            SELECT household_code, yq, upc, sum(spend) s
                            FROM %s
                WHERE yq BETWEEN %d AND %d GROUP BY 1, 2, 3),
          p AS (SELECT DISTINCT household_code, yq, upc FROM %s),
          ph AS (SELECT DISTINCT household_code, yq FROM p)
     SELECT b.household_code, b.yq + 10 AS yq,
            sum(CASE WHEN p.upc IS NOT NULL THEN b.s ELSE 0 END)
                / sum(b.s) AS habit
     FROM b
     JOIN ph
         ON ph.household_code = b.household_code AND ph.yq = b.yq - 10
     LEFT JOIN p ON p.household_code = b.household_code
       AND p.yq = b.yq - 10 AND p.upc = b.upc
     GROUP BY 1, 2 HAVING sum(b.s) > 0",
                        sprintf(
                            hu,
                            wp2_paths()$data,
                            b
                        ),
                        b * 10L + 1L,
                        b * 10L + 4L,
                        sprintf(hu, wp2_paths()$data, p)
                    )
                )
            }
        )
    )
    arrow::write_parquet(hab, hab_f)
}
d <- merge(d, hab, by = c('household_code', 'yq'), all.x = TRUE)
cat(
    '\n(4) habit share available for',
    d[!is.na(habit), .N],
    'gate rows; mean',
    mean(d$habit, na.rm = TRUE),
    '\n'
)
d[!is.na(habit), hab3 := cut(
    frank(habit, ties.method = 'first') / .N,
    c(0, 1 / 3, 2 / 3, 1),
    labels = c('Low', 'Middle', 'High')
)]
print(d[!is.na(hab3), .(median_habit = median(habit), n = .N), keyby = hab3])
rh <- rbindlist(
    lapply(
        c('Low', 'Middle', 'High'),
        function(k) {
            iv(d[hab3 == k], c('d_lkpu', 'd_lspend', 'd_lkcal'), k)
        }
    )
)
print(rh)
# Test that the kcal/$ response is equal in the high- and low-habit groups
dh <- d[hab3 %in% c('Low', 'High')]
dh[, `:=`(hi = as.integer(hab3 == 'High'))]
dh[, `:=`(x_hi = infl_exposure * hi, z_hi = z * hi, hq = paste(hab3, yq))]
mt <- feols(
    c(d_lkpu, d_lkcal) ~ 1 | hq | infl_exposure + x_hi ~ z + z_hi,
    dh,
    cluster = ~state,
    notes = FALSE
)
for (j in 1:2) {
    cat('High - Low habit,', c('kcal/$', 'kcal')[j], ':\n')
    print(coeftable(mt[[j]])['fit_x_hi', ])
}
hm <- d[!is.na(hab3), .(med = median(habit)), keyby = hab3]
habit_diff <- rbindlist(
    lapply(
        1:2,
        function(j) {
            ct <- coeftable(mt[[j]])
            data.table(
                outcome = c('d_lkpu', 'd_lkcal')[j],
                b = ct['fit_x_hi', 1],
                se = ct['fit_x_hi', 2],
                p = ct['fit_x_hi', 4]
            )
        }
    )
)
save_results(
    '19_revision',
    rc = rc,
    rm_ = rm_,
    hd = hd,
    mean_ed = mean_ed,
    t_meta = data.table(
        t = t_meta,
        p = 2 * pt(abs(t_meta), 48, lower.tail = FALSE)
    ),
    rh = rh,
    hm = hm,
    habit_diff = habit_diff
)
print(sessionInfo())
