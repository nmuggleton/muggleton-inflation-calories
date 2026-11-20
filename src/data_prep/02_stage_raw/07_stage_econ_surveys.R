# Stage the PanelViews economic surveys (Chicago Booth Expectations and
# Attitudes Survey 2015-16, Chicago Booth Expectations and Characteristics
# Survey 2018-19): nine waves, one workbook each, with question numbers that
# change by wave.

rm(list = ls())

library(data.table)
library(readxl)
library(arrow)

args <- commandArgs(trailingOnly = TRUE)
dat <- args[1]
raw <- Sys.getenv(
    'ECON_SURVEY_RAW',
    file.path(Sys.getenv('RMS_RAW'), 'PanelViewSurvey-Economic')
)
stopifnot(dir.exists(dat), dir.exists(raw))
out <- file.path(dat, 'intermediate', 'panelviews', 'economic')
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Per wave: the workbook, the reference quarter (the last to end before the
# survey was fielded), whether the sample is split by wording, the question
# holding the stated change (four variables for the split waves: rise and fall
# for own prices and for inflation), the stem of the ten probability bins,
# whether the bins run from deflation to inflation (2019 only), and the column
# that orders respondents.
waves <- list(
    list(
        wave = '2015', file = '2015 CBEAS/CBEAS Wave 1 Survey Data.xlsx',
        yq = 20152L, split = TRUE,
        q = c('(Q25_1)', '(Q44_1)', '(Q51_1)', '(Q52_1)'),
        bins = c('Q49', 'Q57'), rev = FALSE, ord = NA
    ),
    list(
        wave = '2016', file = '2016 CBEAS/CBEAS Wave 2 Survey Data.xlsx',
        yq = 20162L, split = TRUE,
        q = c('(Q25_1)', '(Q42_1)', '(Q51_1)', '(Q52_1)'),
        bins = c('Q49', 'Q57'), rev = FALSE, ord = NA
    ),
    list(
        wave = '2018w1',
        file = '2018 CBECS Wave 1/2018 CBECS Wave 1 Survey Data.xlsx',
        yq = 20181L, split = FALSE, q = '(Q75_1)', bins = 'Q77_A',
        rev = FALSE, ord = NA
    ),
    list(
        wave = '2018w1a',
        file = '2018 CBECS Wave 1a/2018 CBECS Wave 1a Survey Data.xlsx',
        yq = 20181L, split = FALSE, q = '(Q75_1)', bins = 'Q77_A',
        rev = FALSE, ord = NA
    ),
    list(
        wave = '2018w2',
        file = '2018 CBECS Wave 2/2018 CBECS Wave 2 Survey Data.xlsx',
        yq = 20183L, split = FALSE, q = '(Q67_1)', bins = 'Q70_A',
        rev = FALSE, ord = 'response sequence'
    ),
    list(
        wave = '2018w3',
        file = '2018 CBECS Wave 3/2018 CBECS Wave 3 Survey Data.xlsx',
        yq = 20184L, split = FALSE, q = '(Q93_1)', bins = 'Q82_A',
        rev = FALSE, ord = 'response sequence'
    ),
    list(
        wave = '2019w1',
        file = '2019 CBECS Wave 1/2019 CBECS Wave 1 Survey Data.xlsx',
        yq = 20191L, split = FALSE, q = '(Q42_1)', bins = 'Q82_A',
        rev = TRUE, ord = 'response sequence'
    ),
    list(
        wave = '2019w2',
        file = '2019 CBECS Wave 2/2019 CBECS Wave 2 Survey Data.xlsx',
        yq = 20192L, split = FALSE, q = '(Q46_1)', bins = 'Q47_A',
        rev = TRUE, ord = 'response sequence'
    ),
    list(
        wave = '2019w3',
        file = '2019 CBECS Wave 3/2019 CBECS Wave 3 Survey Data.xlsx',
        yq = 20193L, split = FALSE, q = '(Q38_1)', bins = 'Q39_A',
        rev = TRUE, ord = 'response sequence'
    )
)

num <- function(x) suppressWarnings(as.numeric(x))

# The one column whose header contains `pat`
col <- function(s, pat) {
    j <- grep(pat, names(s), fixed = TRUE)
    if (length(j) != 1) stop('pattern ', pat, ' matches ', length(j))
    s[[j]]
}

# A change asked as two questions (by how much up, by how much down)
signed <- function(s, up, down) {
    u <- num(col(s, up))
    v <- num(col(s, down))
    fifelse(!is.na(u), u, fifelse(!is.na(v), -v, NA_real_))
}

# Probability bins for the next 12 months: item 1 is inflation of 12 percent or
# more and item 10 deflation of 12 percent or more (the order is reversed in
# 2019).
mid <- c(14, 10, 6, 3, 1, -1, -3, -6, -10, -14)
bins <- function(s, stem, rev) {
    m <- sapply(1:10, function(k) num(col(s, sprintf('(%s_%d)', stem, k))))
    if (rev) m <- m[, 10:1, drop = FALSE]
    tot <- rowSums(m)
    ok <- !is.na(tot) & abs(tot - 100) <= 1
    p <- m / tot
    mu <- as.numeric(p %*% mid)
    sdv <- sqrt(pmax(as.numeric(p %*% mid^2) - mu^2, 0))
    data.table(
        subj_sd = fifelse(ok, sdv, NA_real_),
        n_bins = fifelse(ok, as.numeric(rowSums(m > 0)), NA_real_)
    )
}

long <- rbindlist(
    lapply(
        waves,
        function(w) {
            s <- as.data.table(
                suppressMessages(
                    suppressWarnings(
                        read_excel(file.path(raw, w$file), guess_max = 20000)
                    )
                )
            )
            cat(w$wave, ':', nrow(s), 'rows x', ncol(s), 'columns\n')
            if (w$split) {
                own <- signed(s, w$q[1], w$q[2])
                gen <- signed(s, w$q[3], w$q[4])
                is_own <- !is.na(own)
                b_own <- bins(s, w$bins[1], w$rev)
                b_gen <- bins(s, w$bins[2], w$rev)
                # Main shopper first, then shared, then others
                shop <- num(col(s, '(Q43)'))
                shop[is.na(shop)] <- 9
                o <- data.table(
                    household_code = num(s[['CORRECTED HHID']]),
                    wording = fifelse(
                        is_own,
                        'own prices',
                        fifelse(!is.na(gen), 'inflation', NA_character_)
                    ),
                    perceived = fifelse(is_own, own, gen),
                    subj_sd = fifelse(is_own, b_own$subj_sd, b_gen$subj_sd),
                    n_bins = fifelse(is_own, b_own$n_bins, b_gen$n_bins),
                    shop = shop,
                    ord = seq_len(nrow(s))
                )
            } else {
                b <- bins(s, w$bins, w$rev)
                o <- data.table(
                    household_code = num(s[['CORRECTED HHID']]),
                    wording = 'inflation',
                    perceived = num(col(s, w$q)),
                    subj_sd = b$subj_sd,
                    n_bins = b$n_bins,
                    shop = 0,
                    ord = if (is.na(w$ord)) {
                        as.numeric(seq_len(nrow(s)))
                    } else {
                        num(s[[grep(w$ord, names(s))[1]]])
                    }
                )
            }
            setorder(o, household_code, shop, ord)
            o <- o[!is.na(perceived)][, .SD[1], by = household_code]
            o[, `:=`(wave = w$wave, yq = w$yq, shop = NULL, ord = NULL)]
            o[]
        }
    )
)
long[, household_code := as.integer(household_code)]
stopifnot(
    !anyNA(long$household_code),
    !anyDuplicated(long[, .(household_code, wave)])
)

cat('\nrespondent households by wave and wording:\n')
print(
    long[
        ,
        .(n = .N, with_bins = sum(!is.na(subj_sd))),
        keyby = .(wave, wording, yq)
    ]
)
cat(
    'distinct households:', uniqueN(long$household_code),
    '| in 2 or more waves:',
    long[, .N, by = household_code][N >= 2, .N], '\n'
)

dst <- file.path(out, 'beliefs_long.parquet')
write_parquet(long, dst)
cat('wrote', dst, '\n')
