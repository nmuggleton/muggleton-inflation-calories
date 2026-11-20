# Sizes and export settings for a two-column journal: widths of 8.7, 11.4 or
# 17.8 cm and a maximum height of 22.5 cm; type at least 6 pt at publication
# size, in one family (Arial/Helvetica), with fonts embedded; lines at least
# 0.25 pt; vector art preferred, and raster files in RGB at 600-900 ppi when
# they contain type, with LZW rather than JPEG compression.
#
# Usage:
#   source("src/lib/theme_print.R")
#   fig_geom_defaults()                 # optional: thinner default marks
#   p <- ggplot(df, aes(x, y)) + geom_point() + theme_print()
#   save_print(p, "fig1.pdf", width = "single")

library(ggplot2)

# Units ------------------------------------------------------------------------
#' Convert a stroke width in points to a ggplot2 linewidth.
#'
#' ggplot2 linewidth is drawn as linewidth * .pt R line units, and one R line
#' unit is 1/96 inch (0.75 pt). This returns the linewidth that prints at
#' exactly `pt` points.
#'
#' @param pt Stroke width in points.
#' @return Numeric linewidth for ggplot2.
pt2lw <- function(pt) pt / (ggplot2::.pt * 72.27 / 96)

#' Column widths in millimetres.
fig_width <- c(single = 87, onehalf = 114, double = 178)

#' Maximum printed figure height in millimetres.
fig_max_height <- 225

#' Minimum printed type size and stroke width.
fig_min_type <- 6
fig_min_stroke <- 0.25

#' ggplot2 text size (mm) that prints at `pt` points, for geom_text().
pt2size <- function(pt) pt / ggplot2::.pt

# Palettes ---------------------------------------------------------------------
#' Named palettes.
#'
#' two      : the corpus default contrast (navy against a warm red), chosen so
#'            the pair stays distinct under deuteranopia and in greyscale.
#' pre_post : event studies; grey before treatment, navy after.
#' okabe    : Okabe-Ito, for three to seven unordered groups.
#' paired   : two factors crossed (dark/light = factor 1, hue = factor 2).
pal <- list(
    two = c('#1F4E79', '#C8553D'),
    pre_post = c(pre = '#8C8C8C', post = '#1F4E79'),
    okabe = c(
        '#0072B2', '#D55E00', '#009E73', '#CC79A7',
        '#E69F00', '#56B4E9', '#000000'
    ),
    paired = c('#1F4E79', '#8FB3D9', '#C8553D', '#EDB3A5')
)


# Theme ------------------------------------------------------------------------
#' ggplot2 theme sized for print.
#'
#' Open L-shaped axes, no panel border, no background fill, optional light
#' reference grid, 7 pt type with 6 pt tick labels, facet labels as plain
#' left-aligned text, and bold panel tags at the top left.
#'
#' @param base_size Body text size in points (7 recommended; never below 6).
#' @param base_family Font family ("Helvetica" on macOS, "Arial" on Windows).
#' @param grid Which major grid lines to draw: "none", "y", "x", or "xy".
#' @param legend Legend position: "bottom", "top", "right", "none", or
#'   a numeric c(x, y) for placement inside the panel.
#' @return A ggplot2 theme object.
theme_print <- function(
    base_size = 7,
    base_family = 'Helvetica',
    grid = c('none', 'y', 'x', 'xy'),
    legend = 'bottom'
) {
    grid <- match.arg(grid)
    ink <- '#1A1A1A'
    soft <- '#4D4D4D'
    stroke <- pt2lw(0.5)
    grid_line <- ggplot2::element_line(
        colour = '#E3E3E3',
        linewidth = pt2lw(0.4)
    )

    inside <- is.numeric(legend)
    th <- ggplot2::theme_classic(
        base_size = base_size,
        base_family = base_family
    ) +
        ggplot2::theme(
            text = ggplot2::element_text(colour = ink),
            line = ggplot2::element_line(colour = ink, linewidth = stroke),
            axis.line = ggplot2::element_line(colour = ink, linewidth = stroke),
            axis.ticks = ggplot2::element_line(
                colour = ink,
                linewidth = stroke
            ),
            axis.ticks.length = grid::unit(1, 'mm'),
            axis.text = ggplot2::element_text(
                size = base_size - 1,
                colour = soft
            ),
            axis.text.x = ggplot2::element_text(
                margin = ggplot2::margin(t = 1)
            ),
            axis.text.y = ggplot2::element_text(
                margin = ggplot2::margin(r = 1)
            ),
            axis.title = ggplot2::element_text(size = base_size),
            axis.title.x = ggplot2::element_text(
                margin = ggplot2::margin(t = 3)
            ),
            axis.title.y = ggplot2::element_text(
                margin = ggplot2::margin(r = 3)
            ),
            panel.background = ggplot2::element_blank(),
            panel.grid.major.x = if (grid %in% c('x', 'xy')) {
                grid_line
            } else {
                ggplot2::element_blank()
            },
            panel.grid.major.y = if (grid %in% c('y', 'xy')) {
                grid_line
            } else {
                ggplot2::element_blank()
            },
            panel.grid.minor = ggplot2::element_blank(),
            panel.spacing = grid::unit(3, 'mm'),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(
                size = base_size,
                hjust = 0,
                margin = ggplot2::margin(b = 2)
            ),
            strip.placement = 'outside',
            legend.background = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            legend.key.size = grid::unit(3, 'mm'),
            legend.title = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size = base_size - 0.5),
            legend.margin = ggplot2::margin(0, 0, 0, 0),
            legend.box.spacing = grid::unit(1.5, 'mm'),
            legend.spacing.x = grid::unit(1, 'mm'),
            plot.title = ggplot2::element_text(
                size = base_size,
                face = 'bold',
                hjust = 0
            ),
            plot.title.position = 'plot',
            plot.caption = ggplot2::element_text(
                size = base_size - 1,
                colour = soft,
                hjust = 0
            ),
            plot.tag = ggplot2::element_text(
                size = base_size + 1,
                face = 'bold'
            ),
            plot.tag.position = c(0, 1),
            plot.margin = ggplot2::margin(2, 2, 2, 2, 'mm'),
            plot.background = ggplot2::element_blank()
        )

    # Legend placement: ggplot2 3.5 and later distinguish inside placement from
    # the rest.
    new_api <- utils::packageVersion('ggplot2') >= '3.5.0'
    if (!inside) {
        th <- th + ggplot2::theme(legend.position = legend)
    } else if (new_api) {
        th <- th + ggplot2::theme(
            legend.position = 'inside',
            legend.position.inside = legend
        )
    } else {
        th <- th + ggplot2::theme(legend.position = legend)
    }
    th
}

# Geom defaults and helpers ----------------------------------------------------
# Export -----------------------------------------------------------------------
#' Save a figure at print size.
#'
#' PDF output uses cairo_pdf so fonts embed (journals prefer vector art).
#' Raster output is RGB at 600 ppi (600-900 ppi for figures with
#' type); TIFF uses LZW compression, never JPEG. ragg is used when
#' installed, otherwise the cairo devices. Height defaults to width / 1.4,
#' the median aspect ratio of the audited corpus.
#'
#' @param plot A ggplot or patchwork object.
#' @param file Output path; the extension selects the device.
#' @param width "single", "onehalf", "double", or a width in mm.
#' @param height Height in mm (NULL = width / aspect).
#' @param aspect Width-to-height ratio used when height is NULL.
#' @param dpi Resolution for raster output.
#' @param trim PDF only: draw the figure to the edge of its width. The
#'   white margin ggplot leaves round the drawing (2-4 mm a side in the
#'   paper's figures) is measured with ghostscript, the figure is redrawn
#'   on a canvas larger by that margin, and the PDF is cropped (pdfcrop) to
#'   `pad` mm round the drawing, so the page keeps the column width and
#'   the type keeps its size.
#' @param pad White edge kept round the drawing when trimming, in mm.
#' @return The file path, invisibly.
save_print <- function(
    plot, file, width = 'single', height = NULL,
    aspect = 1.4, dpi = 600, trim = TRUE, pad = 0.5
) {
    w <- if (is.character(width)) fig_width[[width]] else width
    h <- if (is.null(height)) w / aspect else height
    if (!w %in% fig_width) {
        warning(
            'Width ',
            w,
            ' mm is not a column width (',
            paste(fig_width, collapse = ', '),
            ' mm).'
        )
    }
    if (h > fig_max_height) {
        warning('Height exceeds fig_max_height (', fig_max_height, ' mm).')
    }
    if (dpi < 600) warning('Use 600-900 ppi for figures with type.')
    ext <- tolower(tools::file_ext(file))
    has_ragg <- requireNamespace('ragg', quietly = TRUE)
    if (ext == 'pdf') {
        ggplot2::ggsave(
            file,
            plot,
            width = w,
            height = h,
            units = 'mm',
            device = grDevices::cairo_pdf,
            bg = 'white'
        )
        if (trim) {
            gs <- Sys.which('gs')
            crop <- Sys.which('pdfcrop')
            if (!nzchar(crop) && file.exists('/Library/TeX/texbin/pdfcrop')) {
                crop <- '/Library/TeX/texbin/pdfcrop'
            }
            if (!nzchar(gs) || !nzchar(crop)) {
                stop(
                    'save_print(trim = TRUE) needs ghostscript (gs) and pdfcrop.'
                )
            }
            # The drawing's bounding box, in points, with the bottom-left corner
            # as the origin
            bb <- function(f) {
                out <- system2(
                    gs,
                    c(
                        '-q', '-dNOPAUSE', '-dBATCH', '-sDEVICE=bbox',
                        shQuote(f)
                    ),
                    stdout = TRUE,
                    stderr = TRUE
                )
                x <- grep('^%%HiResBoundingBox', out, value = TRUE)
                if (length(x) != 1) stop('save_print: no bounding box for ', f)
                as.numeric(strsplit(x, ' ')[[1]][2:5])
            }
            mm <- 25.4 / 72
            b <- bb(file) * mm # left, bottom, right, top, in mm
            margin_lr <- b[1] + (w - b[3])
            margin_bt <- b[2] + (h - b[4])
            ggplot2::ggsave(
                file,
                plot,
                width = w + margin_lr - 2 * pad,
                height = h + margin_bt - 2 * pad,
                units = 'mm',
                device = grDevices::cairo_pdf,
                bg = 'white'
            )
            tmp <- tempfile(fileext = '.pdf')
            res <- system2(
                crop,
                c(
                    '--hires', '--margins', pad / mm, shQuote(file),
                    shQuote(tmp)
                ),
                stdout = FALSE,
                stderr = FALSE
            )
            if (res != 0 || !file.exists(tmp)) {
                stop(
                    'save_print: pdfcrop failed on ',
                    file
                )
            }
            file.copy(tmp, file, overwrite = TRUE)
            # The cropped page is the column width to within the change in
            # ggplot's own margins on the larger canvas (in practice less than
            # 0.2 mm)
            b <- bb(file) * mm
            page_w <- b[3] - b[1] + 2 * pad
            if (abs(page_w - w) > 0.5) {
                stop(
                    'save_print: trimmed width ',
                    round(page_w, 1),
                    ' mm, not ',
                    w,
                    ' mm'
                )
            }
        }
    } else if (ext %in% c('tif', 'tiff')) {
        if (has_ragg) {
            ggplot2::ggsave(
                file,
                plot,
                width = w,
                height = h,
                units = 'mm',
                dpi = dpi,
                device = ragg::agg_tiff,
                bg = 'white',
                compression = 'lzw'
            )
        } else {
            ggplot2::ggsave(
                file,
                plot,
                width = w,
                height = h,
                units = 'mm',
                dpi = dpi,
                device = 'tiff',
                type = 'cairo',
                compression = 'lzw',
                bg = 'white'
            )
        }
    } else if (ext == 'png') {
        dev <- if (has_ragg) ragg::agg_png else 'png'
        args <- list(
            file,
            plot,
            width = w,
            height = h,
            units = 'mm',
            dpi = dpi,
            device = dev,
            bg = 'white'
        )
        if (!has_ragg) args$type <- 'cairo'
        do.call(ggplot2::ggsave, args)
    } else {
        stop(
            'Use .pdf (preferred), .tif, or .png; not JPEG ',
            'compression.'
        )
    }
    invisible(file)
}
