#include <R.h>
#include <Rinternals.h>
#include <math.h>

/* Sort paired values and weights with a small-array insertion sort. */
static void qc_sort_pairs(double *values, double *weights, int n)
{
    int i, j;
    double value, weight;

    for (i = 1; i < n; ++i) {
        value = values[i];
        weight = weights[i];
        j = i - 1;
        while (j >= 0 && values[j] > value) {
            values[j + 1] = values[j];
            weights[j + 1] = weights[j];
            --j;
        }
        values[j + 1] = value;
        weights[j + 1] = weight;
    }
}

/* Return the ordinary median of an already sorted vector. */
static double qc_sorted_median(const double *values, int n)
{
    if (n % 2)
        return values[n / 2];
    return 0.5 * (values[n / 2 - 1] + values[n / 2]);
}

/* Compute row-wise neighbour estimates and local MAD values. */
SEXP C_qc_precip_row_stats(SEXP x, SEXP weights, SEXP min_neighbours,
                           SEXP weighted)
{
    SEXP dimensions, answer;
    int nrow, ncol, minimum, use_weights, row, col, n, i;
    double total, cumulative, centre, estimate;
    double *values, *row_weights, *deviations;
    const double mad_constant = 1.4826;

    dimensions = getAttrib(x, R_DimSymbol);
    nrow = INTEGER(dimensions)[0];
    ncol = INTEGER(dimensions)[1];
    minimum = asInteger(min_neighbours);
    use_weights = asLogical(weighted);

    values = (double *) R_alloc((size_t) ncol, sizeof(double));
    row_weights = (double *) R_alloc((size_t) ncol, sizeof(double));
    deviations = (double *) R_alloc((size_t) ncol, sizeof(double));

    PROTECT(answer = allocMatrix(REALSXP, nrow, 2));
    for (row = 0; row < nrow; ++row) {
        n = 0;
        for (col = 0; col < ncol; ++col) {
            double value = REAL(x)[row + nrow * col];
            double weight = REAL(weights)[col];
            if (R_FINITE(value) && R_FINITE(weight) && weight > 0) {
                values[n] = value;
                row_weights[n] = weight;
                ++n;
            }
        }
        if (n < minimum) {
            REAL(answer)[row] = NA_REAL;
            REAL(answer)[row + nrow] = NA_REAL;
            continue;
        }

        qc_sort_pairs(values, row_weights, n);
        centre = qc_sorted_median(values, n);
        estimate = centre;
        if (use_weights) {
            total = 0;
            for (i = 0; i < n; ++i)
                total += row_weights[i];
            cumulative = 0;
            for (i = 0; i < n; ++i) {
                cumulative += row_weights[i];
                if (cumulative >= 0.5 * total) {
                    estimate = values[i];
                    break;
                }
            }
        }

        for (i = 0; i < n; ++i) {
            deviations[i] = fabs(values[i] - centre);
            row_weights[i] = 1;
        }
        qc_sort_pairs(deviations, row_weights, n);
        REAL(answer)[row] = estimate;
        REAL(answer)[row + nrow] =
            mad_constant * qc_sorted_median(deviations, n);
    }
    UNPROTECT(1);
    return answer;
}
