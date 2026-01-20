# Internal vctrs methods

These methods are the extensions that allow panel_data objects to work
with vctrs and modern dplyr/tidyr operations.

## Usage

``` r
# S3 method for class 'panel_data'
vec_restore(x, to, ...)

# S3 method for class 'panel_data'
vec_proxy(x, ...)

# S3 method for class 'panel_data'
vec_ptype2(x, y, ...)

# S3 method for class 'panel_data.panel_data'
vec_ptype2(x, y, ...)

# S3 method for class 'panel_data.data.frame'
vec_ptype2(x, y, ...)

# S3 method for class 'data.frame.panel_data'
vec_ptype2(x, y, ...)

# S3 method for class 'panel_data.tbl_df'
vec_ptype2(x, y, ...)

# S3 method for class 'tbl_df.panel_data'
vec_ptype2(x, y, ...)

# S3 method for class 'panel_data'
vec_cast(x, to, ...)

# S3 method for class 'panel_data.panel_data'
vec_cast(x, to, ...)

# S3 method for class 'panel_data.data.frame'
vec_cast(x, to, ...)

# S3 method for class 'panel_data.tbl_df'
vec_cast(x, to, ...)

# S3 method for class 'data.frame.panel_data'
vec_cast(x, to, ...)

# S3 method for class 'tbl_df.panel_data'
vec_cast(x, to, ...)
```
