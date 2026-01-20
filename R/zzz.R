# vctrs generics are non-standard, so we must register methods on load.
.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("vctrs::vec_restore", "panel_data")
  vctrs::s3_register("vctrs::vec_proxy", "panel_data")
  vctrs::s3_register("vctrs::vec_ptype2", "panel_data")
  vctrs::s3_register("vctrs::vec_ptype2", "panel_data.panel_data")
  vctrs::s3_register("vctrs::vec_ptype2", "panel_data.data.frame")
  vctrs::s3_register("vctrs::vec_ptype2", "panel_data.tbl_df")
  vctrs::s3_register("vctrs::vec_ptype2", "data.frame.panel_data")
  vctrs::s3_register("vctrs::vec_ptype2", "tbl_df.panel_data")
  vctrs::s3_register("vctrs::vec_cast", "panel_data")
  vctrs::s3_register("vctrs::vec_cast", "panel_data.panel_data")
  vctrs::s3_register("vctrs::vec_cast", "panel_data.data.frame")
  vctrs::s3_register("vctrs::vec_cast", "panel_data.tbl_df")
  vctrs::s3_register("vctrs::vec_cast", "data.frame.panel_data")
  vctrs::s3_register("vctrs::vec_cast", "tbl_df.panel_data")
}
