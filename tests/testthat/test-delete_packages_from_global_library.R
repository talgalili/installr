context("Test deletion of packages from the global-library (length guard)")

# Regression tests for the guard that protects the recursive unlink() used when
# removing packages from the global library.
#
# The original code used `if (package.to.del.from.global.lib > 0)` on a
# character vector of candidate paths. That guard:
#   * errored with "argument is of length zero" when no packages matched
#     (i.e. the vector was character(0)), and
#   * only inspected the first element (with a "condition has length > 1"
#     warning) when several paths were present.
# The fix checks `length(package.to.del.from.global.lib) > 0` instead. These
# tests exercise zero, one and multiple candidate paths using temporary
# directories so that no real files are ever at risk.

test_that("delete_packages_from_global_library does not error on zero candidates", {
   # character(0) must return cleanly (the old guard errored here).
   expect_silent(
      out <- installr:::delete_packages_from_global_library(character(0))
   )
   expect_equal(out, 0)
})

test_that("delete_packages_from_global_library deletes a single candidate path", {
   td <- tempfile()
   dir.create(td)
   on.exit(unlink(td, recursive = TRUE), add = TRUE)

   pkg <- file.path(td, "pkgA")
   dir.create(pkg)
   stopifnot(dir.exists(pkg))

   expect_equal(
      installr:::delete_packages_from_global_library(pkg),
      1
   )
   expect_false(dir.exists(pkg))
})

test_that("delete_packages_from_global_library deletes multiple candidate paths", {
   td <- tempfile()
   dir.create(td)
   on.exit(unlink(td, recursive = TRUE), add = TRUE)

   to_delete <- file.path(td, c("pkgA", "pkgB", "pkgC"))
   keep <- file.path(td, "pkgKeep")
   for (d in c(to_delete, keep)) dir.create(d)
   stopifnot(all(dir.exists(c(to_delete, keep))))

   expect_equal(
      installr:::delete_packages_from_global_library(to_delete),
      3
   )
   expect_false(any(dir.exists(to_delete)))
   expect_true(dir.exists(keep))
})
