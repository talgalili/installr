context("Test that a global-library for R packages can be constructed")

test_that("A User-specified dir can be used as a global-library", {
   td <- tempdir()

   # create a directory for use as a global-library
   # - ensure the directory exists before calling create.global.library
   # - then check that the dir can be used as a global library
   pre_existing <- file.path(td, "cr.gl.lib.pre-existing-user-dir")
   dir.create(pre_existing)
   stopifnot(dir.exists(pre_existing))

   expect_false(
      create.global.library(pre_existing),
      info = "create.global.library did not need to construct the library"
   )
   expect_true(
      dir.exists(pre_existing),
      info = paste(
         "the (pre-existing) dir exists after calling create.global.library"
      )
   )

   # specify a directory for use as a global library
   # - ensure that the directory does not exist before calling
   # create.global.library
   # - then check that the dir can be used as a global library and that it
   # exists after calling create.global.library
   non_existing <- file.path(td, "cr.gl.lib.non-existing-user-dir")
   if (dir.exists(non_existing)) {
      unlink(non_existing, recursive = TRUE)
   }
   if (dir.exists(non_existing)) {
      stop("can't remove a pre-existing directory using unlink()")
   }

   expect_true(
      create.global.library(non_existing),
      info = "create.global.library did need to construct the library"
   )
   expect_true(
      dir.exists(non_existing),
      info = paste(
         "after calling create.global.library, a previously non-existing dir",
         "now exists"
      )
   )
})

test_that(
   "If no user-specified global-library is provided, use `<R.home>/../library`",
   {
      td <- tempdir()

      # On windows
      # - typical R installations are stored in
      # R.home() = "<path to program-files>/R/R-3.6.1"
      # - ... and we make a global library in "<path to program-files>/R/library"

      # When the global library does not already exist:
      test_dir <- file.path(td, "cr.gl.lib.non-existing-default-dir")
      mock_r_home <- file.path(test_dir, "R", "R-3.6.1")
      expected_global_library <- file.path(test_dir, "R", "library")

      dir.create(mock_r_home, recursive = TRUE)
      stopifnot(dir.exists(mock_r_home))
      if (dir.exists(expected_global_library)) {
         unlink(expected_global_library)
      }
      if (dir.exists(expected_global_library)) {
         stop("Couldn't unlink a pre-existing dir")
      }

      mockery::stub(create.global.library, "R.home", mock_r_home)
      expect_true(
         create.global.library(),
         info = "create.global.library should make the default global library"
      )
      expect_true(
         dir.exists(expected_global_library),
         info = paste(
            "default-global-library exists after running create.global.library",
            "with no args"
         )
      )
   }
)

test_that(
   "If the global-library cannot be created, throw a warning",
   {
      td <- tempdir()

      # On windows
      # - typical R installations are stored in
      # R.home() = "<path to program-files>/R/R-3.6.1"
      # - ... and we make a global library in "<path to program-files>/R/library"

      # When the global library does not already exist:
      test_dir <- file.path(td, "cr.gl.lib.unwriteable-dir")
      mock_r_home <- file.path(test_dir, "R", "R-3.6.1")
      expected_global_library <- file.path(test_dir, "R", "library")

      dir.create(mock_r_home, recursive = TRUE)
      stopifnot(dir.exists(mock_r_home))
      if (dir.exists(expected_global_library)) {
         stop("library should not already exist")
      }

      mockery::stub(create.global.library, "R.home", mock_r_home)
      mockery::stub(create.global.library, "dir.create", function(d) {
         warning("cannot create dir", d, ", reason 'Permission denied'")
         FALSE
      })

      expect_warning(
         create.global.library(),
         info = paste(
            "create.global.library should throw a warning if it can't make",
            "the global library"
         )
      )
      expect_false(
         dir.exists(expected_global_library),
         info = paste(
            "global-library can't be created in an unwriteable dir"
         )
      )
   }
)