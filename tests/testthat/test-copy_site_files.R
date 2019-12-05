context("Test that copy_site_files argument is respected in updateR")

#' Mock old & new R installation paths - used to mock "get.installed.R.folders"
#'
#' Create two temporary directories to simulate old and new R installations.
#' Then return a function which, when called for the first time will return the
#' old installation path, when called the second time will return the new
#' installation path, and if called a third time will return the new path.
#'
#' @param Renviron.site add an Renviron.site file to the old installation path
#' @param Rprofile.site add an Rprofile.site file to the old installation path
#'
#' @return a function
#' 
#' > get_r_paths <- mock_r_paths(Rprofile.site=T, Renviron.site=F)
#' > list.files(file.path(get_r_paths(), 'etc'))
#' [1] "Rprofile.site"
#' > list.files(file.path(get_r_paths(), 'etc'))
#' character(0)
mock_r_paths <- function(Renviron.site, Rprofile.site) {

  # create old dir with etc & Rprofile.site/Renviron.site files
  tf <- tempfile()
  tf_etc <- file.path(tf, 'etc')
  dir.create(tf_etc, recursive = T)
  if(Rprofile.site==T) writeChar('foo', file.path(tf_etc, 'Rprofile.site'))
  if(Renviron.site==T) writeChar('foo', file.path(tf_etc, 'Renviron.site'))
  old_rpath <- tf
  
  # create new dir with etc but no Rprofile.site/Renviron.site files
  tf <- tempfile()
  tf_etc <- file.path(tf, 'etc')
  dir.create(tf_etc, recursive = T)
  new_rpath <- tf
  
  paths <- c(old_rpath, new_rpath)
  idx <- 0
  
  get_r_path <- function() {
    idx <<- min(idx + 1, length(paths))
    return(paths[idx])
  }
  
  return(get_r_path)
  
}

#' Mock the answers to yes/no questions - used to mock "ask.user.yn.question"
#'
#' Return T to the question of whether to install R.  Return T or F to the
#' question about copying .site files based on the user-provided argument.
#' Return F to everything else.
#'
#' @param copy_question_response T/F - how should the user response to the question about copying site files? 
#'
#' @return a function
mock_yn_answers <- function(copy_question_response) {
  get_answer <- function(question, GUI = TRUE, add_lines_before = TRUE) {
    if(question == "Do you wish to install the latest version of R?") {
      return(T)
    } else if (question == "Do you wish to copy your 'Rprofile.site' and 'Renviron.site' from the older version of R to the newer version of R?") {
      warning("User was asked about copying") # this warning is used to track if the user was asked this question or not
      return(copy_question_response)
    } else {
      return(F) 
    }    
  }
}

# Create a data frame capturing all possible inputs of interest - we will run a
# test for each combination of these
input_permutations <- expand.grid(
  Rprofile.site=c(T,F), # does Rprofile.site exist in old installation path?
  Renviron.site=c(T,F), # does Renviron.site exist in old installation path?
  copy_site_files=c(T,F,NA), # value of copy_site_files argument to updateR function (T,F,NA used to represent a missing arg)
  copy_Rprofile.site=c(T,F,NA), # value of copy_Rprofile.site argument to updateR function (T,F,NA used to represent a missing arg)
  copy_question_response=c(T,F) # user response to the question of whether to copy or not
)

# These vectors will be used to determine expectations for each input case
Rprofile.site <- input_permutations[["Rprofile.site"]]
Renviron.site <- input_permutations[["Renviron.site"]]
copy_site_files <- input_permutations[["copy_site_files"]]
copy_Rprofile.site <- input_permutations[["copy_Rprofile.site"]]
copy_question_response <- input_permutations[["copy_question_response"]]

# This section describes the logic for figuring out whether files should be copied or not
# We create a vector do_copy_files which will flag for each set of inputs whether files should be copied
#
# if copy site files is false, OR
# if copy site files is missing and copy_rprofile.site is set to false OR
# if copy site files is missing and copy_rprofile.site is missing and the user answers NO to the prompt
# then we won't copy files
# otherwise we will
# note:  use %in% instead of == because (NA %in% F) returns F
do_not_copy_files <- (
  copy_site_files %in% F | 
    (is.na(copy_site_files) & copy_Rprofile.site %in% F) |
    (is.na(copy_site_files) & is.na(copy_Rprofile.site) & copy_question_response == F)
)
do_copy_files <- !do_not_copy_files

# Expectations to check for each test:
# Does the output dir contain Rprofile.site? (T/F)
# Does the output dir contain Renviron.site? (T/F)
# Is a deprecation warning produced, per the below? (T/F)
# Is a file not found warning produced, per the below? (T/F)
# Was the user asked about copying the site files? (T/F)

# deprection/file not found warnings:
# warning('"copy_Rprofile.site" argument has been deprecated in favour of copy_site_files - both Renviron.site and Rprofile.site will be copied if they exist')
# warning('Could not find either "Rprofile.site" or "Renviron.site" in your old R-etc folder')

# if we copied files & Rprofile.site exists, output dir should contain Rprofile.site
input_permutations[["expect_Rprofile.site"]] <- do_copy_files & Rprofile.site 
# if we copied files & Renviron.site exists, output dir should contain Renviron.site
input_permutations[["expect_Renviron.site"]] <- do_copy_files & Renviron.site #
# if updateR was called with copy_Rprofile.site, a deprecation warning should be given
input_permutations[["expect_deprecation_warning"]] <- copy_Rprofile.site %in% c(T,F) 
# if we copied fields and neither Rprofile.site or Renviron.site exists a file not found warning should be given
input_permutations[["expect_file_not_found_warning"]] <- do_copy_files & !(Rprofile.site | Renviron.site) 
# if neither copy_site_files nor copy_Rprofile.site arguments were provided, user should be prompted
input_permutations[["expect_user_prompt"]] <- is.na(copy_site_files) & is.na(copy_Rprofile.site)


#' Run a test for a combination of inputs
#' 
#' @param Rprofile.site does Rprofile.site exist in old installation path? (T,F)
#' @param Renviron.site does Renviron.site exist in old installation path? (T,F)
#' @param copy_site_files value of copy_site_files argument to updateR function (T,F,NA used to represent a missing arg)
#' @param copy_Rprofile.site value of copy_Rprofile.site argument to updateR function (T,F,NA used to represent a missing arg)
#' @param copy_question_response user response to the question of whether to copy or not
#' @param expect_Rprofile.site Does the output dir contain Rprofile.site? (T/F)
#' @param expect_Renviron.site Does the output dir contain Renviron.site? (T/F)
#' @param expect_deprecation_warning Is a deprecation warning produced? (T/F)
#' @param expect_file_not_found_warning Is a file not found warning produced? (T/F)
#' @param expect_user_prompt Was the user asked about copying the site files? (T/F)
#'
#' @return NULL
make_copy_site_files_test <- function(
  Rprofile.site, 
  Renviron.site, 
  copy_site_files, 
  copy_Rprofile.site,
  copy_question_response,
  expect_Rprofile.site,
  expect_Renviron.site,
  expect_deprecation_warning,
  expect_file_not_found_warning,
  expect_user_prompt
) {
  
  # Standard mocks - always detect a new version of R, always return success for install.R
  mockery::stub(updateR, "check.for.updates.R", T)
  mockery::stub(updateR, "install.R", T)
  
  # R path mock - will create Renviron.site/Rprofile.site in the old r path or
  # not depending on arguments
  get_r_paths <- mock_r_paths(Renviron.site=Renviron.site, Rprofile.site=Rprofile.site)
  mockery::stub(updateR, "get.installed.R.folders", get_r_paths)
  
  # Ask Y/N mock - will answer YES to install R, NO to most other Qs, and
  # respond to the Q about copying site files depending on the
  # copy_question_response argument
  mockery::stub(updateR, "ask.user.yn.question", mock_yn_answers(copy_question_response))
  
  # Suppress the 'Installing the newest version of R' message
  mockery::stub(updateR, "cat", function(x) {
    msg <- "Installing the newest version of R"
    if(substr(x, 1, nchar(msg)) == msg) return(invisible(NULL))
    cat(x)
  })
  
  test_that(
    sprintf('Rprofile.site: %s, Renviron.site: %s, copy_site_files: %s, copy_Rprofile.site: %s, copy_question_response: %s',
      Rprofile.site,
      Renviron.site,
      copy_site_files,
      copy_Rprofile.site,
      copy_question_response
    ),{
      
    # arguments to use when calling updateR
    arglist <- list(
      Rprofile.site=Rprofile.site, 
      Renviron.site=Renviron.site
    )
    if(!is.na(copy_site_files)) arglist[['copy_site_files']] <- copy_site_files
    if(!is.na(copy_Rprofile.site)) arglist[['copy_Rprofile.site']] <- copy_Rprofile.site
      
    # generate and capture warnings
    generated_warnings <- capture_warnings(
      do.call(
        updateR,
        args = arglist
      )
    )
    
    # get set of expected warning messages based on arguments
    possible_warnings <- c(
      expect_deprecation_warning='"copy_Rprofile.site" argument has been deprecated in favour of copy_site_files - both Renviron.site and Rprofile.site will be copied if they exist',
      expect_file_not_found_warning='Could not find either "Rprofile.site" or "Renviron.site" in your old R-etc folder',
      expect_user_prompt='User was asked about copying'
    )
    expect_warning_arguments <- c(
      expect_deprecation_warning,
      expect_file_not_found_warning,
      expect_user_prompt
    )
    expected_warnings <- possible_warnings[expect_warning_arguments]
    
    # check generated warnings match expected
    expect_setequal(!!generated_warnings, !!expected_warnings)
    
    # re-use function from mock to 
    new_r_path <- get_r_paths()
    new_r_path_etc_files <- list.files(file.path(new_r_path, 'etc'))
    
    # get set of expected files in new r path based on arguments
    possible_files <- c(
      expect_Rprofile.site='Rprofile.site',
      expect_Renviron.site='Renviron.site'
    )
    expect_file_arguments <- c(
      expect_Rprofile.site,
      expect_Renviron.site
    )
    expected_files <- possible_files[expect_file_arguments]
    
    # check new_r_path_files match expected
    expect_setequal(!!new_r_path_etc_files, !!expected_files) 
      
  })
  
}

# run a test for each row in input_permutations
for (i in seq_len(nrow(input_permutations))) {
  
  # convert values from the row into an arglist
  arglist <- sapply(names(input_permutations), function(j) {
    input_permutations[i,j]
  }, USE.NAMES = T, simplify = F)
  
  # run the test
  do.call(
    make_copy_site_files_test,
    args = arglist
  )
  
}