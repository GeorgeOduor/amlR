#
#
#
golem::fill_desc(
  pkg_name = "aml",
  pkg_title = "Maishabank",
  pkg_description = "PKG_DESC.",
  author_first_name = "george",
  author_last_name = "oduor",
  author_email = "AUTHOR@MAIL.COM",
  repo_url = NULL
)
golem::set_golem_options(talkative = T)
usethis::use_mit_license("Golem User")
usethis::use_readme_rmd(open = FALSE)
usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)
usethis::use_git()
golem::use_recommended_tests()
golem::use_favicon()
golem::remove_favicon()
golem::use_utils_ui(with_test = F)
golem::use_utils_server(with_test = F)
rstudioapi::navigateToFile("dev/02_dev.R")
