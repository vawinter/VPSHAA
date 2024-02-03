# Setting up github
# 10/11/2022

library(usethis)


use_git_config(user.name = "vermanica",
               user.email = "veronicaann621@gmail.com")

git_default_branch_configure()

use_git_remote(
  "origin",
  "https://github.com/vermanica/eHSF.git",
  overwrite = T
)

create_github_token()
# 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

gitcreds::gitcreds_set()


# setting URL if above does not work
git remote set-url origin https://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@github.com/vermanica/eHSF.git