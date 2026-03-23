# List available recipes
default:
  @just --list

# Install git hooks (run once after cloning)
install-hooks:
  cp hooks/pre-commit .git/hooks/pre-commit
  chmod +x .git/hooks/pre-commit
  @echo "Hooks installed."

# Create a branch for a new talk or workshop
# Usage: just branch 42 talk_dds_btydbayes_202504
branch issue name:
  git checkout master
  git pull origin master
  git checkout -b {{issue}}/{{name}}
  git push -u origin {{issue}}/{{name}}
