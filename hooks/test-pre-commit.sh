#!/usr/bin/env bash
# Test the pre-commit hook
echo "Testing pre-commit hook on master branch..."
git checkout master 2>/dev/null
touch test-file.txt
git add test-file.txt
git commit -m "Test commit" 2>&1 | head -15
git reset HEAD test-file.txt 2>/dev/null
rm -f test-file.txt
