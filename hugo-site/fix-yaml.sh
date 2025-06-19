#!/usr/bin/env bash
set -euo pipefail

echo "🔧 Fixing YAML syntax errors..."

fix_yaml() {
    local file="$1"
    local slug="$2"
    
    if [ -f "$file" ]; then
        # Fix the broken YAML by replacing the malformed line
        sed -i '' "s/^slug: ${slug}tags:/slug: ${slug}\ntags:/" "$file"
        echo "✓ Fixed YAML in $file"
    else
        echo "✗ File not found: $file"
    fi
}

# Fix all the broken files
fix_yaml "content/blog/2022/04/open-source-open-eyes/index.md" "open-source-open-eyes"
fix_yaml "content/blog/2022/08/add-timestamps-to-org-files/index.md" "add-timestamps-to-org-files"
fix_yaml "content/blog/2015/05/first-open-source-contribution/index.md" "first-open-source-contribution"
fix_yaml "content/blog/2015/11/more-specific-factories/index.md" "more-specific-factories"
fix_yaml "content/blog/2021/05/using-mix-install/index.md" "using-mix-install"
fix_yaml "content/blog/2021/09/intro-to-postgres-explain/index.md" "intro-to-postgres-explain"
fix_yaml "content/blog/2021/06/more-specific-factories-with-traits/index.md" "more-specific-factories-with-traits"
fix_yaml "content/blog/2020/04/how-i-debug-my-dependencies/index.md" "how-i-debug-my-dependencies"

echo "🔧 YAML fixes complete!"