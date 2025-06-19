#!/usr/bin/env bash
set -euo pipefail

echo "🔧 Fixing slug mismatches for internal links..."

# Add slug to posts that need specific slugs to match internal links
add_slug() {
    local file="$1"
    local slug="$2"
    
    if [ -f "$file" ]; then
        # Check if slug already exists
        if ! grep -q "^slug:" "$file"; then
            # Add slug after the date line
            sed -i '' "/^date:/a\\
slug: $slug" "$file"
            echo "✓ Added slug '$slug' to $file"
        else
            echo "- Slug already exists in $file"
        fi
    else
        echo "✗ File not found: $file"
    fi
}

# Fix posts that have broken internal links pointing to them
add_slug "content/blog/2020/04/how-i-debug-my-dependencies/index.md" "how-i-debug-my-dependencies"
add_slug "content/blog/2015/11/more-specific-factories/index.md" "more-specific-factories"
add_slug "content/blog/2022/08/add-timestamps-to-org-files/index.md" "add-timestamps-to-org-files"
add_slug "content/blog/2022/04/open-source-open-eyes/index.md" "open-source-open-eyes"
add_slug "content/blog/2021/06/more-specific-factories-with-traits/index.md" "more-specific-factories-with-traits"
add_slug "content/blog/2021/05/using-mix-install/index.md" "using-mix-install"
add_slug "content/blog/2015/05/first-open-source-contribution/index.md" "first-open-source-contribution"
add_slug "content/blog/2015/03/anatomy-of-ruby-class/index.md" "anatomy-of-ruby-class"
add_slug "content/blog/2021/09/intro-to-postgres-explain/index.md" "intro-to-postgres-explain"

echo "🔧 Slug fixes complete!"