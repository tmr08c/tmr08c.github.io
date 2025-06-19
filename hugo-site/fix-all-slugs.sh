#!/usr/bin/env bash
set -euo pipefail

echo "🔧 Adding explicit slug fields to posts with directory/title mismatches..."

# Function to add slug field to a post
add_slug_to_post() {
    local file="$1"
    local slug="$2"
    
    if [ -f "$file" ]; then
        # Check if slug already exists
        if ! grep -q "^slug:" "$file"; then
            # Add slug after the date line using a more careful approach
            # Create a temporary file with the slug added
            awk -v slug="$slug" '
                /^date:/ { print; print "slug: " slug; next }
                { print }
            ' "$file" > "${file}.tmp"
            
            mv "${file}.tmp" "$file"
            echo "✓ Added slug '$slug' to $file"
        else
            echo "- Slug already exists in $file"
        fi
    else
        echo "✗ File not found: $file"
    fi
}

# Array of all mismatched posts (based on the output above)
declare -a mismatches=(
    "content/blog/2015/03/bulk-sql-activerecord/index.md:bulk-sql-activerecord"
    "content/blog/2015/03/struct-as-json/index.md:struct-as-json"
    "content/blog/2015/04/adding-carnival-to-jekyll/index.md:adding-carnival-to-jekyll"
    "content/blog/2015/04/barcamp-orlando-2015/index.md:barcamp-orlando-2015"
    "content/blog/2015/04/private-class-methods-ruby/index.md:private-class-methods-ruby"
    "content/blog/2015/09/nerdtree-and-ctrlp/index.md:nerdtree-and-ctrlp"
    "content/blog/2015/10/power-to-say-no/index.md:power-to-say-no"
    "content/blog/2019/04/golang-fass/index.md:golang-fass"
    "content/blog/2020/03/referencing-local-gem-in-gemfile/index.md:referencing-local-gem-in-gemfile"
    "content/blog/2020/07/splat-versus-spread/index.md:splat-versus-spread"
    "content/blog/2020/08/initialize-hash-value-in-ruby/index.md:initialize-hash-value-in-ruby"
    "content/blog/2020/08/writing-more-while-remote/index.md:writing-more-while-remote"
    "content/blog/2020/12/gh-cli-for-maintaining/index.md:gh-cli-for-maintaining"
    "content/blog/2021/03/99-bottles-elixir/index.md:99-bottles-elixir"
    "content/blog/2021/04/phoenix-app-in-iframe/index.md:phoenix-app-in-iframe"
    "content/blog/2021/07/creating-development-migrations-with-ecto/index.md:creating-development-migrations-with-ecto"
    "content/blog/2021/11/pattern-matching-empty-map-with-elixir/index.md:pattern-matching-empty-map-with-elixir"
    "content/blog/2022/01/projectile-async-command/index.md:projectile-async-command"
    "content/blog/2022/03/ecto-explain/index.md:ecto-explain"
    "content/blog/2022/04/if-missing-flag-cli-installers/index.md:if-missing-flag-cli-installers"
    "content/blog/2022/05/adding-psuedo-headers-with-magit/index.md:adding-psuedo-headers-with-magit"
    "content/blog/2022/06/using-new-gh-feed/index.md:using-new-gh-feed"
    "content/blog/2022/10/alt-escape-vim/index.md:alt-escape-vim"
    "content/blog/2022/11/livebook-desktop/index.md:livebook-desktop"
    "content/blog/2022/12/org-roam-node-from-elfeed-entry/index.md:org-roam-node-from-elfeed-entry"
    "content/blog/2023/05/employee-trading-cards/index.md:employee-trading-cards"
    "content/blog/2023/07/unlimited-pto-manager-tool/index.md:unlimited-pto-manager-tool"
)

# Process each mismatch
for entry in "${mismatches[@]}"; do
    file="${entry%:*}"
    slug="${entry#*:}"
    add_slug_to_post "$file" "$slug"
done

echo
echo "🔧 All slug fixes complete!"
echo "Re-running mismatch checker to verify..."
echo