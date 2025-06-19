#!/usr/bin/env bash
set -euo pipefail

echo "🗓️ Checking for date mismatches between directory structure and frontmatter..."

# Function to extract date from frontmatter
extract_date() {
    local file="$1"
    awk '/^---$/{flag=!flag} flag && /^date:/' "$file" | \
        sed "s/^date: *//; s/['\"]//g" | \
        head -1
}

# Find all blog post index.md files
find content/blog -name "index.md" | sort | while read -r post_file; do
    # Extract directory year/month
    dir_path=$(dirname "$post_file")
    dir_year=$(echo "$dir_path" | cut -d'/' -f3)
    dir_month=$(echo "$dir_path" | cut -d'/' -f4)
    
    # Extract date from frontmatter
    frontmatter_date=$(extract_date "$post_file")
    
    if [ -z "$frontmatter_date" ]; then
        echo "❌ $post_file - No date found"
        continue
    fi
    
    # Extract year and month from frontmatter date
    fm_year=$(date -j -f "%Y-%m-%d" "$(echo "$frontmatter_date" | cut -d'T' -f1)" "+%Y" 2>/dev/null || echo "invalid")
    fm_month=$(date -j -f "%Y-%m-%d" "$(echo "$frontmatter_date" | cut -d'T' -f1)" "+%m" 2>/dev/null || echo "invalid")
    
    if [ "$fm_year" = "invalid" ] || [ "$fm_month" = "invalid" ]; then
        echo "❌ $post_file - Invalid date format: $frontmatter_date"
        continue
    fi
    
    # Compare directory vs frontmatter date
    if [ "$dir_year" = "$fm_year" ] && [ "$dir_month" = "$fm_month" ]; then
        echo "✅ $dir_year/$dir_month - Date matches frontmatter"
    else
        echo "🔥 MISMATCH: $post_file"
        echo "   Directory: $dir_year/$dir_month"
        echo "   Frontmatter: $fm_year/$fm_month ($frontmatter_date)"
        echo
    fi
done