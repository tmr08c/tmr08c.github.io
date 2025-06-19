#!/usr/bin/env bash
set -euo pipefail

echo "🔍 Checking for slug mismatches between directory names and post titles..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL_POSTS=0
MISMATCHES=0
MATCHES=0

# Function to slugify a title (mimic Hugo's behavior)
slugify() {
    echo "$1" | \
        tr '[:upper:]' '[:lower:]' | \
        sed 's/[^a-z0-9]/-/g' | \
        sed 's/--*/-/g' | \
        sed 's/^-\|-$//g'
}

# Function to extract title from markdown frontmatter
extract_title() {
    local file="$1"
    # Extract title from YAML frontmatter
    awk '/^---$/{flag=!flag} flag && /^title:/' "$file" | \
        sed 's/^title: *//; s/^"//; s/"$//' | \
        head -1
}

# Function to check if post already has explicit slug
has_explicit_slug() {
    local file="$1"
    grep -q "^slug:" "$file"
}

echo "Analyzing all blog posts..."
echo

# Find all blog post index.md files
find content/blog -name "index.md" | sort | while read -r post_file; do
    TOTAL_POSTS=$((TOTAL_POSTS + 1))
    
    # Extract directory name (should be the expected slug)
    dir_path=$(dirname "$post_file")
    expected_slug=$(basename "$dir_path")
    
    # Extract title from post
    title=$(extract_title "$post_file")
    
    if [ -z "$title" ]; then
        echo -e "${RED}✗${NC} $post_file - No title found"
        continue
    fi
    
    # Generate what Hugo would use as slug from title
    hugo_slug=$(slugify "$title")
    
    # Check if already has explicit slug
    if has_explicit_slug "$post_file"; then
        echo -e "${BLUE}i${NC} $expected_slug - Already has explicit slug"
        continue
    fi
    
    # Compare expected vs Hugo-generated slug
    if [ "$expected_slug" = "$hugo_slug" ]; then
        echo -e "${GREEN}✓${NC} $expected_slug - Matches title: \"$title\""
        MATCHES=$((MATCHES + 1))
    else
        echo -e "${YELLOW}!${NC} $expected_slug - Title: \"$title\""
        echo -e "    Expected: $expected_slug"
        echo -e "    Hugo would generate: $hugo_slug"
        echo -e "    File: $post_file"
        MISMATCHES=$((MISMATCHES + 1))
        echo
    fi
done

echo
echo "📊 Summary:"
echo "Total posts analyzed: $TOTAL_POSTS"
echo -e "Matching slugs: ${GREEN}$MATCHES${NC}"
echo -e "Mismatched slugs: ${YELLOW}$MISMATCHES${NC}"

if [ $MISMATCHES -gt 0 ]; then
    echo
    echo -e "${YELLOW}💡 Posts with mismatches need explicit 'slug:' fields in their frontmatter${NC}"
    echo "This will ensure internal links continue to work correctly."
fi