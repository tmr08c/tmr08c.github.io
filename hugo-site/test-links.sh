#!/usr/bin/env bash
set -euo pipefail

echo "🔗 Testing all internal blog links..."

# Base URL for testing
BASE_URL="http://localhost:1313"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
TOTAL_LINKS=0
WORKING_LINKS=0
BROKEN_LINKS=0

# Function to test a link
test_link() {
    local url="$1"
    local source_file="$2"
    TOTAL_LINKS=$((TOTAL_LINKS + 1))
    
    # Test the link
    if curl -s -f -o /dev/null "$BASE_URL$url"; then
        echo -e "${GREEN}✓${NC} $url (from $source_file)"
        WORKING_LINKS=$((WORKING_LINKS + 1))
    else
        echo -e "${RED}✗${NC} $url (from $source_file)"
        BROKEN_LINKS=$((BROKEN_LINKS + 1))
    fi
}

echo "Testing internal links found in blog posts..."
echo

# Test all the links found in the previous analysis
test_link "/2020/11/gh-cli-for-contributing" "2020/12/gh-cli-for-maintaining"
test_link "/2020/04/how-i-debug-my-dependencies" "2020/06/decision-making-spectrums"
test_link "/2020/05/concurrent-ruby-hello-async/" "2020/09/concurrent-ruby-lazy-threads"
test_link "/2015/11/more-specific-factories" "2021/06/more-specific-factories-with-traits"
test_link "/2022/08/add-timestamps-to-org-files/" "2022/12/org-roam-node-from-elfeed-entry"
test_link "/2015/05/why-i-love-the-cloud/" "2015/10/spotify-running"
test_link "/2022/04/open-source-open-eyes/" "2022/06/using-new-gh-feed"
test_link "/2021/06/more-specific-factories-with-traits/" "2015/11/more-specific-factories"
test_link "/2015/11/using-vim-to-drive-tdd/" "2015/11/more-specific-factories"
test_link "/2020/04/how-i-debug-my-dependencies" "2020/03/referencing-local-gem-in-gemfile"
test_link "/2021/05/using-mix-install/" "2021/10/using-mix-install-for-benchmarks"
test_link "/2015/02/why-i-made-this-blog/" "2015/05/first-open-source-contribution"
test_link "/2023/03/job-search-checklist/" "2023/04/questions-to-ask-an-interviewer"
test_link "/2015/02/why-i-made-this-blog/" "2023/04/questions-to-ask-an-interviewer"
test_link "/2015/05/first-open-source-contribution/" "2023/04/questions-to-ask-an-interviewer"
test_link "/2021/12/2021-gratitude/" "2023/01/2022-gratitude"
test_link "/2023/02/extracting-2022-gratitude-entries/" "2023/01/2022-gratitude"
test_link "/2022/04/open-source-open-eyes/" "2023/01/2022-gratitude"
test_link "/2021/12/2021-gratitude/" "2023/02/extracting-2022-gratitude-entries"
test_link "/2023/01/2022-gratitude" "2023/02/extracting-2022-gratitude-entries"
test_link "/2015/03/anatomy-of-ruby-class/" "2015/04/private-class-methods-ruby"
test_link "/2021/09/intro-to-postgres-explain" "2022/03/ecto-explain"
test_link "/2021/12/2021-gratitude/" "2023/03/job-search-checklist"

# Test a few anchor links (these might fail if sections don't exist)
echo
echo "Testing anchor links (these may fail if section headers changed)..."
test_link "/2020/04/how-i-debug-my-dependencies#asking-for-help" "2020/06/decision-making-spectrums"
test_link "/2020/05/concurrent-ruby-hello-async/#return-types" "2020/09/concurrent-ruby-lazy-threads"
test_link "/2022/08/add-timestamps-to-org-files/#adding-the-timestamps" "2022/12/org-roam-node-from-elfeed-entry"
test_link "/2021/12/2021-gratitude/#getting-access" "2023/01/2022-gratitude"
test_link "/2021/12/2021-gratitude/#first-attempt---org-element" "2023/02/extracting-2022-gratitude-entries"
test_link "/2021/12/2021-gratitude/#second-attempt---sed" "2023/02/extracting-2022-gratitude-entries"
test_link "/2022/08/add-timestamps-to-org-files/#getting-dates" "2023/02/extracting-2022-gratitude-entries"
test_link "/2021/09/intro-to-postgres-explain/#caveats" "2022/03/ecto-explain"

# Test the /talks link
echo
echo "Testing non-blog internal links..."
test_link "/talks" "2015/04/barcamp-orlando-2015"

echo
echo "📊 Summary:"
echo "Total links tested: $TOTAL_LINKS"
echo -e "Working links: ${GREEN}$WORKING_LINKS${NC}"
echo -e "Broken links: ${RED}$BROKEN_LINKS${NC}"

if [ $BROKEN_LINKS -eq 0 ]; then
    echo -e "${GREEN}🎉 All internal links are working!${NC}"
    exit 0
else
    echo -e "${RED}⚠️ Some links are broken and need attention.${NC}"
    exit 1
fi