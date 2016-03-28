package erlyberly;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BasicSearch {

    interface SearchMatcher {
        IsMatch isMatch(String source);
    }

    enum IsMatch { MATCH, NO_MATCH, FILTERED };

    private final List<SearchMatcher> matchers = new ArrayList<BasicSearch.SearchMatcher>();

    public BasicSearch(String searchText) {
        List<String> searches = new ArrayList<String>(Arrays.asList(searchText.split("\\|")));

        // remove the empties
        while(searches.remove(""));

        for (String string : searches) {
            SearchMatcher sm;

            if(string.charAt(0) == '!') {
                if(string.length() == 1)
                    continue;
                String string2 = string.substring(1);

                // this is a NOT filter, if we match then filter out, otherwise let it though
                sm = (s) -> { return s.contains(string2) ? IsMatch.FILTERED : IsMatch.MATCH; };
            }
            else {
                sm = (s) -> { return s.contains(string) ? IsMatch.MATCH : IsMatch.NO_MATCH; };
            }
            matchers.add(sm);
        }
    }

    public boolean matches(String... sourceStrings) {
        if(matchers.isEmpty())
            return true;

        boolean match = false;

        for (String string : sourceStrings) {

            if(sourceStrings == null)
                continue;

            for (SearchMatcher matcher : matchers) {
                switch(matcher.isMatch(string)) {
                case FILTERED:
                    // if this is filtered out, return false immediately
                    return false;
                case MATCH:
                    // we may match but could filter out later on
                    match = true;
                case NO_MATCH:
                    // keep going until we get something more substantive
                    break;
                }
            }
        }
        return match;
    }
}
