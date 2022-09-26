/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 * <p>
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BasicSearch {

    @FunctionalInterface
    interface SearchMatcher {
        IsMatch isMatch(String source);
    }

    enum IsMatch {MATCH, NO_MATCH, FILTERED}

    private final List<SearchMatcher> matchers = new ArrayList<>();

    BasicSearch(final String searchText) {
        super();
        final List<String> searches = new ArrayList<>(Arrays.asList(searchText.split("\\|")));

        // remove the empties
        while (searches.remove("")) ;

        for (final String string : searches) {
            final SearchMatcher sm;

            if ('!' == string.charAt(0)) {
                if (1 == string.length()) continue;
                final String string2 = string.substring(1);

                // this is a NOT filter, if we match then filter out, otherwise let it though
                sm = (s) -> s.contains(string2) ? IsMatch.FILTERED : IsMatch.MATCH;
            } else {
                sm = (s) -> s.contains(string) ? IsMatch.MATCH : IsMatch.NO_MATCH;
            }
            this.matchers.add(sm);
        }
    }

    boolean matches(final String... sourceStrings) {
        if (this.matchers.isEmpty()) return true;

        boolean match = false;

        for (final String string : sourceStrings) {

            for (final SearchMatcher matcher : this.matchers) {
                switch (matcher.isMatch(string)) {
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
