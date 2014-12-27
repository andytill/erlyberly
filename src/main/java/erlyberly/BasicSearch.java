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
				sm = (s) -> { return s.contains(string2) ? IsMatch.FILTERED : IsMatch.NO_MATCH; };
			}
			else {
				sm = (s) -> { return s.contains(string) ? IsMatch.MATCH : IsMatch.NO_MATCH; };
			}
			matchers.add(sm);
		}
	}

	public boolean matches(String string) {
		if(matchers.isEmpty())
			return true;
		if(string == null)
			return false;
		
		for (SearchMatcher matcher : matchers) {
			switch(matcher.isMatch(string)) {
			case FILTERED:
				return false;
			case MATCH:
				return true;
			case NO_MATCH:
				break;
			}
		}
		return false;
	}
}
