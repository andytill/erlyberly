package erlyberly;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BasicSearch {
	
	private final List<String> searches;

	public BasicSearch(String searchText) {
		searches = new ArrayList<String>(Arrays.asList(searchText.split("\\|")));
		
		// remove the empties
		while(searches.remove(""));
	}

	public boolean matches(String string) {
		if(searches.isEmpty())
			return true;
		if(string == null)
			return false;
		
		for (String s : searches) {
			if(string.contains(s)) {
				return true;
			}
		}
		return false;
	}
}
