package erlyberly;

import static org.junit.Assert.*;

import org.junit.Test;

public class BasicSearchTest {

	@Test
	public void test01() {
		assertMatching("wert", "qwerty");
	}

	@Test
	public void test02() {
		assertMatching("derp|wert", "qwerty");
	}

	@Test
	public void test03() {
		assertMatching("wert|derp", "qwerty");
	}

	@Test
	public void test04() {
		assertMatching("wert||derp", "qwerty");
	}

	@Test
	public void test05() {
		assertNotMatching("wert|derp", "turp");
	}
	
	@Test
	public void test06() {
		assertNotMatching("wert||derp", "turp");
	}

	@Test
	public void test07() {
		assertMatching("", "trolololol");
	}

	@Test
	public void test08() {
		// even match null when there is no search!
		assertMatching("", null);
	}

	@Test
	public void test10() {
		assertNotMatching("!the", "all the things");
	}

	@Test
	public void test11() {
		assertNotMatching("the|!the", "all the things");
	}

	@Test
	public void test13() {
		assertMatching("!the", "banjo");
	}

	private void assertMatching(String searchText, String sourceText) {
		BasicSearch basicSearch = new BasicSearch(searchText);
		
		assertTrue(basicSearch.matches(sourceText));
	}

	private void assertNotMatching(String searchText, String sourceText) {
		BasicSearch basicSearch = new BasicSearch(searchText);
		
		assertFalse(basicSearch.matches(sourceText));
	}
}
