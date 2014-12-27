package erlyberly;

import static org.junit.Assert.*;

import org.junit.Test;

public class BasicSearchTest {

	@Test
	public void test() {
		assertMatching("wert", "qwerty");
	}

	@Test
	public void test2() {
		assertMatching("derp|wert", "qwerty");
	}

	@Test
	public void test3() {
		assertMatching("wert|derp", "qwerty");
	}

	@Test
	public void test4() {
		assertMatching("wert||derp", "qwerty");
	}

	@Test
	public void test5() {
		assertNotMatching("wert|derp", "turp");
	}
	
	@Test
	public void test6() {
		assertNotMatching("wert||derp", "turp");
	}

	@Test
	public void test7() {
		assertMatching("", "trolololol");
	}

	@Test
	public void test8() {
		// even match null when there is no search!
		assertMatching("", null);
	}

	@Test
	public void test9() {
		assertNotMatching("all the things", null);
	}

	@Test
	public void test10() {
		assertNotMatching("!the", "all the things");
	}

	@Test
	public void test11() {
		// positive match occurring first
		assertMatching("the|!the", "all the things");
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
