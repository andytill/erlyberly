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

import org.hamcrest.CoreMatchers;
import org.hamcrest.MatcherAssert;
import org.junit.Test;

public class BasicSearchTest {

    @Test
    public void test01() {
        BasicSearchTest.assertMatching("wert", "qwerty");
    }

    @Test
    public void test02() {
        BasicSearchTest.assertMatching("derp|wert", "qwerty");
    }

    @Test
    public void test03() {
        BasicSearchTest.assertMatching("wert|derp", "qwerty");
    }

    @Test
    public void test04() {
        BasicSearchTest.assertMatching("wert||derp", "qwerty");
    }

    @Test
    public void test05() {
        BasicSearchTest.assertNotMatching("wert|derp", "turp");
    }

    @Test
    public void test06() {
        BasicSearchTest.assertNotMatching("wert||derp", "turp");
    }

    @Test
    public void test07() {
        BasicSearchTest.assertMatching("", "trolololol");
    }

    @Test
    public void test08() {
        // even match null when there is no search!
        BasicSearchTest.assertMatching("", null);
    }

    @Test
    public void test10() {
        BasicSearchTest.assertNotMatching("!the", "all the things");
    }

    @Test
    public void test11() {
        BasicSearchTest.assertNotMatching("the|!the", "all the things");
    }

    @Test
    public void test13() {
        BasicSearchTest.assertMatching("!the", "banjo");
    }

    private static void assertMatching(final String searchText, final String sourceText) {
        final BasicSearch basicSearch = new BasicSearch(searchText);

        MatcherAssert.assertThat(basicSearch.matches(sourceText), CoreMatchers.is(true));
    }

    private static void assertNotMatching(final String searchText, final String sourceText) {
        final BasicSearch basicSearch = new BasicSearch(searchText);

        MatcherAssert.assertThat(basicSearch.matches(sourceText), CoreMatchers.is(false));
    }
}
