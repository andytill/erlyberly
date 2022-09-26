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

import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.control.Control;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;

import java.util.ArrayList;

public enum FilterFocusManager {
    ;

    private static final KeyCodeCombination REFRESH_MODULES_SHORTCUT = new KeyCodeCombination(KeyCode.F, KeyCombination.SHORTCUT_DOWN);

    private static final ArrayList<Control> FILTERS = new ArrayList<>();

    private static int lastFocusedIndex = -1;

    public static void init(final Scene scene) {
        FILTERS.add(null);
        FILTERS.add(null);
        FILTERS.add(null);

        Platform.runLater(() -> scene.getAccelerators().put(REFRESH_MODULES_SHORTCUT, FilterFocusManager::nextFilter));
    }

    private static void nextFilter() {
        if (FILTERS.isEmpty()) return;

        final Control control = findNextFilter();

        requestFocus(control);
    }

    private static Control findNextFilter() {
        // if we know about a filter control that was focused last but does not currently
        // have focus then return to it. The filter must be visible and attached to the scene.
        if (-1 != lastFocusedIndex) {
            final Control lastFocused = FILTERS.get(lastFocusedIndex);
            if (!lastFocused.isFocused() && lastFocused.isVisible() && null != lastFocused.getScene()) {
                return lastFocused;
            }
        }

        int focused = findCurrentFilter();

        if (-1 == focused) {
            focused = 0;
        }
        // iterate the filters until we find one that is part of the scene or we run out of filters
        int iterations = 0;
        while (iterations < FILTERS.size()) {
            iterations++;
            focused++;
            if (focused >= FILTERS.size()) {
                focused = 0;
            }
            final boolean isFocusable = null != FILTERS.get(focused).getScene();
            if (isFocusable) break;
        }
        return FILTERS.get(focused);
    }

    private static int findCurrentFilter() {
        int focused = -1;
        for (int i = 0; i < FILTERS.size(); i++) {
            final Control control = FILTERS.get(i);
            if (null != control && control.isFocused()) {
                focused = i;
                break;
            }
        }
        return focused;
    }

    private static void requestFocus(final Control control) {
        if (null != control) Platform.runLater(control::requestFocus);
    }

    public static void addFilter(final Control control, final int order) {
        assert null != control;
        assert 0 <= order;
        assert Platform.isFxApplicationThread();

        FILTERS.set(order, control);

        control.focusedProperty().addListener((o, oldFocus, newFocus) -> lastFocusedIndex = order);
    }
}
