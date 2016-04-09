/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly;

import java.util.ArrayList;

import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.control.Control;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;

public class FilterFocusManager {

    private static final KeyCodeCombination REFRESH_MODULES_SHORTCUT = new KeyCodeCombination(KeyCode.F, KeyCombination.SHORTCUT_DOWN);

    private static final ArrayList<Control> FILTERS = new ArrayList<>();

    private static int lastFocusedIndex = -1;

    public static void init(Scene scene) {
        FILTERS.add(null);
        FILTERS.add(null);
        FILTERS.add(null);

        Platform.runLater(() -> {
            scene.getAccelerators().put(REFRESH_MODULES_SHORTCUT, () -> { nextFilter(); });
        });
    }

    private static void nextFilter() {
        if(FILTERS.isEmpty())
            return;

        Control control = findNextFilter();

        requestFocus(control);
    }

    private static Control findNextFilter() {
        if(lastFocusedIndex != -1) {
            Control lastFocused = FILTERS.get(lastFocusedIndex);
            if(!lastFocused.isFocused()) {
                return lastFocused;
            }
        }

        int focused = findCurrentFilter();

        if(focused == -1) {
            focused = 0;
        }
        else {
            // iterate the filters until we find one that is part of the scene or we run out of filters
            int iterations = 0;
            while(iterations < FILTERS.size()) {
                iterations++;
                focused++;
                if(focused >= FILTERS.size()) {
                    focused = 0;
                }
                boolean isFocusable = FILTERS.get(focused).getScene() != null;
                if(isFocusable)
                    break;
            }
        }

        return FILTERS.get(focused);
    }

    private static int findCurrentFilter() {
        int focused = -1;
        for (int i = 0; i < FILTERS.size(); i++) {
            Control control = FILTERS.get(i);
            if(control != null && control.isFocused()) {
                focused = i;
                break;
            }
        }
        return focused;
    }

   private static void requestFocus(Control control) {
       if(control != null)
           Platform.runLater(() -> { control.requestFocus(); });
    }

   public static void addFilter(Control control, int order) {
        assert control != null;
        assert order >= 0;
        assert Platform.isFxApplicationThread();

        FILTERS.set(order, control);

        control.focusedProperty().addListener((o, oldFocus, newFocus) -> {
            lastFocusedIndex = order;
        });
    }
}
