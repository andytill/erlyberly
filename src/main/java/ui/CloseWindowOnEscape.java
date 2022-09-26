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
package ui;

import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

public final class CloseWindowOnEscape implements EventHandler<KeyEvent> {

    private final Stage stage;

    private CloseWindowOnEscape(final Stage aTermsStage) {
        super();
        this.stage = aTermsStage;
    }

    @Override
    public void handle(final KeyEvent evt) {
        if (KeyCode.ESCAPE == evt.getCode()) {
            final EventHandler<WindowEvent> onCloseRequest = this.stage.getOnCloseRequest();
            if (null != onCloseRequest) {
                onCloseRequest.handle(null);
            }
            this.stage.close();
        }
    }

    static void apply(final Scene scene, final Stage stage) {
        scene.addEventFilter(KeyEvent.KEY_PRESSED, new CloseWindowOnEscape(stage));
    }
}
