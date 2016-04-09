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
package ui;

import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

public class CloseWindowOnEscape implements EventHandler<KeyEvent> {

    private final Stage stage;

    public CloseWindowOnEscape(Stage aTermsStage) {
        stage = aTermsStage;
    }

    @Override
    public void handle(KeyEvent evt) {
        if (evt.getCode().equals(KeyCode.ESCAPE)) {
            EventHandler<WindowEvent> onCloseRequest = stage.getOnCloseRequest();
            if(onCloseRequest != null) {
                onCloseRequest.handle(null);
            }
            stage.close();
        }
    }

    public static void apply(Scene scene, Stage stage) {
        scene.addEventFilter(KeyEvent.KEY_PRESSED, new CloseWindowOnEscape(stage));
    }
}
