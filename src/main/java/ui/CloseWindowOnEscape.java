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
