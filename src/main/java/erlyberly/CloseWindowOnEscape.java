package erlyberly;

import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.stage.Stage;

class CloseWindowOnEscape implements EventHandler<KeyEvent> {
	
	private final Stage stage;

	public CloseWindowOnEscape(Stage aTermsStage) {
		stage = aTermsStage;
	}

	@Override
	public void handle(KeyEvent evt) {
	    if (evt.getCode().equals(KeyCode.ESCAPE)) {
	    	stage.close();
	    }
	}

	public static void apply(Scene scene, Stage stage) {
		scene.addEventFilter(KeyEvent.KEY_PRESSED, new CloseWindowOnEscape(stage));
	}
}