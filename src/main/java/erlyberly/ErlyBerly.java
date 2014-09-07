package erlyberly;

import java.io.IOException;
import java.net.URL;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;

public class ErlyBerly extends Application {

	public static void main(String[] args) throws Exception {
		launch(args);
	}
	
    @Override
    public void start(Stage primaryStage) {
        
        URL location = getClass().getResource("/erlyberly/entop.fxml");
        FXMLLoader fxmlLoader = new FXMLLoader(location);

        Pane root = null;
		try {
			root = (Pane)fxmlLoader.load();
		} catch (IOException e) {
			throw new RuntimeException("Cannot load FXML");
		}
        
        primaryStage.setScene(new Scene(root, 500, 250));
        primaryStage.setTitle("erlyberly");
        primaryStage.show();
    }
}
