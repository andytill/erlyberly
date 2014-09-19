package erlyberly;

import java.io.IOException;
import java.net.URL;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.event.EventHandler;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.SplitPane;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import erlyberly.node.NodeAPI;

public class ErlyBerly extends Application {

	private static final NodeAPI nodeAPI = new NodeAPI();

	public static void main(String[] args) throws Exception {
		launch(args);
	}
	
    @Override
    public void start(Stage primaryStage) {
        try {
			PrefBind.setup();
		} catch (IOException e) {
			e.printStackTrace();
		}
        
		SplitPane splitPane;
		
		splitPane = new SplitPane();
		splitPane.getItems().add(loadFxml("/erlyberly/entop.fxml"));
		splitPane.getItems().add(loadFxml("/erlyberly/dbg.fxml"));
		
        primaryStage.setScene(new Scene(splitPane, 500, 250));
        primaryStage.setTitle("erlyberly");
        primaryStage.show();
        
        displayConnectionPopup(primaryStage);
    }

	private void displayConnectionPopup(Stage primaryStage) {
		Stage connectStage;
        
		connectStage = new Stage();
        connectStage.initModality(Modality.WINDOW_MODAL);
        connectStage.initOwner(primaryStage.getScene().getWindow());
        connectStage.setScene(new Scene(loadFxml("/erlyberly/connection.fxml")));

        // javafx vertical resizing is laughably ugly, lets just disallow it
        connectStage.setResizable(false);
        connectStage.setWidth(400);
        
        // if the user closes the window without connecting then close the app
        connectStage.setOnCloseRequest(new EventHandler<WindowEvent>() {
			@Override
			public void handle(WindowEvent e) {
				if(!nodeAPI.connectedProperty().get()) {
					Platform.exit();
				}
			}});

        connectStage.show();
	}

	private Parent loadFxml(String fxml) {
		URL location = getClass().getResource(fxml);
        FXMLLoader fxmlLoader = new FXMLLoader(location);

        Parent node = null;
		try {
			node = (Parent) fxmlLoader.load();
		} catch (IOException e) {
			throw new RuntimeException("Cannot load FXML");
		}
		return node;
	}

	public static NodeAPI nodeAPI() {
		return nodeAPI;
	}
}
