package erlyberly;

import java.io.IOException;
import java.net.URL;

import erlyberly.node.NodeAPI;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.SplitPane;
import javafx.stage.Stage;

public class ErlyBerly extends Application {

	private static final NodeAPI nodeAPI = new NodeAPI();

	public static void main(String[] args) throws Exception {
		launch(args);
	}
	
    @Override
    public void start(Stage primaryStage) {
        
		SplitPane splitPane;
		
		splitPane = new SplitPane();
		splitPane.getItems().add(loadFxml("/erlyberly/entop.fxml"));
		splitPane.getItems().add(loadFxml("/erlyberly/dbg.fxml"));
		
        primaryStage.setScene(new Scene(splitPane, 500, 250));
        primaryStage.setTitle("erlyberly");
        primaryStage.show();
    }

	private Node loadFxml(String fxml) {
		URL location = getClass().getResource(fxml);
        FXMLLoader fxmlLoader = new FXMLLoader(location);

        Node node = null;
		try {
			node = (Node) fxmlLoader.load();
		} catch (IOException e) {
			throw new RuntimeException("Cannot load FXML");
		}
		return node;
	}

	public static NodeAPI nodeAPI() {
		return nodeAPI;
	}
}
