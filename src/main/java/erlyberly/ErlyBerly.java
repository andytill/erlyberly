package erlyberly;

import java.io.IOException;
import java.net.URL;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.event.EventHandler;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.SplitPane;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import de.jensd.shichimifx.utils.SplitPaneDividerSlider;
import de.jensd.shichimifx.utils.SplitPaneDividerSlider.Direction;
import erlyberly.node.NodeAPI;

public class ErlyBerly extends Application {

	private static final NodeAPI nodeAPI = new NodeAPI();

	private SplitPaneDividerSlider divider;

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

		FxmlLoadable dbgFxml = new FxmlLoadable("/erlyberly/dbg.fxml");
		
		splitPane.getItems().add(loadEntopPane());
		splitPane.getItems().add(dbgFxml.load());
		
		divider = new SplitPaneDividerSlider(splitPane, 0, Direction.LEFT);
		
		setupProcPaneHiding(dbgFxml);
		
        primaryStage.setScene(new Scene(splitPane));
        primaryStage.setTitle("erlyberly");
        primaryStage.sizeToScene();
        primaryStage.show();
        
        displayConnectionPopup(primaryStage);
    }

	private void setupProcPaneHiding(FxmlLoadable dbgFxml) {
		DbgView dbgView;
		
		dbgView = (DbgView)dbgFxml.controller;
		dbgView.hideProcsProperty().addListener((ObservableValue<? extends Boolean> ov, Boolean t, Boolean t1) -> {
			divider.setAimContentVisible(t1);
		});

		// initialise
		divider.setAimContentVisible(dbgView.hideProcsProperty().get());
	}

	private Parent loadEntopPane() {
		Parent entopPane = new FxmlLoadable("/erlyberly/entop.fxml").load();
        SplitPane.setResizableWithParent(entopPane, Boolean.FALSE);
        
		return entopPane;
	}

	private void displayConnectionPopup(Stage primaryStage) {
		Stage connectStage;
        
		connectStage = new Stage();
        connectStage.initModality(Modality.WINDOW_MODAL);
        connectStage.initOwner(primaryStage.getScene().getWindow());
        connectStage.setScene(new Scene(new FxmlLoadable("/erlyberly/connection.fxml").load()));

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

	public static NodeAPI nodeAPI() {
		return nodeAPI;
	}
	
	static class FxmlLoadable {
		String resource;
		Parent fxmlNode;
		Object controller;

		public FxmlLoadable(String aResource) {
			resource = aResource;
		}

		public Parent load() {
			URL location = getClass().getResource(resource);
	        FXMLLoader fxmlLoader = new FXMLLoader(location);

			try {
				fxmlNode = (Parent) fxmlLoader.load();
				controller = fxmlLoader.getController();
			} catch (IOException e) {
				throw new RuntimeException("Cannot load FXML");
			}
			return fxmlNode;
		}
	}
}
