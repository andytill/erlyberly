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
import javafx.scene.control.SplitPane.Divider;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import erlyberly.node.NodeAPI;

public class ErlyBerly extends Application {

	private static final NodeAPI nodeAPI = new NodeAPI();

	private SplitPane splitPane;

	private Region entopPane;

	private double entopDivPosition;

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

		FxmlLoadable topBarFxml;
		
		topBarFxml = new FxmlLoadable("/erlyberly/topbar.fxml");
		topBarFxml.load();
		
		FxmlLoadable dbgFxml;
		
		dbgFxml = new FxmlLoadable("/erlyberly/dbg.fxml");
		dbgFxml.load();
        
		splitPane = new SplitPane();
		
		entopPane = (Region) loadEntopPane();
		splitPane.getItems().add(entopPane);
		
		splitPane.getItems().add(dbgFxml.load());
		
		setupProcPaneHiding(topBarFxml, dbgFxml);

		VBox rootView = new VBox(topBarFxml.load(), splitPane);
		
        Scene scene;
        
		scene = new Scene(rootView);
		scene.getStylesheets().add(getClass().getResource("/erlyberly/erlyberly.css").toString());  
		
		primaryStage.setScene(scene);
        primaryStage.titleProperty().bind(nodeAPI.summaryProperty());
        primaryStage.sizeToScene();
        primaryStage.show();
        
        displayConnectionPopup(primaryStage);
    }

	private void setupProcPaneHiding(FxmlLoadable topBarFxml, FxmlLoadable dbgFxml) {
		TopBarView topView;
		DbgView dbgView;
		
		topView = (TopBarView)topBarFxml.controller;
		dbgView = (DbgView) dbgFxml.controller;
		
		topView.hideProcsProperty().addListener((ObservableValue<? extends Boolean> o, Boolean ob, Boolean nb) -> {
			if(nb) {
				splitPane.getItems().add(0, entopPane);
				
				Divider div = splitPane.getDividers().get(0);
				div.setPosition(entopDivPosition);
			}
			else {
				Divider div = splitPane.getDividers().get(0);

				entopDivPosition = div.getPosition();
				
				div.setPosition(0d);
				splitPane.getItems().remove(0);
			}
		});
		
		topView.hideFunctionsProperty().addListener((ObservableValue<? extends Boolean> o, Boolean ob, Boolean nb) -> {
			dbgView.setFunctionsVisibility(nb);
		});
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
			if(fxmlNode != null)
				return fxmlNode;
			
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
