package erlyberly;

import java.io.IOException;
import java.net.URL;

import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;

class FxmlLoadable {
	final String resource;
	
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
		}
		catch (IOException e) {
			throw new RuntimeException("Cannot load FXML", e);
		}
		return fxmlNode;
	}
}