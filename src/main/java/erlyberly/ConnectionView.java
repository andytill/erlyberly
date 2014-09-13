package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

public class ConnectionView implements Initializable {

	@FXML
	private TextField nodeNameField;
	@FXML
	private TextField cookieField;
	@FXML
	private Button connectButton;
	
	@FXML
	public void onConnect() {
		ErlyBerly.nodeAPI().connect(nodeNameField.getText(), cookieField.getText());
	}

	@Override
	public void initialize(URL url, ResourceBundle r) {
		// close this window when we're connected
		ErlyBerly.nodeAPI().connectedProperty().addListener(new InvalidationListener() {
			@Override
			public void invalidated(Observable o) {
				if(!ErlyBerly.nodeAPI().connectedProperty().get())
					return;
					
				ErlyBerly.nodeAPI().connectedProperty().removeListener(this);
				
				Stage stage;
				
				stage = (Stage) connectButton.getScene().getWindow();
				stage.close();
			}});
	}
}
