package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Stage;
import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;

/**
 * Connection details control to connect to the remote node. 
 */
public class ConnectionView implements Initializable {

	@FXML
	private TextField nodeNameField;
	@FXML
	private TextField cookieField;
	@FXML
	private Button connectButton;
	@FXML
	private Label messageLabel;

	@Override
	public void initialize(URL url, ResourceBundle r) {
		PrefBind.bind("targetNodeName", nodeNameField.textProperty());
		PrefBind.bind("cookieName", cookieField.textProperty());
		
		// close this window when we're connected
		ErlyBerly.nodeAPI().connectedProperty().addListener(new InvalidationListener() {
			@Override
			public void invalidated(Observable o) {
				if(!ErlyBerly.nodeAPI().connectedProperty().get())
					return;
					
				ErlyBerly.nodeAPI().connectedProperty().removeListener(this);
				
				closeThisWindow();
			}});
	}
	
	@FXML
	public void onConnect() {
		try {
			ErlyBerly
				.nodeAPI()
				.connectionInfo(nodeNameField.getText(), cookieField.getText())
				.connect();
		} 
		catch (Exception e) {
			messageLabel.setGraphic(bannedIcon());
			messageLabel.setText(e.getMessage());
		}
	}

	private Icon bannedIcon() {
		return Icon.create()
				.icon(AwesomeIcon.BAN)
				.style("-fx-font-family: FontAwesome; -fx-font-size: 2em; -fx-text-fill: red;");
	}

	private void closeThisWindow() {
		Stage stage;
		
		stage = (Stage) connectButton.getScene().getWindow();
		stage.close();
	}
}
