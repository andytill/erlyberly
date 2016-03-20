package erlyberly;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.control.CheckBox;
import javafx.stage.Stage;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpErlangException;

import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;

/**
 * Connection details control to connect to the remote node. 
 */
public class ConnectionView implements Initializable {
    
    private final SimpleBooleanProperty isConnecting = new SimpleBooleanProperty();

    @FXML
    private TextField nodeNameField;
    @FXML
    private TextField cookieField;
    @FXML
    private Button connectButton;
    @FXML
    private Label messageLabel;
    @FXML
    private CheckBox autoConnectField;

    @Override
    public void initialize(URL url, ResourceBundle r) {
        PrefBind.bind("targetNodeName", nodeNameField.textProperty());
        PrefBind.bind("cookieName", cookieField.textProperty());
        PrefBind.bindBoolean("autoConnect", autoConnectField.selectedProperty());
        
        nodeNameField.disableProperty().bind(isConnecting);
        cookieField.disableProperty().bind(isConnecting);
        connectButton.disableProperty().bind(isConnecting);
        autoConnectField.disableProperty().bind(isConnecting);
        
        // TODO: or immediately start connecting when the CMDLINE flag is present...
        if (autoConnectField.isSelected() && !ErlyBerly.nodeAPI().manuallyDisconnected()) {
            try {
                // TODO: This sleep/yield, allows the proc controller to start it's thread,
                // and to prevent a npointer exception.
                Thread.sleep(50);
            } catch(InterruptedException ex) {
                Thread.currentThread().interrupt();
            }
            
            this.onConnect();
        }else{
            // show the Connection Dialogue...
        }
    }
    
    @FXML
    public void onConnect() {
        String cookie;
        
        cookie = cookieField.getText();
        cookie = cookie.replaceAll("'", "");
        
        isConnecting.set(true);
        
        new ConnectorThead(nodeNameField.getText(), cookie).start();
    }
    
    private void connectionFailed(String message) {
        assert Platform.isFxApplicationThread();
        
        isConnecting.set(false);
        
        messageLabel.setGraphic(bannedIcon());
        messageLabel.setText(message);
    }

    private Icon bannedIcon() {
        return Icon.create()
                .icon(AwesomeIcon.BAN)
                .style("-fx-font-family: FontAwesome; -fx-font-size: 2em; -fx-text-fill: red;");
    }
    
    // TODO: make into a more generic stage handling function.
    private void closeThisWindow() {
        Stage stage;    
        stage = (Stage) connectButton.getScene().getWindow();
        stage.close();
    }
    
    /**
     * Daemon thread used to connect to the remote node, so it doesn't block the UI. 
     */
    class ConnectorThead extends Thread {
        private final String remoteNodeName;
        private final String cookie;

        public ConnectorThead(String aRemoteNodeName, String aCookie) {
            remoteNodeName = aRemoteNodeName;
            cookie = aCookie;
            
            setDaemon(true);
            setName("Erlyberly Connector Thread");
        }
        
        @Override
        public void run() {
            try {
                ErlyBerly
                    .nodeAPI()
                    .connectionInfo(remoteNodeName, cookie)
                    .manual_connect();

                Platform.runLater(() -> { closeThisWindow(); });
            }
            catch (OtpErlangException | OtpAuthException | IOException e) {
                Platform.runLater(() -> { connectionFailed(e.getMessage()); });
            }
        }
    }
}