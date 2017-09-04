/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpErlangException;

import de.jensd.fx.fontawesome.AwesomeIcon;
import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Stage;
import ui.FAIcon;

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
        final String cookie, remoteNodeName;
        cookie = cookieField.getText().replaceAll("'", "");
        remoteNodeName = nodeNameField.getText();

        isConnecting.set(true);
        ErlyBerly.runIO(() -> {
            try {
                ErlyBerly
                    .nodeAPI()
                    .connectionInfo(remoteNodeName, cookie)
                    .manualConnect();

                Platform.runLater(() -> { closeThisWindow(); });
            }
            catch (OtpErlangException | OtpAuthException | IOException e) {
                // e.printStackTrace();
                Platform.runLater(() -> { connectionFailed(e.getMessage()); });
            }
        });
    }

    private void connectionFailed(String message) {
        assert Platform.isFxApplicationThread();

        isConnecting.set(false);

        messageLabel.setGraphic(bannedIcon());
        messageLabel.setText(message);
    }

    private FAIcon bannedIcon() {
        return FAIcon.create()
                .icon(AwesomeIcon.BAN)
                .style("-fx-font-family: FontAwesome; -fx-font-size: 2em; -fx-text-fill: red;");
    }

    // TODO: make into a more generic stage handling function.
    private void closeThisWindow() {
        Stage stage;
        stage = (Stage) connectButton.getScene().getWindow();
        stage.close();
    }
}
