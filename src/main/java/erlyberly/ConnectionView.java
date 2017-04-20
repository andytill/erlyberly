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
import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpErlangException;

import de.jensd.fx.fontawesome.AwesomeIcon;
import floatyfield.FloatyFieldControl;
import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.control.Separator;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.ToggleGroup;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import ui.FAIcon;

/**
 * Connection details control to connect to the remote node.
 */
public class ConnectionView extends VBox {

    private final SimpleBooleanProperty isConnecting = new SimpleBooleanProperty();

    private FloatyFieldControl nodeNameField;
    private FloatyFieldControl cookieField;
    private final Button connectButton;
    private Label messageLabel;
    // private CheckBox autoConnectField;
    private final TableView<KnownNode> knownNodesTable;

    private RadioButton newNodeRadioButton;

    private RadioButton knownRadioButton;

    @SuppressWarnings("unchecked")
    public ConnectionView() {
        setSpacing(10d);
        setPadding(new Insets(10, 10, 10, 10));

        ToggleGroup group = new ToggleGroup();
        newNodeRadioButton = new RadioButton("New node");
        knownRadioButton = new RadioButton("Known nodes");
        // TODO put some style on these radio buttons so that they look like section headers
        newNodeRadioButton.setToggleGroup(group);
        knownRadioButton.setToggleGroup(group);
        newNodeRadioButton.setSelected(true);

        nodeNameField = new FloatyFieldControl();
        nodeNameField.getModel().promptTextProperty().set("Node Name");
        nodeNameField.getModel().getField().focusedProperty().addListener((o, oldv, newv) -> {
            if(newv) {
                newNodeRadioButton.setSelected(true);
            }
        });
        cookieField = new FloatyFieldControl();
        cookieField.getModel().promptTextProperty().set("Cookie");
        cookieField.getModel().getField().focusedProperty().addListener((o, oldv, newv) -> {
            if(newv) {
                newNodeRadioButton.setSelected(true);
            }
        });

        knownNodesTable = new TableView<>();
        VBox.setVgrow(knownNodesTable, Priority.SOMETIMES);
        knownNodesTable.getColumns().setAll(
            newColumn("Node Name", "nodeName"),
            newColumn("Cookie", "cookie")
        );
        knownNodesTable.getSelectionModel().selectedItemProperty().addListener(
            (o, oldv, newv) -> {
                if(newv != null) {
                    knownRadioButton.setSelected(true);
                }
            });
        knownNodesTable.getItems().addAll(knownNodesConfigToRowObjects(PrefBind.getKnownNodes()));

        // button width across the whole scene
        final double connectButtonHeight = 42d;
        connectButton = new Button("Connect");
        connectButton.setPrefWidth(1000d);
        connectButton.setPrefHeight(connectButtonHeight);
        connectButton.setMinHeight(connectButtonHeight);
        connectButton.setOnAction(this::onConnectButtonPressed);

        getChildren().addAll(
            newNodeRadioButton,
            nodeNameField, new Separator(),
            cookieField, new Separator(),
            knownRadioButton,
            knownNodesTable,
            connectButton);

/*
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
        }
        else {
            // show the Connection Dialogue...
        }*/
    }

    private List<KnownNode> knownNodesConfigToRowObjects(List<List<String>> knownNodeStrings) {
        ArrayList<KnownNode> knownNodeRows = new ArrayList<>();
        for (List<String> confStrings : knownNodeStrings) {
            knownNodeRows.add(
                new KnownNode(confStrings.get(0), confStrings.get(1))
            );
        }
        return knownNodeRows;
    }

    private TableColumn<KnownNode, String> newColumn(String colText, String colPropertyName) {
        TableColumn<KnownNode, String> column;
        column = new TableColumn<>(colText);
        column.setCellValueFactory(new PropertyValueFactory<KnownNode, String>(colPropertyName));
        return column;
    }

    private void onConnectButtonPressed(ActionEvent e) {
        String cookie, nodeName;
        if(newNodeRadioButton.isSelected()) {
            nodeName = nodeNameField.getModel().getText();
            cookie = removeApostrophesFromCookie(cookieField.getModel().getText());
            if(nodeName == null || "".equals(nodeName))
                return;
            maybeStoreNodeInfoInConfig(nodeName, cookie);
            connectToRemoteNode(cookie, nodeName);
        }
        else if(knownRadioButton.isSelected()) {
            KnownNode knownNode = knownNodesTable.getSelectionModel().getSelectedItem();
            if(knownNode == null)
                return;
            nodeName = knownNode.getNodeName();
            cookie = removeApostrophesFromCookie(knownNode.getCookie());
            connectToRemoteNode(cookie, nodeName);
        }
    }

    private void maybeStoreNodeInfoInConfig(String nodeName, String cookie) {
       KnownNode knownNode = new KnownNode(nodeName, cookie);
       if(!knownNodesTable.getItems().contains(knownNode)) {
           PrefBind.storeKnownNode(knownNode);
       }
    }

    private void connectToRemoteNode(String cookie, String nodeName) {
        isConnecting.set(true);
        new ConnectorThead(nodeName, cookie).start();
    }

    private String removeApostrophesFromCookie(String cookie) {
        return cookie.replaceAll("'", "");
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
                    .manualConnect();

                Platform.runLater(() -> { closeThisWindow(); });
            }
            catch (OtpErlangException | OtpAuthException | IOException e) {
                Platform.runLater(() -> { connectionFailed(e.getMessage()); });
            }
        }
    }

    /**
     * Type for table rows. Public because of restrictions using reflection.
     */
    public static class KnownNode {
        private final String nodeName, cookie;
        public KnownNode(String nodeName, String cookie) {
            assert nodeName != null;
            assert !"".equals(nodeName);
            this.nodeName = nodeName;
            this.cookie = cookie;
        }
        public String getNodeName() {
            return nodeName;
        }
        public String getCookie() {
            return cookie;
        }
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((cookie == null) ? 0 : cookie.hashCode());
            result = prime * result + ((nodeName == null) ? 0 : nodeName.hashCode());
            return result;
        }
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            KnownNode other = (KnownNode) obj;
            if (cookie == null) {
                if (other.cookie != null)
                    return false;
            } else if (!cookie.equals(other.cookie))
                return false;
            if (nodeName == null) {
                if (other.nodeName != null)
                    return false;
            } else if (!nodeName.equals(other.nodeName))
                return false;
            return true;
        }
    }
}
