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
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpEpmd;
import com.ericsson.otp.erlang.OtpErlangException;

import de.jensd.fx.fontawesome.AwesomeIcon;
import floatyfield.FloatyFieldControl;
import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.Separator;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.CheckBoxTableCell;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.input.KeyCombination;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import ui.FAIcon;

/**
 * Connection details control to connect to the remote node.
 */
@SuppressWarnings({ "unchecked" })
public class ConnectionView extends SplitPane {

    private final SimpleBooleanProperty isConnecting = new SimpleBooleanProperty();

    /**
     * Is there enough user input to connect to a node?
     */
    private final SimpleBooleanProperty isNodeConnectable = new SimpleBooleanProperty();

    private FloatyFieldControl nodeNameField;
    private FloatyFieldControl cookieField;
    private Label messageLabel;
    private final TableView<KnownNode> knownNodesTable;

    private FloatyFieldControl filterField;

    private FilteredList<KnownNode> filteredRows;

    private Button connectButton;

    private ObservableList<KnownNode> rows;

    public ConnectionView() {
        nodeNameField = new FloatyFieldControl();
        isNodeConnectable.bind(nodeNameField.getModel().textProperty().isNotEmpty());
        nodeNameField.getModel().promptTextProperty().set("Node Name");
        cookieField = new FloatyFieldControl();
        cookieField.getModel().promptTextProperty().set("Cookie");

        filterField = new FloatyFieldControl();
        filterField.getModel().promptTextProperty().set("Filter Known Nodes");
        filterField.getModel().textProperty().addListener((o) -> { onFilterChanged(); });
        HBox.setHgrow(filterField, Priority.SOMETIMES);
        knownNodesTable = new TableView<>();
        knownNodesTable.getColumns().setAll(
            newNodeRunningColumn("Is Running", "running", 75),
            newColumn("Node Name", "nodeName", 200),
            newColumn("Cookie", "cookie")
        );
        VBox.setVgrow(knownNodesTable, Priority.SOMETIMES);
        rows = FXCollections.observableArrayList(knownNodesConfigToRowObjects(PrefBind.getKnownNodes()));
        filteredRows = new FilteredList<KnownNode>(rows);

        MenuItem deleteItem;
        deleteItem = new MenuItem("Delete");
        deleteItem.setOnAction(this::onDelete);
        deleteItem.setAccelerator(KeyCombination.keyCombination("delete"));
        ContextMenu knownNodeMenu;
        knownNodeMenu = new ContextMenu();
        knownNodeMenu.getItems().add(deleteItem);

        knownNodesTable.setItems(filteredRows);
        knownNodesTable.getSelectionModel().selectedItemProperty().addListener(
                (o, oldv, newv) -> {
                    if(newv == null)
                        return;
                    nodeNameField.getModel().getField().setText(newv.getNodeName());
                    cookieField.getModel().getField().setText(newv.getCookie());
                });
        knownNodesTable.setContextMenu(knownNodeMenu);

        // button width across the whole scene
        final double connectButtonHeight = 42d;
        connectButton = new Button("Connect to Remote Node");
        connectButton.setPrefWidth(1000d);
        connectButton.setPrefHeight(connectButtonHeight);
        connectButton.setMinHeight(connectButtonHeight);
        connectButton.setOnAction(this::onConnectButtonPressed);
        connectButton.setDefaultButton(true);

        messageLabel = new Label();

        VBox newNodeBox;
        newNodeBox = new VBox();
        newNodeBox.setSpacing(10d);
        newNodeBox.setPadding(new Insets(10, 10, 10, 10));
        newNodeBox.getChildren().addAll(
                nodeNameField, new Separator(Orientation.HORIZONTAL),
                cookieField, new Separator(Orientation.HORIZONTAL),
                connectButton, messageLabel);

        VBox knownNodeBox;
        knownNodeBox = new VBox();
        knownNodeBox.setSpacing(10d);
        knownNodeBox.setPadding(new Insets(10, 10, 10, 10));
        knownNodeBox.getChildren().addAll(
            new HBox(
                filterField,
                searchIcon()),
            knownNodesTable);
        getItems().addAll(knownNodeBox, newNodeBox);

        nodeNameField.getModel().getField().disableProperty().bind(isConnecting);
        cookieField.getModel().getField().disableProperty().bind(isConnecting);
        filterField.getModel().getField().disableProperty().bind(isConnecting);
        knownNodesTable.disableProperty().bind(isConnecting);
        connectButton.disableProperty().bind(isConnecting.or(isNodeConnectable.not()));
    }

    private void onDelete(ActionEvent e) {
        KnownNode selectedItem = knownNodesTable.getSelectionModel().getSelectedItem();
        if(selectedItem == null)
            return;
        rows.remove(selectedItem);
        PrefBind.removeKnownNode(selectedItem);
    }

    private TableColumn<KnownNode, ?> newColumn(String colText, String colPropertyName, double colWidth) {
        TableColumn<KnownNode, String> col = newColumn(colText, colPropertyName);
        col.setPrefWidth(colWidth);
        return col;
    }

    private FAIcon searchIcon() {
        FAIcon icon = FAIcon.create()
                .icon(AwesomeIcon.SEARCH)
                .style("-fx-font-family: FontAwesome; -fx-font-size: 1.8em; -fx-text-fill: gray;");
        // make the icon align with the input text, not with the height of the entire field control
        icon.setAlignment(Pos.BOTTOM_RIGHT);
        icon.setMaxHeight(Double.MAX_VALUE);
        return icon;
    }

    private void onFilterChanged() {
        final String filterString = filterField.getModel().getText();
        filteredRows.setPredicate((node) -> {
            return "".equals(filterString) || node.getNodeName().contains(filterString);
        });
    }

    private List<KnownNode> knownNodesConfigToRowObjects(List<List<String>> knownNodeStrings) {
        // get node names from epmd to give the user some options as to what to connect to
        HashSet<String> hashSet = new HashSet<>();
        try {
            hashSet.addAll(Arrays.asList(OtpEpmd.lookupNames()));
        }
        catch (IOException e) {
            e.printStackTrace();
        }

        ArrayList<KnownNode> knownNodeRows = new ArrayList<>();
        for (List<String> confStrings : knownNodeStrings) {
            KnownNode knownNode = new KnownNode(confStrings.get(0), confStrings.get(1));
            // if this configured node name is in the epmd list then mark it as "running"
            for (String string : hashSet) {
                if(string.contains(knownNode.getNodeName())) {
                    knownNode.setRunning(true);
                    hashSet.remove(string); // concurrent mod exception?
                    break;
                }
            }
            knownNodeRows.add(knownNode);
        }
        for (String runningNodeName : hashSet) {
            // don't show erlyberly in the list, weird stuff happens if erlyberly connects to itself!
            if(runningNodeName.contains("erlyberly"))
                continue;
            // for some reason OtpEpmd gives node names in the following format:
            //     "name gerp at port 52153"
            // so we have to do some string cutting to get the proper name
            String[] xxx = runningNodeName.split(" ");
            KnownNode knownNode = new KnownNode(xxx[1], "");
            knownNode.setRunning(true);
            knownNodeRows.add(knownNode);
        }
        Collections.sort(knownNodeRows, (a, b) -> {
            return a.getNodeName().compareTo(b.getNodeName());
        });
        return knownNodeRows;
    }

    private TableColumn<KnownNode, String> newColumn(String colText, String colPropertyName) {
        TableColumn<KnownNode, String> column;
        column = new TableColumn<>(colText);
        column.setCellValueFactory(new PropertyValueFactory<KnownNode, String>(colPropertyName));
        return column;
    }

    private TableColumn<KnownNode, Boolean> newNodeRunningColumn(String colText, String colPropertyName, double colWidth) {
        TableColumn<KnownNode, Boolean> column;
        column = new TableColumn<>(colText);
        column.setCellValueFactory( node -> { return node.getValue().runningProperty(); });
        column.setCellFactory( tc -> new CheckBoxTableCell<>());
        column.setPrefWidth(colWidth);
        return column;
    }

    private void onConnectButtonPressed(ActionEvent e) {
        String cookie, nodeName;
        nodeName = nodeNameField.getModel().getText();
        cookie = removeApostrophesFromCookie(cookieField.getModel().getText());
        if(nodeName == null || "".equals(nodeName))
            return;
        maybeStoreNodeInfoInConfig(nodeName, cookie);
        connectToRemoteNode(cookie, nodeName);
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
        ErlyBerly.runIO(() -> {
            try {
                ErlyBerly
                    .nodeAPI()
                    .connectionInfo(nodeName, cookie)
                    .manualConnect();

                Platform.runLater(() -> { closeThisWindow(); });
            }
            catch (OtpErlangException | OtpAuthException | IOException e) {
                Platform.runLater(() -> { connectionFailed(e.getMessage()); });
            }
        });
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
        stage = (Stage) this.getScene().getWindow();
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
        private SimpleBooleanProperty running = new SimpleBooleanProperty(false);
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
        public boolean isRunning() {
            return running.get();
        }
        public SimpleBooleanProperty runningProperty() {
            return running;
        }
        public void setRunning(boolean running) {
            this.running.set(running);
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
