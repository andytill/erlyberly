/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 * <p>
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpEpmd;
import com.ericsson.otp.erlang.OtpErlangException;
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
import javafx.scene.control.*;
import javafx.scene.control.cell.CheckBoxTableCell;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.input.KeyCombination;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

import java.io.IOException;
import java.util.*;

/**
 * Connection details control to connect to the remote node.
 */
public class ConnectionView extends SplitPane {

    private final SimpleBooleanProperty isConnecting = new SimpleBooleanProperty();

    private final FloatyFieldControl nodeNameField;
    private final FloatyFieldControl cookieField;
    private final Label messageLabel;
    private final TableView<KnownNode> knownNodesTable;

    private final FloatyFieldControl filterField;

    private final FilteredList<KnownNode> filteredRows;

    private final ObservableList<KnownNode> rows;

    ConnectionView() {
        super();
        this.nodeNameField = new FloatyFieldControl();
        /**
         * Is there enough user input to connect to a node?
         */
        final SimpleBooleanProperty isNodeConnectable = new SimpleBooleanProperty();
        isNodeConnectable.bind(this.nodeNameField.getModel().textProperty().isNotEmpty());
        this.nodeNameField.getModel().promptTextProperty().set("Node Name");
        this.cookieField = new FloatyFieldControl();
        this.cookieField.getModel().promptTextProperty().set("Cookie");

        this.filterField = new FloatyFieldControl();
        this.filterField.getModel().promptTextProperty().set("Filter Known Nodes");
        this.filterField.getModel().textProperty().addListener((o) -> this.onFilterChanged());
        HBox.setHgrow(this.filterField, Priority.SOMETIMES);
        this.knownNodesTable = new TableView<>();
        this.knownNodesTable.getColumns().setAll(ConnectionView.newNodeRunningColumn("Is Running", "running", 75), ConnectionView.newColumn("Node Name", "nodeName", 200), ConnectionView.newColumn("Cookie", "cookie"));
        VBox.setVgrow(this.knownNodesTable, Priority.SOMETIMES);
        this.rows = FXCollections.observableArrayList(ConnectionView.knownNodesConfigToRowObjects(PrefBind.getKnownNodes()));
        this.filteredRows = new FilteredList<>(this.rows);

        final MenuItem deleteItem;
        deleteItem = new MenuItem("Delete");
        deleteItem.setOnAction(this::onDelete);
        deleteItem.setAccelerator(KeyCombination.keyCombination("delete"));
        final ContextMenu knownNodeMenu;
        knownNodeMenu = new ContextMenu();
        knownNodeMenu.getItems().add(deleteItem);

        this.knownNodesTable.setItems(this.filteredRows);
        this.knownNodesTable.getSelectionModel().selectedItemProperty().addListener((o, oldv, newv) -> {
            if (null == newv) return;
            this.nodeNameField.getModel().getField().setText(newv.getNodeName());
            this.cookieField.getModel().getField().setText(newv.getCookie());
        });
        this.knownNodesTable.setContextMenu(knownNodeMenu);

        // button width across the whole scene
        final double connectButtonHeight = 42d;
        final Button connectButton = new Button("Connect to Remote Node");
        connectButton.setPrefWidth(1000d);
        connectButton.setPrefHeight(connectButtonHeight);
        connectButton.setMinHeight(connectButtonHeight);
        connectButton.setOnAction(this::onConnectButtonPressed);
        connectButton.setDefaultButton(true);

        this.messageLabel = new Label();

        final VBox newNodeBox;
        newNodeBox = new VBox();
        newNodeBox.setSpacing(10d);
        newNodeBox.setPadding(new Insets(10, 10, 10, 10));
        newNodeBox.getChildren().addAll(this.nodeNameField, new Separator(Orientation.HORIZONTAL), this.cookieField, new Separator(Orientation.HORIZONTAL), connectButton, this.messageLabel);

        final VBox knownNodeBox;
        knownNodeBox = new VBox();
        knownNodeBox.setSpacing(10d);
        knownNodeBox.setPadding(new Insets(10, 10, 10, 10));
        knownNodeBox.getChildren().addAll(new HBox(this.filterField, ConnectionView.searchIcon()), this.knownNodesTable);
        this.getItems().addAll(knownNodeBox, newNodeBox);

        this.nodeNameField.getModel().getField().disableProperty().bind(this.isConnecting);
        this.cookieField.getModel().getField().disableProperty().bind(this.isConnecting);
        this.filterField.getModel().getField().disableProperty().bind(this.isConnecting);
        this.knownNodesTable.disableProperty().bind(this.isConnecting);
        connectButton.disableProperty().bind(this.isConnecting.or(isNodeConnectable.not()));
    }

    private void onDelete(final ActionEvent e) {
        final KnownNode selectedItem = this.knownNodesTable.getSelectionModel().getSelectedItem();
        if (null == selectedItem) return;
        this.rows.remove(selectedItem);
        PrefBind.removeKnownNode(selectedItem);
    }

    private static TableColumn<KnownNode, ?> newColumn(final String colText, final String colPropertyName, final double colWidth) {
        final TableColumn<KnownNode, String> col = ConnectionView.newColumn(colText, colPropertyName);
        col.setPrefWidth(colWidth);
        return col;
    }

    private static Glyph searchIcon() {
        final Glyph icon = new FontAwesome().create(FontAwesome.Glyph.SEARCH);
        icon.setStyle("-fx-font-family: FontAwesome; -fx-font-size: 1.8em; -fx-text-fill: gray;");
        // make the icon align with the input text, not with the height of the entire field control
        icon.setAlignment(Pos.BOTTOM_RIGHT);
        icon.setMaxHeight(Double.MAX_VALUE);
        return icon;
    }

    private void onFilterChanged() {
        final String filterString = this.filterField.getModel().getText();
        this.filteredRows.setPredicate((node) -> {
            if (null != filterString && filterString.isEmpty()) return true;
            assert null != filterString;
            return node.getNodeName().contains(filterString);
        });
    }

    private static List<KnownNode> knownNodesConfigToRowObjects(final List<List<String>> knownNodeStrings) {
        // get node names from epmd to give the user some options as to what to connect to
        final HashSet<String> runningNodeNames = new HashSet<>();
        try {
            runningNodeNames.addAll(Arrays.asList(OtpEpmd.lookupNames()));
        } catch (final IOException e) {
            e.printStackTrace();
        }
        // a lit of node names that are running according to epmd but do not appear in the list
        // of node names we have accessed before
        final List<String> unknownRunningNodeNames = new ArrayList<>(runningNodeNames);

        final List<KnownNode> knownNodeRows = new ArrayList<>();
        for (final List<String> confStrings : knownNodeStrings) {
            final KnownNode knownNode = new KnownNode(confStrings.get(0), confStrings.get(1));
            // if this configured node name is in the epmd list then mark it as "running"
            for (final String string : runningNodeNames) {
                if (string.contains(knownNode.getNodeName())) {
                    knownNode.setRunning(true);
                    unknownRunningNodeNames.remove(string);
                    break;
                }
            }
            knownNodeRows.add(knownNode);
        }
        for (final String runningNodeName : unknownRunningNodeNames) {
            // don't show erlyberly in the list, weird stuff happens if erlyberly connects to itself!
            if (runningNodeName.contains("erlyberly")) continue;
            // for some reason OtpEpmd gives node names in the following format:
            //     "name gerp at port 52153"
            // so we have to do some string cutting to get the proper name
            final String[] xxx = runningNodeName.split(" ");
            final KnownNode knownNode = new KnownNode(xxx[1], "");
            knownNode.setRunning(true);
            knownNodeRows.add(knownNode);
        }
        knownNodeRows.sort(Comparator.comparing(KnownNode::getNodeName));
        return knownNodeRows;
    }

    private static TableColumn<KnownNode, String> newColumn(final String colText, final String colPropertyName) {
        final TableColumn<KnownNode, String> column;
        column = new TableColumn<>(colText);
        column.setCellValueFactory(new PropertyValueFactory<>(colPropertyName));
        return column;
    }

    private static TableColumn<KnownNode, Boolean> newNodeRunningColumn(final String colText, final String colPropertyName, final double colWidth) {
        final TableColumn<KnownNode, Boolean> column;
        column = new TableColumn<>(colText);
        column.setCellValueFactory(node -> node.getValue().runningProperty());
        column.setCellFactory(tc -> new CheckBoxTableCell<>());
        column.setPrefWidth(colWidth);
        return column;
    }

    private void onConnectButtonPressed(final ActionEvent e) {
        final String cookie;
        final String nodeName;
        nodeName = this.nodeNameField.getModel().getText();
        cookie = ConnectionView.removeApostrophesFromCookie(this.cookieField.getModel().getText());
        if (null == nodeName || nodeName.isEmpty()) return;
        this.maybeStoreNodeInfoInConfig(nodeName, cookie);
        this.connectToRemoteNode(cookie, nodeName);
    }

    private void maybeStoreNodeInfoInConfig(final String nodeName, final String cookie) {
        final KnownNode knownNode = new KnownNode(nodeName, cookie);
        if (!this.knownNodesTable.getItems().contains(knownNode)) {
            PrefBind.storeKnownNode(knownNode);
        }
    }

    private void connectToRemoteNode(final String cookie, final String nodeName) {
        this.isConnecting.set(true);
        new ConnectorThead(nodeName, cookie).start();
        ErlyBerly.runIO(() -> {
            try {
                ErlyBerly.nodeAPI().connectionInfo(nodeName, cookie).manualConnect();

                Platform.runLater(this::closeThisWindow);
            } catch (final OtpErlangException | OtpAuthException | IOException e) {
                Platform.runLater(() -> this.connectionFailed(e.getMessage()));
            }
        });
    }

    private static String removeApostrophesFromCookie(final String cookie) {
        return cookie.replaceAll("'", "");
    }

    private void connectionFailed(final String message) {
        assert Platform.isFxApplicationThread();

        this.isConnecting.set(false);

        this.messageLabel.setGraphic(ConnectionView.bannedIcon());
        this.messageLabel.setText(message);
    }

    private static Glyph bannedIcon() {
        final Glyph icon = new FontAwesome().create(FontAwesome.Glyph.BAN);
        icon.setStyle("-fx-font-family: FontAwesome; -fx-font-size: 2em; -fx-text-fill: red;");
        return icon;
    }

    // TODO: make into a more generic stage handling function.
    private void closeThisWindow() {
        final Stage stage;
        stage = (Stage) this.getScene().getWindow();
        stage.close();
    }

    /**
     * Daemon thread used to connect to the remote node, so it doesn't block the UI.
     */
    class ConnectorThead extends Thread {
        private final String remoteNodeName;
        private final String cookie;

        ConnectorThead(final String aRemoteNodeName, final String aCookie) {
            super();
            this.remoteNodeName = aRemoteNodeName;
            this.cookie = aCookie;

            this.setDaemon(true);
            this.setName("Erlyberly Connector Thread");
        }

        @Override
        public void run() {
            try {
                ErlyBerly.nodeAPI().connectionInfo(this.remoteNodeName, this.cookie).manualConnect();

                Platform.runLater(ConnectionView.this::closeThisWindow);
            } catch (final OtpErlangException | OtpAuthException | IOException e) {
                Platform.runLater(() -> ConnectionView.this.connectionFailed(e.getMessage()));
            }
        }
    }

    /**
     * Type for table rows. Public because of restrictions using reflection.
     */
    public static class KnownNode {
        private final String nodeName, cookie;
        private final SimpleBooleanProperty running = new SimpleBooleanProperty(false);

        public KnownNode(final String nodeName, final String cookie) {
            super();
            assert null != nodeName;
            assert !nodeName.isEmpty();
            this.nodeName = nodeName;
            this.cookie = cookie;
        }

        public String getNodeName() {
            return this.nodeName;
        }

        public String getCookie() {
            return this.cookie;
        }

        public boolean isRunning() {
            return this.running.get();
        }

        public SimpleBooleanProperty runningProperty() {
            return this.running;
        }

        public void setRunning(final boolean running) {
            this.running.set(running);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((null == this.cookie) ? 0 : this.cookie.hashCode());
            result = prime * result + ((null == this.nodeName) ? 0 : this.nodeName.hashCode());
            return result;
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) return true;
            if (null == obj) return false;
            if (this.getClass() != obj.getClass()) return false;
            final KnownNode other = (KnownNode) obj;
            if (null == this.cookie) {
                if (null != other.cookie) return false;
            } else if (!this.cookie.equals(other.cookie)) return false;
            if (null == this.nodeName) {
                return null == other.nodeName;
            } else return this.nodeName.equals(other.nodeName);
        }
    }
}
