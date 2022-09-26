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

import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

public class SeqTraceView extends VBox {

    private final TableView<SeqTraceLog> table;

    public SeqTraceView(ObservableList<SeqTraceLog> seqTraceLogs) {
        table = new TableView<SeqTraceLog>();
        table.setOnMouseClicked(this::onTraceClicked);
        table.setMaxHeight(Integer.MAX_VALUE);
        VBox.setVgrow(table, Priority.ALWAYS);

        setupTableColumns();

        getChildren().add(table);

        seqTraceLogs.addListener(this::traceLogsChanged);
    }

    @SuppressWarnings("unchecked")
    private void setupTableColumns() {
        TableColumn<SeqTraceLog, String> msgType, serial, from, to, message, timestamp;

        msgType = new TableColumn<>("Msg Type");
        msgType.setCellValueFactory(new PropertyValueFactory<>("msgType"));

        timestamp = new TableColumn<>("Timestamp");
        timestamp.setCellValueFactory(new PropertyValueFactory<>("timestamp"));
        timestamp.setPrefWidth(150d);

        serial = new TableColumn<>("Serial");
        serial.setCellValueFactory(new PropertyValueFactory<>("serial"));

        from = new TableColumn<>("From");
        from.setCellValueFactory(new PropertyValueFactory<>("from"));

        to = new TableColumn<>("To");
        to.setCellValueFactory(new PropertyValueFactory<>("to"));

        // the message column should take the remaining space
        // TODO http://stackoverflow.com/questions/30288558/make-the-last-column-in-a-javafx-tableview-take-the-remaining-space
        message = new TableColumn<>("Message");
        message.setCellValueFactory(new PropertyValueFactory<>("message"));
        message.setPrefWidth(700d);

        table.getColumns().addAll(msgType, timestamp, serial, from, to, message);
    }

    private void traceLogsChanged(ListChangeListener.Change<? extends SeqTraceLog> e) {
        while (e.next()) {
            for (SeqTraceLog trace : e.getAddedSubList()) {
                table.getItems().add(trace);
            }
        }
    }

    private void onTraceClicked(MouseEvent me) {
        if (me.getButton().equals(MouseButton.PRIMARY)) {
            if (me.getClickCount() == 2) {
                SeqTraceLog selectedItem = table.getSelectionModel().getSelectedItem();

                if (selectedItem != null) {
                    showTraceTermView(selectedItem);
                }
            }
        }
    }

    private void showTraceTermView(final SeqTraceLog seqTraceLog) {
        TermTreeView argTermsTreeView;

        argTermsTreeView = newTermTreeView();
        argTermsTreeView.populateFromTerm(seqTraceLog.getMessage());

        ErlyBerly.showPane("Seq Trace", ErlyBerly.wrapInPane(argTermsTreeView));
    }

    private TermTreeView newTermTreeView() {
        TermTreeView termTreeView;

        termTreeView = new TermTreeView();
        termTreeView.setMaxHeight(Integer.MAX_VALUE);
        VBox.setVgrow(termTreeView, Priority.ALWAYS);

        return termTreeView;
    }
}
