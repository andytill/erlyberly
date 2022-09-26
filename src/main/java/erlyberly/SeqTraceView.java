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

    public SeqTraceView(final ObservableList<SeqTraceLog> seqTraceLogs) {
        super();
        this.table = new TableView<>();
        this.table.setOnMouseClicked(this::onTraceClicked);
        this.table.setMaxHeight(Integer.MAX_VALUE);
        VBox.setVgrow(this.table, Priority.ALWAYS);

        this.setupTableColumns();

        this.getChildren().add(this.table);

        seqTraceLogs.addListener(this::traceLogsChanged);
    }

    private void setupTableColumns() {
        final TableColumn<SeqTraceLog, String> msgType;
        final TableColumn<SeqTraceLog, String> serial;
        final TableColumn<SeqTraceLog, String> from;
        final TableColumn<SeqTraceLog, String> to;
        final TableColumn<SeqTraceLog, String> message;
        final TableColumn<SeqTraceLog, String> timestamp;

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

        this.table.getColumns().addAll(msgType, timestamp, serial, from, to, message);
    }

    private void traceLogsChanged(final ListChangeListener.Change<? extends SeqTraceLog> e) {
        while (e.next()) {
            for (final SeqTraceLog trace : e.getAddedSubList()) {
                this.table.getItems().add(trace);
            }
        }
    }

    private void onTraceClicked(final MouseEvent me) {
        if (MouseButton.PRIMARY == me.getButton()) {
            if (2 == me.getClickCount()) {
                final SeqTraceLog selectedItem = this.table.getSelectionModel().getSelectedItem();

                if (null != selectedItem) {
                    SeqTraceView.showTraceTermView(selectedItem);
                }
            }
        }
    }

    private static void showTraceTermView(final SeqTraceLog seqTraceLog) {
        final TermTreeView argTermsTreeView;

        argTermsTreeView = SeqTraceView.newTermTreeView();
        argTermsTreeView.populateFromTerm(seqTraceLog.getMessage());

        ErlyBerly.showPane("Seq Trace", ErlyBerly.wrapInPane(argTermsTreeView));
    }

    private static TermTreeView newTermTreeView() {
        final TermTreeView termTreeView;

        termTreeView = new TermTreeView();
        termTreeView.setMaxHeight(Integer.MAX_VALUE);
        VBox.setVgrow(termTreeView, Priority.ALWAYS);

        return termTreeView;
    }
}
