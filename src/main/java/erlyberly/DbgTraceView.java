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

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import floatyfield.FloatyFieldView;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.css.PseudoClass;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.Control;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.control.TitledPane;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;


public class DbgTraceView extends VBox {

    private final ObservableList<TraceLog> traceLogs = FXCollections.observableArrayList();

    private final SortedList<TraceLog> sortedTtraces = new SortedList<TraceLog>(traceLogs);

    private final FilteredList<TraceLog> filteredTraces = new FilteredList<TraceLog>(sortedTtraces);

    private final TableView<TraceLog> tracesBox;

    /**
     * Set insertTracesAtTop=true in the .erlyberly file in your home directory to
     * make traces be inserted at the top of the list.
     */
    private boolean insertTracesAtTop;

    public DbgTraceView(DbgController dbgController) {
        setSpacing(5d);
        setStyle("-fx-background-insets: 5;");
        setMaxHeight(Double.MAX_VALUE);

        insertTracesAtTop = PrefBind.getOrDefaultBoolean("insertTracesAtTop", false);

        tracesBox = new TableView<TraceLog>();
        tracesBox.getStyleClass().add("trace-log-table");
        tracesBox.setOnMouseClicked(this::onTraceClicked);
        tracesBox.setMaxHeight(Double.MAX_VALUE);
        VBox.setVgrow(tracesBox, Priority.ALWAYS);

        putTableColumns();

        // #47 double wrapping the filtered list in another sorted list, otherwise
        // the table cannot be sorted on columns. Binding the items will throw exceptions
        // when sorting on columns.
        // see http://code.makery.ch/blog/javafx-8-tableview-sorting-filtering/
        SortedList<TraceLog> sortedData = new SortedList<>(filteredTraces);
        sortedData.comparatorProperty().bind(tracesBox.comparatorProperty());
        tracesBox.setItems(sortedData);

        putTraceContextMenu();

        Parent p = traceLogFloatySearchControl();

        getChildren().addAll(p, tracesBox);

        dbgController.getTraceLogs().addListener(this::traceLogsChanged);

        ErlyBerly.nodeAPI().connectedProperty().addListener(
            (o, oldV, newV) -> {
                if(oldV && !newV) {
                    traceLogs.add(TraceLog.newNodeDown());
                }
            });
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    private void putTableColumns() {
        TableColumn<TraceLog,Long> seqColumn;
        seqColumn = new TableColumn<TraceLog,Long>("Seq.");
        seqColumn.setCellValueFactory(new PropertyValueFactory("instanceNum"));
        configureColumnWidth("seqColumnWidth", seqColumn);

        TableColumn<TraceLog,String> pidColumn;
        pidColumn = new TableColumn<TraceLog,String>("Pid");
        pidColumn.setCellValueFactory(new PropertyValueFactory("pid"));
        configureColumnWidth("pidColumnWidth", pidColumn);

        TableColumn<TraceLog,String> regNameColumn;
        regNameColumn = new TableColumn<TraceLog,String>("Reg. Name");
        regNameColumn.setCellValueFactory(new PropertyValueFactory("regName"));
        configureColumnWidth("regNameColumnWidth", regNameColumn);

        TableColumn<TraceLog,String> durationNameColumn;
        durationNameColumn = new TableColumn<TraceLog,String>("Duration (microseconds)");
        durationNameColumn.setCellValueFactory(new PropertyValueFactory("duration"));
        configureColumnWidth("durationNameColumnWidth", durationNameColumn);

        TableColumn<TraceLog,String> functionNameColumn;
        functionNameColumn = new TableColumn<TraceLog,String>("Function");
        functionNameColumn.setCellValueFactory(new PropertyValueFactory("function"));
        configureColumnWidth("functionNameColumnWidth", functionNameColumn);

        TableColumn<TraceLog,String> argsColumn;
        argsColumn = new TableColumn<TraceLog,String>("Args");
        argsColumn.setCellValueFactory(new PropertyValueFactory("args"));
        configureColumnWidth("argsColumnWidth", argsColumn);

        TableColumn<TraceLog,String> resultColumn;
        resultColumn = new TableColumn<TraceLog,String>("Result");
        resultColumn.setCellValueFactory(new PropertyValueFactory("result"));
        configureColumnWidth("resultColumnWidth", resultColumn);

        tracesBox.getColumns().setAll(
            seqColumn, pidColumn, regNameColumn, durationNameColumn, functionNameColumn, argsColumn, resultColumn
        );

        // based on http://stackoverflow.com/questions/27015961/tableview-row-style
        PseudoClass exceptionClass = PseudoClass.getPseudoClass("exception");
        PseudoClass notCompletedClass = PseudoClass.getPseudoClass("not-completed");
        PseudoClass breakerRowClass = PseudoClass.getPseudoClass("breaker-row");
        tracesBox.setRowFactory(tv -> {
            TableRow<TraceLog> row = new TableRow<>();
            ChangeListener<Boolean> completeListener = (obs, oldComplete, newComplete) -> {
                row.pseudoClassStateChanged(exceptionClass, row.getItem().isExceptionThrower());
                row.pseudoClassStateChanged(notCompletedClass, !row.getItem().isComplete());
            };
            row.itemProperty().addListener((obs, oldTl, tl) -> {
                if (oldTl != null) {
                    oldTl.isCompleteProperty().removeListener(completeListener);
                }
                if (tl != null) {

                    row.pseudoClassStateChanged(notCompletedClass, !row.getItem().isComplete());
                    if("breaker-row".equals(tl.getCssClass())) {
                        row.pseudoClassStateChanged(breakerRowClass, true);

                        row.pseudoClassStateChanged(exceptionClass, false);
                        row.pseudoClassStateChanged(notCompletedClass, false);
                    }
                    else {
                        tl.isCompleteProperty().addListener(completeListener);
                        row.pseudoClassStateChanged(breakerRowClass, false);
                        row.pseudoClassStateChanged(exceptionClass, tl.isExceptionThrower());
                    }
                }
                else {
                    row.pseudoClassStateChanged(exceptionClass, false);
                    row.pseudoClassStateChanged(notCompletedClass, false);
                    row.pseudoClassStateChanged(breakerRowClass, false);
                }
            });
            return row ;
        });
    }

    private void configureColumnWidth(String widthProperty, TableColumn<TraceLog, ?> functionNameColumn) {
        functionNameColumn.setPrefWidth(PrefBind.getOrDefaultDouble(widthProperty, functionNameColumn.getPrefWidth()));
        functionNameColumn.widthProperty().addListener((o, ov, nv) -> {
            PrefBind.set(widthProperty, nv);
        });
    }

    private void putTraceContextMenu() {
        TraceContextMenu traceContextMenu = new TraceContextMenu();
        traceContextMenu.setItems(traceLogs);
        traceContextMenu
                .setSelectedItems(tracesBox.getSelectionModel().getSelectedItems());

        tracesBox.setContextMenu(traceContextMenu);
        tracesBox.selectionModelProperty().get().setSelectionMode(SelectionMode.MULTIPLE);
    }

    private void onTraceClicked(MouseEvent me) {
        if(me.getButton().equals(MouseButton.PRIMARY) && me.getClickCount() == 2) {
            TraceLog selectedItem = tracesBox.getSelectionModel().getSelectedItem();

            if(selectedItem != null && selectedItem != null) {
                showTraceTermView(selectedItem);
            }
        }
    }

    private TermTreeView newTermTreeView() {
        TermTreeView termTreeView;

        termTreeView = new TermTreeView();
        termTreeView.setMaxHeight(Integer.MAX_VALUE);
        VBox.setVgrow(termTreeView, Priority.ALWAYS);

        return termTreeView;
    }


    private void showTraceTermView(final TraceLog traceLog) {
        OtpErlangObject args = traceLog.getArgsList();
        OtpErlangObject result = traceLog.getResultFromMap();

        TermTreeView resultTermsTreeView, argTermsTreeView;

        resultTermsTreeView = newTermTreeView();

        if(result != null) {
            resultTermsTreeView.populateFromTerm(traceLog.getResultFromMap());
        }
        else {
            WeakChangeListener<Boolean> listener = new WeakChangeListener<Boolean>((o, oldV, newV) -> {
                if(newV)
                    resultTermsTreeView.populateFromTerm(traceLog.getResultFromMap());
            });

            traceLog.isCompleteProperty().addListener(listener);
        }

        argTermsTreeView = newTermTreeView();
        argTermsTreeView.populateFromListContents((OtpErlangList)args);

        SplitPane splitPane, splitPaneH;

        splitPane = new SplitPane();
        splitPane.setOrientation(Orientation.HORIZONTAL);
        splitPane.getItems().addAll(
            labelledTreeView("Function arguments", argTermsTreeView),
            labelledTreeView("Result", resultTermsTreeView)
        );

        StackTraceView stackTraceView;
        stackTraceView = new StackTraceView();
        stackTraceView.populateFromMfaList(traceLog.getStackTrace());
        String stackTraceTitle = "Stack Trace (" + traceLog.getStackTrace().arity() + ")";
        TitledPane titledPane = new TitledPane(stackTraceTitle, stackTraceView);
        titledPane.setExpanded(!stackTraceView.isStackTracesEmpty());
        splitPaneH = new SplitPane();
        splitPaneH.setOrientation(Orientation.VERTICAL);
        splitPaneH.getItems().addAll(splitPane, titledPane);

        StringBuilder sb = new StringBuilder(traceLog.getPidString());
        sb.append(" ");
        traceLog.appendModFuncArity(sb);

        ErlyBerly.showPane(sb.toString(), ErlyBerly.wrapInPane(splitPaneH));
    }

    private Node labelledTreeView(String label, TermTreeView node) {
        return new VBox(new Label(label), node);
    }

    private void onTraceFilterChange(String searchText) {
        BasicSearch basicSearch = new BasicSearch(searchText);
        filteredTraces.setPredicate((t) -> {
            String logText = t.toString();
            return basicSearch.matches(logText);
        });
    }

    private Region traceLogFloatySearchControl() {
        FxmlLoadable loader = new FxmlLoadable("/floatyfield/floaty-field.fxml");

        loader.load();

        FloatyFieldView ffView;

        ffView = (FloatyFieldView) loader.controller;
        ffView.promptTextProperty().set("Filter trace logs");

        HBox.setHgrow(loader.fxmlNode, Priority.ALWAYS);

        ffView.textProperty().addListener((o, ov, nv) -> { onTraceFilterChange(nv); });

        Region fxmlNode = (Region) loader.fxmlNode;
        fxmlNode.setPadding(new Insets(5, 5, 0, 5));
        Platform.runLater(() -> {
            FilterFocusManager.addFilter((Control) loader.fxmlNode.getChildrenUnmodifiable().get(1), 2);
        });

        return fxmlNode;
    }

    public void traceLogsChanged(ListChangeListener.Change<? extends TraceLog> e) {
        while(e.next()) {
            for (TraceLog trace : e.getAddedSubList()) {
                if(insertTracesAtTop)
                    traceLogs.add(0, trace);
                else
                    traceLogs.add(trace);
            }
        }
    }
}
