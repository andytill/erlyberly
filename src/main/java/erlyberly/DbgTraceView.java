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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import erlyberly.node.OtpUtil;
import floatyfield.FloatyFieldView;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.css.PseudoClass;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;


public class DbgTraceView extends VBox {

    private final DbgController dbgController;

    private final FilteredList<TraceLog> filteredTraces;

    private final TableView<TraceLog> tracesBox;

    public DbgTraceView(final DbgController aDbgController) {
        super();
        this.dbgController = aDbgController;
        final SortedList<TraceLog> sortedTtraces = new SortedList<>(this.dbgController.getTraceLogs());
        this.filteredTraces = new FilteredList<>(sortedTtraces);

        this.setSpacing(5d);
        this.setStyle("-fx-background-insets: 5;");
        this.setMaxHeight(Double.MAX_VALUE);

        this.tracesBox = new TableView<>();
        this.tracesBox.getStyleClass().add("trace-log-table");
        this.tracesBox.setOnMouseClicked(this::onTraceClicked);
        this.tracesBox.setMaxHeight(Double.MAX_VALUE);
        VBox.setVgrow(this.tracesBox, Priority.ALWAYS);

        this.putTableColumns();

        // #47 double wrapping the filtered list in another sorted list, otherwise
        // the table cannot be sorted on columns. Binding the items will throw exceptions
        // when sorting on columns.
        // see http://code.makery.ch/blog/javafx-8-tableview-sorting-filtering/
        final SortedList<TraceLog> sortedData = new SortedList<>(this.filteredTraces);
        sortedData.comparatorProperty().bind(this.tracesBox.comparatorProperty());
        this.tracesBox.setItems(sortedData);

        this.putTraceContextMenu();

        final Parent p = this.traceLogFloatySearchControl();

        this.getChildren().addAll(p, this.tracesBox);
    }

    @SuppressWarnings("rawtypes")
    private void putTableColumns() {
        final TableColumn<TraceLog, Long> seqColumn;
        seqColumn = new TableColumn<>("Seq.");
        seqColumn.setCellValueFactory(new PropertyValueFactory("instanceNum"));
        DbgTraceView.configureColumnWidth("seqColumnWidth", seqColumn);

        final TableColumn<TraceLog, String> pidColumn;
        pidColumn = new TableColumn<>("Pid");
        pidColumn.setCellValueFactory(new PropertyValueFactory("pidString"));
        DbgTraceView.configureColumnWidth("pidColumnWidth", pidColumn);

        final TableColumn<TraceLog, String> regNameColumn;
        regNameColumn = new TableColumn<>("Reg. Name");
        regNameColumn.setCellValueFactory(new PropertyValueFactory("regName"));
        DbgTraceView.configureColumnWidth("regNameColumnWidth", regNameColumn);

        final TableColumn<TraceLog, String> durationNameColumn;
        durationNameColumn = new TableColumn<>("Duration (microseconds)");
        durationNameColumn.setCellValueFactory(new PropertyValueFactory("duration"));
        DbgTraceView.configureColumnWidth("durationNameColumnWidth", durationNameColumn);

        final TableColumn<TraceLog, String> functionNameColumn;
        functionNameColumn = new TableColumn<>("Function");
        functionNameColumn.setCellValueFactory(new PropertyValueFactory("function"));
        DbgTraceView.configureColumnWidth("functionNameColumnWidth", functionNameColumn);

        final TableColumn<TraceLog, String> argsColumn;
        argsColumn = new TableColumn<>("Args");
        argsColumn.setCellValueFactory(new PropertyValueFactory("args"));
        DbgTraceView.configureColumnWidth("argsColumnWidth", argsColumn);

        final TableColumn<TraceLog, String> resultColumn;
        resultColumn = new TableColumn<>("Result");
        resultColumn.setCellValueFactory(new PropertyValueFactory("result"));
        DbgTraceView.configureColumnWidth("resultColumnWidth", resultColumn);

        this.tracesBox.getColumns().setAll(seqColumn, pidColumn, regNameColumn, durationNameColumn, functionNameColumn, argsColumn, resultColumn);

        // based on http://stackoverflow.com/questions/27015961/tableview-row-style
        final PseudoClass exceptionClass = PseudoClass.getPseudoClass("exception");
        final PseudoClass notCompletedClass = PseudoClass.getPseudoClass("not-completed");
        final PseudoClass breakerRowClass = PseudoClass.getPseudoClass("breaker-row");
        this.tracesBox.setRowFactory(tv -> {
            final TableRow<TraceLog> row = new TableRow<>();
            final ChangeListener<Boolean> completeListener = (obs, oldComplete, newComplete) -> {
                row.pseudoClassStateChanged(exceptionClass, row.getItem().isExceptionThrower());
                row.pseudoClassStateChanged(notCompletedClass, row.getItem().isComplete());
            };
            row.itemProperty().addListener((obs, oldTl, tl) -> {
                if (null != oldTl) {
                    oldTl.isCompleteProperty().removeListener(completeListener);
                }
                if (null != tl) {

                    row.pseudoClassStateChanged(notCompletedClass, row.getItem().isComplete());
                    if ("breaker-row".equals(tl.getCssClass())) {
                        row.pseudoClassStateChanged(breakerRowClass, true);

                        row.pseudoClassStateChanged(exceptionClass, false);
                        row.pseudoClassStateChanged(notCompletedClass, false);
                    } else {
                        tl.isCompleteProperty().addListener(completeListener);
                        row.pseudoClassStateChanged(breakerRowClass, false);
                        row.pseudoClassStateChanged(exceptionClass, tl.isExceptionThrower());
                    }
                } else {
                    row.pseudoClassStateChanged(exceptionClass, false);
                    row.pseudoClassStateChanged(notCompletedClass, false);
                    row.pseudoClassStateChanged(breakerRowClass, false);
                }
            });
            return row;
        });
    }

    private static void configureColumnWidth(final String widthProperty, final TableColumn<TraceLog, ?> functionNameColumn) {
        functionNameColumn.setPrefWidth(PrefBind.getOrDefaultDouble(widthProperty, functionNameColumn.getPrefWidth()));
        functionNameColumn.widthProperty().addListener((o, ov, nv) -> PrefBind.set(widthProperty, nv));
    }

    private void putTraceContextMenu() {
        final TraceContextMenu traceContextMenu = new TraceContextMenu(this.dbgController);
        traceContextMenu.setItems(this.dbgController.getTraceLogs());
        traceContextMenu.setSelectedItems(this.tracesBox.getSelectionModel().getSelectedItems());

        this.tracesBox.setContextMenu(traceContextMenu);
        this.tracesBox.selectionModelProperty().get().setSelectionMode(SelectionMode.MULTIPLE);
    }

    private void onTraceClicked(final MouseEvent me) {
        if (MouseButton.PRIMARY == me.getButton() && 2 == me.getClickCount()) {
            final TraceLog selectedItem = this.tracesBox.getSelectionModel().getSelectedItem();

            if (null != selectedItem) {
                DbgTraceView.showTraceTermView(selectedItem);
            }
        }
    }

    private static TermTreeView newTermTreeView() {
        final TermTreeView termTreeView;

        termTreeView = new TermTreeView();
        termTreeView.setMaxHeight(Integer.MAX_VALUE);
        VBox.setVgrow(termTreeView, Priority.ALWAYS);

        return termTreeView;
    }


    private static void showTraceTermView(final TraceLog traceLog) {
        final OtpErlangList args = traceLog.getArgsList();
        final OtpErlangObject result = traceLog.getResultFromMap();

        final TermTreeView resultTermsTreeView;
        final TermTreeView argTermsTreeView;

        resultTermsTreeView = DbgTraceView.newTermTreeView();

        final OtpErlangAtom moduleName = OtpUtil.atom(traceLog.getModFunc().getModuleName());

        if (null != result) {
            resultTermsTreeView.populateFromTerm(moduleName, traceLog.getResultFromMap());
        } else {
            final WeakChangeListener<Boolean> listener = new WeakChangeListener<>((o, oldV, newV) -> {
                if (newV.booleanValue()) resultTermsTreeView.populateFromTerm(traceLog.getResultFromMap());
            });

            traceLog.isCompleteProperty().addListener(listener);
        }

        argTermsTreeView = DbgTraceView.newTermTreeView();
        argTermsTreeView.populateFromListContents(moduleName, args);

        final SplitPane splitPane;
        final SplitPane splitPaneH;

        splitPane = new SplitPane();
        splitPane.setOrientation(Orientation.HORIZONTAL);
        splitPane.getItems().addAll(DbgTraceView.labelledTreeView("Function arguments", argTermsTreeView), DbgTraceView.labelledTreeView("Result", resultTermsTreeView));

        final StackTraceView stackTraceView;
        stackTraceView = new StackTraceView();
        stackTraceView.populateFromMfaList(traceLog.getStackTrace());
        final String stackTraceTitle = "Stack Trace (" + traceLog.getStackTrace().arity() + ")";
        final TitledPane titledPane = new TitledPane(stackTraceTitle, stackTraceView);
        titledPane.setExpanded(!stackTraceView.isStackTracesEmpty());
        splitPaneH = new SplitPane();
        splitPaneH.setOrientation(Orientation.VERTICAL);
        splitPaneH.getItems().addAll(splitPane, titledPane);

        final StringBuilder sb = new StringBuilder(traceLog.getPidString());
        sb.append(" ");
        traceLog.appendModFuncArity(sb);

        ErlyBerly.showPane(sb.toString(), ErlyBerly.wrapInPane(splitPaneH));
    }

    private static Node labelledTreeView(final String label, final TermTreeView node) {
        return new VBox(new Label(label), node);
    }

    private void onTraceFilterChange(final String searchText) {
        final BasicSearch search = new BasicSearch(searchText);
        this.filteredTraces.setPredicate((t) -> search.matches(t.getArgs()) || search.matches(t.getResult()) || search.matches(t.getPidString()) || search.matches(t.getRegName()) || search.matches(t.getFunction()));
    }

    private Region traceLogFloatySearchControl() {
        final FxmlLoadable loader = new FxmlLoadable("/floatyfield/floaty-field.fxml");

        loader.load();

        final FloatyFieldView ffView;

        ffView = (FloatyFieldView) loader.controller;
        ffView.promptTextProperty().set("Filter trace logs");

        HBox.setHgrow(loader.fxmlNode, Priority.ALWAYS);

        ffView.textProperty().addListener((o, ov, nv) -> this.onTraceFilterChange(nv));

        final Region fxmlNode = (Region) loader.fxmlNode;
        fxmlNode.setPadding(new Insets(5, 5, 0, 5));
        Platform.runLater(() -> FilterFocusManager.addFilter((Control) loader.fxmlNode.getChildrenUnmodifiable().get(1), 2));

        return fxmlNode;
    }
}
