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
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.Control;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
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
        
        insertTracesAtTop = PrefBind.getOrDefault("insertTracesAtTop", "false").equals("true");
        
        tracesBox = new TableView<TraceLog>();
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
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    private void putTableColumns() {
        TableColumn<TraceLog,Long> seqColumn;
        seqColumn = new TableColumn<TraceLog,Long>("Seq.");
        seqColumn.setCellValueFactory(new PropertyValueFactory("instanceNum"));
        
        TableColumn<TraceLog,String> pidColumn;
        pidColumn = new TableColumn<TraceLog,String>("Pid");
        pidColumn.setCellValueFactory(new PropertyValueFactory("pid"));

        TableColumn<TraceLog,String> regNameColumn;
        regNameColumn = new TableColumn<TraceLog,String>("Reg. Name");
        regNameColumn.setCellValueFactory(new PropertyValueFactory("regName"));

        TableColumn<TraceLog,String> durationNameColumn;
        durationNameColumn = new TableColumn<TraceLog,String>("Duration");
        durationNameColumn.setCellValueFactory(new PropertyValueFactory("duration"));

        TableColumn<TraceLog,String> functionnNameColumn;
        functionnNameColumn = new TableColumn<TraceLog,String>("Function");
        functionnNameColumn.setCellValueFactory(new PropertyValueFactory("function"));

        TableColumn<TraceLog,String> argsColumn;
        argsColumn = new TableColumn<TraceLog,String>("Args");
        argsColumn.setCellValueFactory(new PropertyValueFactory("args"));
        
        TableColumn<TraceLog,String> resultColumn;
        resultColumn = new TableColumn<TraceLog,String>("Result");
        resultColumn.setCellValueFactory(new PropertyValueFactory("result"));
        
        tracesBox.getColumns().setAll(
            seqColumn, pidColumn, regNameColumn, durationNameColumn, functionnNameColumn, argsColumn, resultColumn
        );

        // based on http://stackoverflow.com/questions/27015961/tableview-row-style
        PseudoClass exceptionClass = PseudoClass.getPseudoClass("exception");
        PseudoClass notCompletedClass = PseudoClass.getPseudoClass("not-completed");
        tracesBox.setRowFactory(tv -> {
            TableRow<TraceLog> row = new TableRow<>();
            row.itemProperty().addListener((obs, oldTl, tl) -> {
                if (tl != null) {
                    row.pseudoClassStateChanged(exceptionClass, tl.isExceptionThrower());
                    row.pseudoClassStateChanged(notCompletedClass, !tl.isComplete());
                }
                else {
                    row.pseudoClassStateChanged(exceptionClass, false);
                    row.pseudoClassStateChanged(notCompletedClass, false);
                }
            });
            return row ;
        });
        
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
                    tl.isCompleteProperty().addListener(completeListener);
                    row.pseudoClassStateChanged(exceptionClass, tl.isExceptionThrower());
                    row.pseudoClassStateChanged(notCompletedClass, !tl.isComplete());
                }
                else {
                    row.pseudoClassStateChanged(exceptionClass, false);
                    row.pseudoClassStateChanged(notCompletedClass, false);
                }
            });
            return row ;
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
        
        SplitPane splitPane;
        
        splitPane = new SplitPane();
        splitPane.getItems().addAll(
            labelledTreeView("Function arguments", argTermsTreeView), 
            labelledTreeView("Result", resultTermsTreeView)
        );
        
        StringBuilder sb = new StringBuilder(traceLog.getPidString());
        sb.append(" ");
        traceLog.appendModFuncArity(sb);
        
        ErlyBerly.showPane(sb.toString(), ErlyBerly.wrapInPane(splitPane));
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
        ffView.promptTextProperty().set("Search trace logs");

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
