package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;
import floatyfield.FloatyFieldView;
import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.binding.BooleanBinding;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Orientation;
import javafx.scene.chart.PieChart;
import javafx.scene.chart.PieChart.Data;
import javafx.scene.control.Button;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Control;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.Separator;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.util.Callback;

/**
 * Handles UI related tasks and delegates processing to {@link ProcController}. 
 */
public class ProcView implements Initializable {

    private final ProcController procController;
    
    @FXML
    private TableView<ProcInfo> processView;
    @FXML
    private Button refreshButton;
    @FXML
    private Button pollButton;
    @FXML
    private Button heapPieButton;
    @FXML
    private Button stackPieButton;
    @FXML
    private Button totalHeapPieButton;
    @FXML
    private HBox headerBox;
    
    public ProcView() {
        procController = new ProcController();
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public void initialize(URL url, ResourceBundle r) {
        MenuItem menuItem;
        
        menuItem = new MenuItem("Get process state (R16B03 or higher)");
        menuItem.setOnAction(this::onShowProcessStateClicked);
        
        menuItem.disableProperty().bind(processView.getSelectionModel().selectedItemProperty().isNull());
        
        processView.setContextMenu(new ContextMenu(menuItem));
        
        // #23 when the context menu is showing, temporarily suspend polling, polling
        // loses selection making the right click context menu no longer enabled since
        // no process is selected
        processView
            .getContextMenu()
            .showingProperty()
            .addListener((o, oldv, newv) -> { procController.setTemporarilySuspendPolling(newv); });
        
        final BooleanBinding notConnected = ErlyBerly.nodeAPI().connectedProperty().not();
        
        ErlyBerly.nodeAPI().connectedProperty().addListener(this::onConnected);
        
        heapPieButton.setGraphic(Icon.create().icon(AwesomeIcon.PIE_CHART));
        heapPieButton.getStyleClass().add("erlyberly-icon-button");
        heapPieButton.setStyle("-fx-background-color: transparent;");
        heapPieButton.setText("");
        heapPieButton.disableProperty().bind(notConnected);
        
        stackPieButton.setGraphic(Icon.create().icon(AwesomeIcon.PIE_CHART));
        stackPieButton.setStyle("-fx-background-color: transparent;");
        stackPieButton.setText("");
        stackPieButton.disableProperty().bind(notConnected);
        
        totalHeapPieButton.setGraphic(Icon.create().icon(AwesomeIcon.PIE_CHART));
        totalHeapPieButton.setStyle("-fx-background-color: transparent;");
        totalHeapPieButton.setText("");
        totalHeapPieButton.disableProperty().bind(notConnected);
        
        refreshButton.setGraphic(Icon.create().icon(AwesomeIcon.ROTATE_LEFT));
        refreshButton.setGraphicTextGap(8d);
        refreshButton.disableProperty().bind(procController.pollingProperty().or(notConnected));
        
        pollButton.setGraphic(Icon.create().icon(AwesomeIcon.REFRESH));
        pollButton.setGraphicTextGap(9d);
        pollButton.disableProperty().bind(notConnected);

        procController.pollingProperty().addListener(this::onPollingChange);
        procController.setListComparator(processView.comparatorProperty());
        
        onPollingChange(null);
        
        TableColumn<ProcInfo, String> pidColumn = (TableColumn<ProcInfo, String>) processView.getColumns().get(0);
        TableColumn<ProcInfo, String> procColumn = (TableColumn<ProcInfo, String>) processView.getColumns().get(1);
        TableColumn<ProcInfo, Long> reducColumn = (TableColumn<ProcInfo, Long>) processView.getColumns().get(2);
        TableColumn<ProcInfo, Long> mQueueLenColumn = (TableColumn<ProcInfo, Long>) processView.getColumns().get(3);
        TableColumn<ProcInfo, Long> heapSizeColumn = (TableColumn<ProcInfo, Long>) processView.getColumns().get(4);
        TableColumn<ProcInfo, Long> stackSizeColumn = (TableColumn<ProcInfo, Long>) processView.getColumns().get(5);
        TableColumn<ProcInfo, Long> totalHeapSizeColumn = (TableColumn<ProcInfo, Long>) processView.getColumns().get(6);
        
        pidColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, String>("pid"));
        pidColumn.setId("pid");

        procColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, String>("processName"));
        procColumn.setId("proc");
        
        reducColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, Long>("reductions"));
        reducColumn.setId("reduc");
        
        mQueueLenColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, Long>("msgQueueLen"));
        mQueueLenColumn.setId("mqueue");
        
        heapSizeColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, Long>("heapSize"));
        heapSizeColumn.setId("heapsize");
        
        stackSizeColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, Long>("stackSize"));
        stackSizeColumn.setId("stacksize");
        
        totalHeapSizeColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, Long>("totalHeapSize"));
        totalHeapSizeColumn.setId("totalheapsize");
        
        processView.setItems(procController.getProcs());
        processView.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
        
        addFloatySearchControl();
        
        initialiseProcessSorting();
    }

    private FxmlLoadable addFloatySearchControl() {
        FxmlLoadable loader = new FxmlLoadable("/floatyfield/floaty-field.fxml");
        
        loader.load();

        FloatyFieldView ffView;
        
        ffView = (FloatyFieldView) loader.controller;
        ffView.promptTextProperty().set("Search on process pid and registered name");
        
        HBox.setHgrow(loader.fxmlNode, Priority.ALWAYS);
        
        headerBox.getChildren().add(0, new Separator(Orientation.VERTICAL));
        headerBox.getChildren().add(0, loader.fxmlNode);
        
        procController.filterProperty().bind(ffView.textProperty());
        
        Platform.runLater(() -> {
            FilterFocusManager.addFilter((Control) loader.fxmlNode.getChildrenUnmodifiable().get(1), 0);
        });
        
        return loader;
    }
    
    private void onShowProcessStateClicked(ActionEvent e) {
        ProcInfo proc = processView.getSelectionModel().getSelectedItem();
        
        if(proc == null)
            return;
        
        procController.processState(proc, (eobj) -> {showProcessStateInWindow(proc, eobj); });
    }
    
    private void showProcessStateInWindow(ProcInfo procInfo, OtpErlangObject obj) {
        if(obj == null)
            obj = new OtpErlangString("Error, erlyberly cannot get process state. Probably not OTP compliant process");
        
        TermTreeView termTreeView;
        
        termTreeView = new TermTreeView();
        termTreeView.setMaxHeight(Integer.MAX_VALUE);
        VBox.setVgrow(termTreeView, Priority.ALWAYS);
        termTreeView.populateFromTerm(obj); 

        ErlyBerly.showPane("Process State for " + procInfo.getShortName(), termTreeView);
    }
    
    @FXML
    private void onHeapPie() {
        ObservableList<PieChart.Data> data = buildData(chartableProcs(), (p) -> {return p.getHeapSize(); });
        
        showPieChart("Process Heap", data);
    }

    @FXML
    private void onStackPie() {
        ObservableList<PieChart.Data> data = buildData(chartableProcs(), (p) -> {return p.getStackSize(); });
            
        showPieChart("Process Stack", data);
    }

    @FXML
    private void onTotalHeapPie() {
        ObservableList<PieChart.Data> data = buildData(chartableProcs(), (p) -> {return p.getTotalHeapSize(); });
        
        showPieChart("Total Heap", data);
    }
    
    private ObservableList<PieChart.Data> buildData(ObservableList<ProcInfo> procs, Callback<ProcInfo, Long> extractor) {
        
        long total = 0;

        for (ProcInfo proc : procs) {
            total += extractor.call(proc);
        }
        
        // threshold is 1%, this is a limit on how many segments are added to the pie chart
        // too many seems to crash the process
        long threshold = total / 200;

        long other = 0;
        
        ObservableList<PieChart.Data> data = FXCollections.observableArrayList();

        for (ProcInfo proc : procs) {
            long value = extractor.call(proc);
            
            if(value >= threshold)
                data.add(new Data(procDescription(proc), proc.getTotalHeapSize()));
            else
                other += value;
        }
        
        if(other > 0)
            data.add(new Data("All processes > " + threshold + " bytes", other));
        
        return data;
    }

    private ObservableList<ProcInfo> chartableProcs() {
        ObservableList<ProcInfo> procs = processView.getSelectionModel().getSelectedItems();
        
        if(procs.isEmpty() || procs.size() == 1) {
            procs = procController.getProcs();
        }
        return procs;
    }

    private void showPieChart(String title, ObservableList<PieChart.Data> data) {
        PieChart pieChart;
        pieChart = new PieChart(data);
        pieChart.setTitle(title);
        ErlyBerly.showPane(title, pieChart);
    }

    private String procDescription(ProcInfo proc) {
        String pid = proc.getProcessName();
        if(pid == null || "".equals(pid)) {
            pid = proc.getPid();
        }
        return pid;
    }

    private void onPollingChange(Observable o) {
        if(procController.pollingProperty().get())
            pollButton.setText("Stop Polling");
        else
            pollButton.setText("Start Polling");
    }
    
    @FXML
    private void onRefresh() {
        procController.refreshOnce();
    }

    @FXML
    private void onTogglePolling() {
        procController.togglePolling();
    }
    
    private void onConnected(Observable o) {
        
        boolean connected = ErlyBerly.nodeAPI().connectedProperty().get();
        
        if(connected) {
            procController.refreshOnce();
        } else {
            procController.clearProcesses();
        }
    }
    
    private void initialiseProcessSorting() {
        InvalidationListener invalidationListener = new ProcSortUpdater();
        
        for (TableColumn<ProcInfo, ?> col : processView.getColumns()) {
            col.sortTypeProperty().addListener(invalidationListener);
        }
        
        processView.getSortOrder().addListener(invalidationListener);
    }
    
    private final class ProcSortUpdater implements InvalidationListener {
        @Override
        public void invalidated(Observable ob) {
            ProcSort procSort = null;
            
            if(!processView.getSortOrder().isEmpty()) {
                TableColumn<ProcInfo, ?> tableColumn = processView.getSortOrder().get(0);
                
                procSort = new ProcSort(tableColumn.getId(), tableColumn.getSortType());
            }
            procController.procSortProperty().set(procSort);
        }
    }
}
