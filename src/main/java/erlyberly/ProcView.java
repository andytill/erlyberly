package erlyberly;

import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.ResourceBundle;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.chart.PieChart;
import javafx.scene.chart.PieChart.Data;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.stage.Stage;
import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;

/**
 * Handles UI related tasks and delegates processing to {@link ProcController}. 
 */
public class ProcView implements Initializable {

	private final ProcController procController;

	private final DateFormat timeFormat;
	
	@FXML
	private TableView<ProcInfo> processView;
	@FXML
	private Label procCountLabel;
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
	
	public ProcView() {
		procController = new ProcController();
	
		timeFormat = new SimpleDateFormat("h:mm:ssaa");
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void initialize(URL url, ResourceBundle r) {
		
		procController.getProcs().addListener(this::onProcessCountChange);
		
		heapPieButton.setGraphic(Icon.create().icon(AwesomeIcon.PIE_CHART));
		heapPieButton.setStyle("-fx-background-color: transparent;");
		heapPieButton.setText("");
		
		stackPieButton.setGraphic(Icon.create().icon(AwesomeIcon.PIE_CHART));
		stackPieButton.setStyle("-fx-background-color: transparent;");
		stackPieButton.setText("");
		
		totalHeapPieButton.setGraphic(Icon.create().icon(AwesomeIcon.PIE_CHART));
		totalHeapPieButton.setStyle("-fx-background-color: transparent;");
		totalHeapPieButton.setText("");

		refreshButton.setGraphic(Icon.create().icon(AwesomeIcon.ROTATE_LEFT));
		refreshButton.setGraphicTextGap(8d);
		refreshButton.disableProperty().bind(procController.pollingProperty());
		
		pollButton.setGraphic(Icon.create().icon(AwesomeIcon.REFRESH));
		pollButton.setGraphicTextGap(9d);

		procController.pollingProperty().addListener(this::onPollingChange);
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
		
		initialiseProcessSorting();
	}
	
	@FXML
	private void onHeapPie() {
		ObservableList<PieChart.Data> data = FXCollections.observableArrayList();
		for (ProcInfo proc : chartableProcs()) {
			String pid = procDescription(proc);
			data.add(new Data(pid, proc.getHeapSize()));
		}
		
		showPieChart("Process Heap", data);
	}

	@FXML
	private void onStackPie() {
		ObservableList<PieChart.Data> data = FXCollections.observableArrayList();
		for (ProcInfo proc : chartableProcs()) {
			data.add(new Data(procDescription(proc), proc.getStackSize()));
		}
		
		showPieChart("Process Stack", data);
	}

	@FXML
	private void onTotalHeapPie() {
		ObservableList<PieChart.Data> data = FXCollections.observableArrayList();
		for (ProcInfo proc : chartableProcs()) {
			data.add(new Data(procDescription(proc), proc.getTotalHeapSize()));
		}
		
		showPieChart("Total Heap", data);
	}

	private ObservableList<ProcInfo> chartableProcs() {
		ObservableList<ProcInfo> procs = processView.getSelectionModel().getSelectedItems();
		
		if(procs.isEmpty()) {
			procs = procController.getProcs();
		}
		return procs;
	}

	private void showPieChart(String title, ObservableList<PieChart.Data> data) {
        PieChart pieChart;
        
		pieChart = new PieChart(data);
        pieChart.setTitle(title);
        
		Stage pieStage;
    
		pieStage = new Stage();
		pieStage.setScene(new Scene(pieChart));
        pieStage.setWidth(800);
        pieStage.setHeight(600);

        pieStage.show();
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
	
	private void onProcessCountChange(Observable o) {
		procCountLabel.setText(procController.getProcs().size() + " processes at " + timeFormat.format(new Date()).toLowerCase());
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
