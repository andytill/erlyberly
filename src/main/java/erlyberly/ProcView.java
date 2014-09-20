package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;

/**
 * Handles UI related tasks and delegates processing to {@link ProcController}. 
 */
public class ProcView implements Initializable {

	private final ProcController procController;
	
	@FXML
	private TableView<ProcInfo> processView;

	public ProcView() {
		procController = new ProcController();
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void initialize(URL url, ResourceBundle r) {
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
		
		initialiseProcessSorting();
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
