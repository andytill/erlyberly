package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.SortEvent;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

/**
 * Handles UI related tasks and delegates processing to {@link ProcController}. 
 */
public class ProcView implements Initializable {

	private final ProcController procController;
	
	@FXML
	private TableView<ProcInfo> processView;
	@FXML
	private TextField nodeNameField;
	@FXML
	private Button connectButton;

	public ProcView() {
		procController = new ProcController();
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void initialize(URL url, ResourceBundle r) {
		TableColumn<ProcInfo, String> procColumn = (TableColumn<ProcInfo, String>) processView.getColumns().get(0);
		TableColumn<ProcInfo, String> reducColumn = (TableColumn<ProcInfo, String>) processView.getColumns().get(1);

		procColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, String>("processName"));
		procColumn.setId("proc");
		
		reducColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, String>("reductions"));
		reducColumn.setId("reduc");
		
		processView.setItems(procController.getProcs());
		
		procController.remoteNodeNameProperty().bind(nodeNameField.textProperty());
		
		// the connect button is disabled when the name field is empty or we're connected
		connectButton.disableProperty().bind(nodeNameField.textProperty().isEmpty().or(procController.connectedProperty()));
		
		// the node name field is disabled when we're connected
		nodeNameField.disableProperty().bind(procController.connectedProperty());
		
		initialiseProcessSorting();
	}
	
	@FXML
	public void onConnect() {
		procController.connect();
	}
	
	@FXML
	public void onNodeNameKeyPressed(KeyEvent e) {
		// allow an enter key to connect
		if(e.getCode() == KeyCode.ENTER) {
			onConnect();
		}
	}

	private void initialiseProcessSorting() {
		InvalidationListener invalidationListener = new ProcSortUpdater();
		
		for (TableColumn<ProcInfo, ?> col : processView.getColumns()) {
			col.sortTypeProperty().addListener(invalidationListener);
		}
		
		processView.getSortOrder().addListener(invalidationListener);
		processView.setOnSort(new EventHandler<SortEvent<TableView<ProcInfo>>>() {
			@Override
			public void handle(SortEvent<TableView<ProcInfo>> e) {
				
			}});
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
