package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;

public class ProcView implements Initializable {
	@FXML
	private TableView<ProcInfo> processView;
	
	private final ProcController procController;

	public ProcView() {
		procController = new ProcController();
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void initialize(URL url, ResourceBundle r) {
		TableColumn<ProcInfo, String> procColumn = (TableColumn<ProcInfo, String>) processView.getColumns().get(0);
		TableColumn<ProcInfo, String> reducColumn = (TableColumn<ProcInfo, String>) processView.getColumns().get(1);

		procColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, String>("processName"));
		reducColumn.setCellValueFactory(new PropertyValueFactory<ProcInfo, String>("reductions"));
		
		procController.connect();
		
		processView.setItems(procController.getProces());
	}
}
