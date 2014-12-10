package erlyberly;

import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.Initializable;


public class DbgController implements Initializable {
	
	public final ObservableList<TraceLog> traces = FXCollections.observableArrayList();

	private volatile boolean collectingTraces;

	public void setCollectingTraces(boolean collecting) {
		collectingTraces = collecting;
	}

	@Override
	public void initialize(URL url, ResourceBundle r) {
		new TraceCollectorThread().start();
		
		
	}
	
	public ObservableList<TraceLog> getTraceLogs() {
		return traces;
	}

	class TraceCollectorThread extends Thread {
		public TraceCollectorThread() {
			setDaemon(true);
		}
		
		@Override
		public void run() {
			while (true) {
				if(collectingTraces) {
					try {
						final ArrayList<TraceLog> collectTraceLogs = ErlyBerly.nodeAPI().collectTraceLogs();
						Platform.runLater(new Runnable() {
							@Override
							public void run() {
								traces.addAll(collectTraceLogs);
							}});
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
				
				try {
					Thread.sleep(500);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		}
	}
}
