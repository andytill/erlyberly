package erlyberly;

import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.Initializable;


public class DbgController implements Initializable {
	
	public final ObservableList<TraceLog> traceLogs = FXCollections.observableArrayList();
	
	private final ObservableList<ModFunc> traces = FXCollections.observableArrayList();

	private volatile boolean collectingTraces;

	public void setCollectingTraces(boolean collecting) {
		collectingTraces = collecting;
	}

	@Override
	public void initialize(URL url, ResourceBundle r) {
		new TraceCollectorThread().start();
	}
	
	public ObservableList<TraceLog> getTraceLogs() {
		return traceLogs;
	}

	public boolean isTraced(ModFunc function) {
		return traces.contains(function);
	}
	
	public void toggleTraceModFunc(ModFunc function) {
		if(traces.contains(function))
			onRemoveTracer(null, function);
		else
			traceModFunc(function);
	}

	private void traceModFunc(ModFunc function) {
		try {
			ErlyBerly.nodeAPI().startTrace(function);
			
			traces.add(function);
			
			setCollectingTraces(true);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	
	private void onRemoveTracer(ActionEvent e, ModFunc function) {
		try {
			ErlyBerly.nodeAPI().stopTrace(function);

			traces.remove(function);
		}
		catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	public void reapplyTraces() {
		for (ModFunc function : traces) {
			try {
				ErlyBerly.nodeAPI().startTrace(function);
			} catch (Exception e) {
				e.printStackTrace();
				
				traces.remove(function);
			}
		}
	}

	public void addTraceListener(InvalidationListener listener) {
		traces.addListener(listener);
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
								traceLogs.addAll(collectTraceLogs);
							}});
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
				
				try {
					Thread.sleep(100);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		}
	}
}
