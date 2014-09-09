package erlyberly;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn.SortType;

/**
 * Logic and processing for the entop control. 
 */
public class ProcController {
	
	private final SimpleObjectProperty<ProcSort> procSortProperty;
	
	private final SimpleStringProperty remoteNodeNameProperty;
	
	
	private ProcPollerThread procPollerThread;
	
	public ProcController() {
		procSortProperty = new SimpleObjectProperty<>(new ProcSort("reduc", SortType.DESCENDING));
		
		remoteNodeNameProperty = new SimpleStringProperty();
		
		procPollerThread = new ProcPollerThread();
		
		ErlyBerly.nodeAPI().connectedProperty().addListener(new InvalidationListener() {
			@Override
			public void invalidated(Observable o) {
				if(ErlyBerly.nodeAPI().connectedProperty().get()) {
					procPollerThread.start();
				}
			}});
	}
	
	public void connect() {
		ErlyBerly.nodeAPI().connect(remoteNodeNameProperty.get());
	}

	public ObservableList<ProcInfo> getProcs() {
		return procPollerThread.processes2;
	}
	
	public SimpleObjectProperty<ProcSort> procSortProperty() {
		return procSortProperty;
	}

	public SimpleStringProperty remoteNodeNameProperty() {
		return remoteNodeNameProperty;
	}

	public SimpleBooleanProperty connectedProperty() {
		return ErlyBerly.nodeAPI().connectedProperty();
	}

	private final class ProcPollerThread extends Thread {
		public ObservableList<ProcInfo> processes2 = FXCollections.observableArrayList();

		public SimpleLongProperty updateCounter = new SimpleLongProperty();
		
		public ProcPollerThread() {
			// make sure we don't hang the VM on close because of this thread
			setDaemon(true);
		}
		
		@Override
		public void run() {
			while(true) {				
		    	final ArrayList<ProcInfo> processes = new ArrayList<>();
		    	
				try {
					ErlyBerly.nodeAPI().retrieveProcessInfo(processes);
				} catch (Exception e) {
					e.printStackTrace();
				}
				
				Platform.runLater(new Runnable() {
					@Override
					public void run() {
						ProcSort procSort = procSortProperty.get();
						if(procSort != null) {
							Comparator<ProcInfo> comparator = null;
							
							if("proc".equals(procSort.getSortField())) {
								comparator = new Comparator<ProcInfo>() {
									@Override
									public int compare(ProcInfo o1, ProcInfo o2) {
										return o1.getProcessName().compareTo(o2.getProcessName());
									}};
							}
							else if("reduc".equals(procSort.getSortField())) {
								comparator = new Comparator<ProcInfo>() {
									@Override
									public int compare(ProcInfo o1, ProcInfo o2) {
										return Long.compare(o1.getReductions(), o2.getReductions());
									}};
							}
							
							if(comparator != null) {
								if(procSort.getSortType() == SortType.DESCENDING) {
									comparator = Collections.reverseOrder(comparator);
								}
								Collections.sort(processes, comparator);
							}
						}
						processes2.clear();
						processes2.addAll(processes);
						
						updateCounter.add(1);
					}});


				try {
					Thread.sleep(1000);
				} catch (InterruptedException e1) {
					e1.printStackTrace();
				}
			}
		}
	}
}
