package erlyberly;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.scene.control.TableColumn.SortType;

/**
 * Logic and processing for the entop control. 
 */
public class ProcController {
	
	private final SimpleBooleanProperty polling;
	
	private final SimpleObjectProperty<ProcSort> procSortProperty;	
	
	private ProcPollerThread procPollerThread;

	private final Object waiter;

	private int timeout = -1;
	
	private final SimpleStringProperty filter = new SimpleStringProperty();
	
	private final ObservableList<ProcInfo> processes2 = FXCollections.observableArrayList();
	
	private final FilteredList<ProcInfo> filteredProcesses2 = new FilteredList<ProcInfo>(processes2);
	
	public ProcController() {
		polling = new SimpleBooleanProperty();
		
		procSortProperty = new SimpleObjectProperty<>(new ProcSort("reduc", SortType.DESCENDING));
		
		procPollerThread = new ProcPollerThread();
		
		waiter = new Object();
		
		Platform.runLater(() -> {
			ErlyBerly.nodeAPI().connectedProperty().addListener((o) -> { startPollingThread(); } );
		});
		
		filter.addListener((o, ov, nv) -> { updateProcFilter(nv); });
	}
	
	private void updateProcFilter(String filterText) {
		filteredProcesses2.setPredicate((proc) -> { return isMatchingProcess(filterText, proc); });
	}

	private boolean isMatchingProcess(String filterText, ProcInfo proc) {
		return 
				"".equals(filterText)
				|| proc.getPid().contains(filterText)
				|| (proc.getProcessName() != null && proc.getProcessName().contains(filterText));
	}

	private void startPollingThread() {
		if(ErlyBerly.nodeAPI().connectedProperty().get() && !procPollerThread.isAlive()) {
			procPollerThread.start();
		}
	}

	public SimpleStringProperty filterProperty() {
		return filter;
	}

	public void refreshOnce() {
		synchronized (waiter) {
			waiter.notify();
		}
	}

	public void togglePolling() {
		if(timeout == -1) {
			timeout = 1000;
			
			refreshOnce();
		}
		else {
			timeout = -1;
		}
		polling.set(timeout > 0);
	}

	public ObservableList<ProcInfo> getProcs() {
		return filteredProcesses2;
	}
	
	public SimpleBooleanProperty pollingProperty() {
		return polling;
	}
	
	public SimpleObjectProperty<ProcSort> procSortProperty() {
		return procSortProperty;
	}

	public SimpleBooleanProperty connectedProperty() {
		return ErlyBerly.nodeAPI().connectedProperty();
	}

	private final class ProcPollerThread extends Thread {
		public ProcPollerThread() {
			// make sure we don't hang the VM on close because of this thread
			setDaemon(true);
			setName("Process Info Poller");
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
					}});

				try {
					synchronized (waiter) {
						if(timeout > 0)
							waiter.wait(timeout);
						else
							waiter.wait();
					}
				} catch (InterruptedException e1) {
					e1.printStackTrace();
				}
			}
		}
	}
}
