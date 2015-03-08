package erlyberly;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.scene.control.TableColumn.SortType;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;

import erlyberly.node.NodeAPI.RpcCallback;

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
	
	private final ObservableList<ProcInfo> processes = FXCollections.observableArrayList();
	
	private final FilteredList<ProcInfo> filteredProcesses = new FilteredList<ProcInfo>(processes);
	
	private final SortedList<ProcInfo> sortedProcesses = new SortedList<ProcInfo>(filteredProcesses);
	
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
	
	public void setListComparator(ObservableValue<? extends Comparator<? super ProcInfo>> tableComparator) {
		sortedProcesses.comparatorProperty().bind(tableComparator);
	}
	
	private void updateProcFilter(String filterText) {
		BasicSearch basicSearch = new BasicSearch(filterText);
		filteredProcesses.setPredicate((proc) -> { return isMatchingProcess(basicSearch, proc); });
	}

	private boolean isMatchingProcess(BasicSearch basicSearch, ProcInfo proc) {
		return 
				basicSearch.matches(proc.getPid(), proc.getProcessName());
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
		return sortedProcesses;
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
		    	final ArrayList<ProcInfo> processList = new ArrayList<>();
		    	
				try {
					ErlyBerly.nodeAPI().retrieveProcessInfo(processList);
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
								Collections.sort(processList, comparator);
							}
						}
						processes.clear();
						processes.addAll(processList);
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

	public void processState(ProcInfo proc, RpcCallback<OtpErlangObject> callback) {
		new ProcessStateThread(proc.getPid(), callback).start();
	}
	
	class ProcessStateThread extends Thread {
		
		private final String pidString;
		private final RpcCallback<OtpErlangObject> callback;

		public ProcessStateThread(String aPidString, RpcCallback<OtpErlangObject> aCallback) {
			pidString = aPidString;
			callback = aCallback;
			
			setDaemon(true);
			setName("Erlyberly Get Process State");
		}
		
		@Override
		public void run() {
			try {
				OtpErlangObject processState = ErlyBerly.nodeAPI().getProcessState(pidString);
				
				Platform.runLater(() -> { callback.callback(processState); });
			} 
			catch (OtpErlangException | IOException e) {
				e.printStackTrace();
			}
		}
	}
}
