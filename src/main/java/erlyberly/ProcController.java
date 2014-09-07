package erlyberly;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;

import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn.SortType;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

/**
 * Logic and processing for the entop control. 
 */
public class ProcController {
	
	private final SimpleObjectProperty<ProcSort> procSortProperty;
	
	private final SimpleStringProperty remoteNodeNameProperty;
	
	private final SimpleBooleanProperty connectedProperty;

	private OtpSelf self;
	
	private OtpConnection connection;
	
	private ProcPollerThread procPollerThread;
	
	public ProcController() {
		procSortProperty = new SimpleObjectProperty<>(new ProcSort("reduc", SortType.DESCENDING));
		
		remoteNodeNameProperty = new SimpleStringProperty();
		
		connectedProperty = new SimpleBooleanProperty();
		
		procPollerThread = new ProcPollerThread();
	}
	
	public void connect() {
		String nodeName = remoteNodeNameProperty.get();

		try {
			self = new OtpSelf("erlyberly-" + System.currentTimeMillis());
			
			// if the node name does not contain a host then assume it is on the
			// same machine
			if(nodeName.contains("@")) {
				String[] split = self.toString().split("\\@");
				
				nodeName += "@" + split[1];
			}
			
			connection = self.connect(new OtpPeer(nodeName));
			
			try {
				OtpUtil.loadRemoteErlyberly(connection);
			} catch (Exception e) {
				e.printStackTrace();
			}
			procPollerThread.start();
			
			connectedProperty.set(true);
		} catch (Exception e) {
			throw new RuntimeException("Error connecting to remote node " + nodeName, e);
		}
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
		return connectedProperty;
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
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e1) {
					e1.printStackTrace();
				}
				
		    	final ArrayList<ProcInfo> processes = new ArrayList<>();
		    	
				try {
					retrieveProcessInfo(processes);
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
			}
		}

		private void retrieveProcessInfo(ArrayList<ProcInfo> processes) throws Exception {
			connection.sendRPC("erlyberly", "process_info", new OtpErlangList());
			OtpErlangList received = (OtpErlangList) connection.receiveRPC(); 
			
			for (OtpErlangObject recv : received) {
				if(recv instanceof OtpErlangList) {
					OtpErlangList pinfo = (OtpErlangList) recv; 
					HashMap<Object, Object> propsToMap = OtpUtil.propsToMap(pinfo);
					processes.add(ProcInfo.toProcessInfo(propsToMap));
				}
			}
			
		}
	}
}
